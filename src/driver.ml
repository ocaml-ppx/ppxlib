(*$ open Ppxlib_cinaps_helpers $*)
open Import
open Utils
open Common
open With_errors
module Arg = Stdlib.Arg

let exe_name = Stdlib.Filename.basename Stdlib.Sys.executable_name
let args = ref []
let add_arg key spec ~doc = args := (key, spec, doc) :: !args
let loc_fname = ref None
let perform_checks = ref Options.perform_checks
let perform_checks_on_extensions = ref Options.perform_checks_on_extensions
let perform_locations_check = ref Options.perform_locations_check
let debug_attribute_drop = ref false
let apply_list = ref None
let preprocessor = ref None
let no_merge = ref false
let request_print_passes = ref false
let request_print_transformations = ref false
let use_color = ref true
let diff_command = ref Options.diff_command
let pretty = ref false
let styler = ref None
let output_metadata_filename = ref None
let corrected_suffix = ref ".ppx-corrected"

let ghost =
  object
    inherit Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
  end

let chop_prefix ~prefix x =
  if String.is_prefix ~prefix x then
    Some (String.drop_prefix x (String.length prefix))
  else None

let get_default_path (loc : Location.t) =
  let fname = loc.loc_start.pos_fname in
  match chop_prefix ~prefix:"./" fname with
  | Some fname -> fname
  | None -> fname

let get_default_path_str : structure -> string = function
  | [] -> ""
  | { pstr_loc = loc; _ } :: _ -> get_default_path loc

let get_default_path_sig : signature -> string = function
  | [] -> ""
  | { psig_loc = loc; _ } :: _ -> get_default_path loc

module Lint_error = struct
  type t = Location.t * string

  let of_string loc s = (loc, s)
end

module Cookies = struct
  type t = T

  let given_through_cli = ref []

  let get T name pattern =
    Option.map (Astlib.Ast_metadata.get_cookie name) ~f:(fun e ->
        let e = Selected_ast.of_ocaml Expression e in
        Ast_pattern.parse pattern e.pexp_loc e Fn.id)

  let get_res T name pattern =
    match
      Option.map (Astlib.Ast_metadata.get_cookie name) ~f:(fun e ->
          let e = Selected_ast.of_ocaml Expression e in
          Ast_pattern.parse_res pattern e.pexp_loc e Fn.id)
    with
    | None -> Ok None
    | Some (Ok e) -> Ok (Some e)
    | Some (Error e) -> Error e

  let set T name expr =
    Astlib.Ast_metadata.set_cookie name (Selected_ast.to_ocaml Expression expr)

  let handlers = ref []
  let add_handler f = handlers := !handlers @ [ f ]

  let add_simple_handler name pattern ~f =
    add_handler (fun T -> f (get T name pattern))

  let acknowledge_cookies T = List.iter !handlers ~f:(fun f -> f T)
  let post_handlers = ref []
  let add_post_handler f = post_handlers := !post_handlers @ [ f ]
  let call_post_handlers T = List.iter !post_handlers ~f:(fun f -> f T)
end

module Instrument = struct
  type pos = Before | After

  type t = {
    transformation :
      Expansion_context.Base.t ->
      Parsetree.structure ->
      Parsetree.structure With_errors.t;
    position : pos;
  }

  module V2 = struct
    let make transformation ~position =
      let transformation ctx st = return (transformation ctx st) in
      { transformation; position }
  end

  let make transformation ~position =
    let transformation _ st = transformation st in
    V2.make transformation ~position
end

module Transform = struct
  (* A full rewrite of implementation OCaml code i.e. .ml files *)
  type 'result full_impl_ast_rewrite_pass =
    Expansion_context.Base.t -> Parsetree.structure -> 'result

  (* A full rewrite of interface OCaml code i.e. .mli files *)
  type 'result full_intf_ast_rewrite_pass =
    Expansion_context.Base.t -> Parsetree.signature -> 'result

  type 'result enclosing_header_footer =
    Expansion_context.Base.t -> Location.t option -> 'result * 'result

  (* Meta data about the transformation like its name. *)
  type meta = {
    name : string;
    aliases : string list;
    registered_at : Caller_id.t;
  }

  (* A Transform.t represents a raw transformation as registered
     by a PPX to be applied to the AST. Most likely only a single
     optional field will be filled with a corresponding transformation,
     the rest will be [None]. *)
  type t = {
    meta : meta;
    impl : Parsetree.structure With_errors.t full_impl_ast_rewrite_pass option;
    intf : Parsetree.signature With_errors.t full_intf_ast_rewrite_pass option;
    lint_impl : Lint_error.t list full_impl_ast_rewrite_pass option;
    lint_intf : Lint_error.t list full_intf_ast_rewrite_pass option;
    preprocess_impl :
      Parsetree.structure With_errors.t full_impl_ast_rewrite_pass option;
    preprocess_intf :
      Parsetree.signature With_errors.t full_intf_ast_rewrite_pass option;
    enclose_impl : Parsetree.structure enclosing_header_footer option;
    enclose_intf : Parsetree.signature enclosing_header_footer option;
    instrument : Instrument.t option;
    rules : Context_free.Rule.t list;
  }

  let has_name t name =
    String.equal name t.meta.name
    || List.exists ~f:(String.equal name) t.meta.aliases

  let all : t list ref = ref []

  let print_caller_id oc (caller_id : Caller_id.t) =
    match caller_id with
    | None -> output_string oc "<unknown location>"
    | Some loc -> Printf.fprintf oc "%s:%d" loc.filename loc.line_number

  let register ?(extensions = []) ?(rules = []) ?enclose_impl ?enclose_intf
      ?impl ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf
      ?instrument ?(aliases = []) name =
    let rules = List.map extensions ~f:Context_free.Rule.extension @ rules in
    let caller_id = Caller_id.get ~skip:[ Stdlib.__FILE__ ] in
    (match List.filter !all ~f:(fun ct -> has_name ct name) with
    | [] -> ()
    | ct :: _ ->
        Printf.eprintf "Warning: code transformation %s registered twice.\n"
          name;
        Printf.eprintf "  - first time was at %a\n" print_caller_id
          ct.meta.registered_at;
        Printf.eprintf "  - second time is at %a\n" print_caller_id caller_id);
    let impl = Option.map impl ~f:(fun f ctx ast -> return (f ctx ast)) in
    let intf = Option.map intf ~f:(fun f ctx ast -> return (f ctx ast)) in
    let preprocess_impl =
      Option.map preprocess_impl ~f:(fun f ctx ast -> return (f ctx ast))
    in
    let preprocess_intf =
      Option.map preprocess_intf ~f:(fun f ctx ast -> return (f ctx ast))
    in
    let meta = { name; aliases; registered_at = caller_id } in
    let ct =
      {
        meta;
        rules;
        enclose_impl;
        enclose_intf;
        impl;
        intf;
        lint_impl;
        preprocess_impl;
        preprocess_intf;
        lint_intf;
        instrument;
      }
    in
    all := ct :: !all

  (* An individual pass is of a particular kind (e.g. lint, preprocess etc.)
     and may be for the implementation, the interface or both *)
  type ('impl, 'intf) pass = {
    meta : meta;
    pass : [ `Impl of 'impl | `Intf of 'intf | `Both of 'impl * 'intf ];
  }

  let get_pass_impl = function
    | { pass = `Impl i | `Both (i, _); _ } -> Some i
    | _ -> None

  let get_pass_intf = function
    | { pass = `Intf i | `Both (_, i); _ } -> Some i
    | _ -> None

  (* We distinguish between different kinds of transformations:
      - Linters: Are transformations applied to unprocessed files.
      - Preprocessors: The main preprocessing transformations.
      - Generic: Essentially any transformation that is not a linter
        or preprocessor is a generic transformation.
      - Enclosers: Enclosers wrap implementations and interfaces with
        headers and footers. They eventually are mapped into generic
        transformations.
      - Instrumentation: these are mapped into generic transformations.
  *)
  type transform =
    [ `Generic of
      ( Parsetree.structure With_errors.t full_impl_ast_rewrite_pass,
        Parsetree.signature With_errors.t full_intf_ast_rewrite_pass )
      pass
    | `Linter of
      ( Lint_error.t list full_impl_ast_rewrite_pass,
        Lint_error.t list full_intf_ast_rewrite_pass )
      pass
    | `Preprocessor of
      ( Parsetree.structure With_errors.t full_impl_ast_rewrite_pass,
        Parsetree.signature With_errors.t full_intf_ast_rewrite_pass )
      pass ]

  let get_transform_meta : transform -> meta = function
    | `Generic { meta; _ } -> meta
    | `Linter { meta; _ } -> meta
    | `Preprocessor { meta; _ } -> meta

  let rec last prev l = match l with [] -> prev | x :: l -> last x l

  let loc_of_list ~get_loc l =
    match l with
    | [] -> None
    | x :: l ->
        let first : Location.t = get_loc x in
        let last = get_loc (last x l) in
        Some { first with loc_end = last.loc_end }

  let merge_into_generic_mappers t ~embed_errors ~hook ~expect_mismatch_handler
      ~tool_name ~input_name =
    let { rules; enclose_impl; enclose_intf; impl; intf; _ } = t in
    let map =
      new Context_free.map_top_down
        rules ~embed_errors ~generated_code_hook:hook ~expect_mismatch_handler
    in
    let gen_header_and_footer context whole_loc f =
      let header, footer = f whole_loc in
      (match whole_loc with
      | Some (loc : Location.t) -> (
          let loc_header = { loc with loc_end = loc.loc_start } in
          let loc_footer = { loc with loc_start = loc.loc_end } in
          (match header with
          | [] -> ()
          | _ -> hook.f context loc_header (Many header));
          match footer with
          | [] -> ()
          | _ -> hook.f context loc_footer (Many footer))
      | None -> (
          match header @ footer with
          | [] -> ()
          | l ->
              let pos =
                {
                  Lexing.pos_fname = "";
                  pos_lnum = 1;
                  pos_bol = 0;
                  pos_cnum = 0;
                }
              in
              let loc =
                { Location.loc_start = pos; loc_end = pos; loc_ghost = false }
              in
              hook.f context loc (Many l)));
      (header, footer)
    in
    let input_name =
      match input_name with Some input_name -> input_name | None -> "_none_"
    in
    let map_impl ctxt st_with_attrs =
      let attrs, st =
        List.split_while st_with_attrs ~f:(function
          | { pstr_desc = Pstr_attribute _; _ } -> true
          | _ -> false)
      in
      let file_path = get_default_path_str st in
      let base_ctxt =
        Expansion_context.Base.top_level ~tool_name ~file_path ~input_name
      in
      let header, footer =
        match enclose_impl with
        | None -> ([], [])
        | Some f ->
            let whole_loc =
              loc_of_list st ~get_loc:(fun st -> st.Parsetree.pstr_loc)
            in
            gen_header_and_footer Structure_item whole_loc (f base_ctxt)
      in
      map#structure base_ctxt (List.concat [ attrs; header; st; footer ])
      >>= fun st -> match impl with None -> return st | Some f -> f ctxt st
    in
    let map_intf ctxt sg_with_attrs =
      let attrs, sg =
        List.split_while sg_with_attrs ~f:(function
          | { psig_desc = Psig_attribute _; _ } -> true
          | _ -> false)
      in
      let file_path = get_default_path_sig sg in
      let base_ctxt =
        Expansion_context.Base.top_level ~tool_name ~file_path ~input_name
      in
      let header, footer =
        match enclose_intf with
        | None -> ([], [])
        | Some f ->
            let whole_loc =
              loc_of_list sg ~get_loc:(fun sg -> sg.Parsetree.psig_loc)
            in
            gen_header_and_footer Signature_item whole_loc (f base_ctxt)
      in
      map#signature base_ctxt (List.concat [ attrs; header; sg; footer ])
      >>= fun sg -> match intf with None -> return sg | Some f -> f ctxt sg
    in
    { meta = t.meta; pass = `Both (map_impl, map_intf) }

  let builtin_of_context_free_rewriters ~hook ~rules ~enclose_impl ~enclose_intf
      ~input_name =
    let meta =
      {
        name = "<builtin:context-free>";
        aliases = [];
        registered_at = Caller_id.get ~skip:[];
      }
    in
    merge_into_generic_mappers ~hook ~input_name
      {
        meta;
        impl = None;
        intf = None;
        lint_impl = None;
        lint_intf = None;
        preprocess_impl = None;
        preprocess_intf = None;
        enclose_impl;
        enclose_intf;
        instrument = None;
        rules;
      }

  let partition_transformations ts =
    let before_instrs, after_instrs, rest =
      List.fold_left ts ~init:([], [], []) ~f:(fun (bef_i, aft_i, rest) t ->
          let reduced_t =
            {
              t with
              lint_impl = None;
              lint_intf = None;
              preprocess_impl = None;
              preprocess_intf = None;
            }
          in
          let f instr =
            (instr.Instrument.position, instr.Instrument.transformation)
          in
          match Option.map t.instrument ~f with
          | Some (Before, transf) ->
              let before =
                `Generic { meta = reduced_t.meta; pass = `Impl transf }
              in
              (before :: bef_i, aft_i, reduced_t :: rest)
          | Some (After, transf) ->
              let after =
                `Generic { meta = reduced_t.meta; pass = `Impl transf }
              in
              (bef_i, after :: aft_i, reduced_t :: rest)
          | None -> (bef_i, aft_i, reduced_t :: rest))
    in
    ( List.filter_map ts ~f:(fun t ->
          let meta =
            {
              t.meta with
              name = Printf.sprintf "<lint:%s>" t.meta.name;
              aliases = [];
            }
          in
          match (t.lint_impl, t.lint_intf) with
          | Some impl, Some intf ->
              Some (`Linter { meta; pass = `Both (impl, intf) })
          | Some impl, None -> Some (`Linter { meta; pass = `Impl impl })
          | None, Some intf -> Some (`Linter { meta; pass = `Intf intf })
          | _ -> None),
      List.filter_map ts ~f:(fun t ->
          let meta =
            {
              t.meta with
              name = Printf.sprintf "<preprocess:%s>" t.meta.name;
              aliases = [];
            }
          in
          match (t.preprocess_impl, t.preprocess_intf) with
          | Some impl, Some intf ->
              Some (`Preprocessor { meta; pass = `Both (impl, intf) })
          | Some impl, None -> Some (`Preprocessor { meta; pass = `Impl impl })
          | None, Some intf -> Some (`Preprocessor { meta; pass = `Intf intf })
          | _ -> None),
      `Before_instrs before_instrs,
      `After_instrs after_instrs,
      `Rest rest )
end

module V2 = struct
  let register_transformation = Transform.register

  let register_transformation_using_ocaml_current_ast ?impl ?intf ?aliases name
      =
    let impl =
      Option.map impl ~f:(Ppxlib_ast.Selected_ast.of_ocaml_mapper Structure)
    in
    let intf =
      Option.map intf ~f:(Ppxlib_ast.Selected_ast.of_ocaml_mapper Signature)
    in
    register_transformation ?impl ?intf ?aliases name
end

let add_ctxt_arg (f : 'a -> 'b) : Expansion_context.Base.t -> 'a -> 'b =
 fun _ x -> f x

let register_transformation ?extensions ?rules ?enclose_impl ?enclose_intf ?impl
    ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf =
  let impl = Option.map impl ~f:add_ctxt_arg in
  let intf = Option.map intf ~f:add_ctxt_arg in
  let preprocess_impl = Option.map preprocess_impl ~f:add_ctxt_arg in
  let preprocess_intf = Option.map preprocess_intf ~f:add_ctxt_arg in
  let lint_impl = Option.map lint_impl ~f:add_ctxt_arg in
  let lint_intf = Option.map lint_intf ~f:add_ctxt_arg in
  let enclose_impl = Option.map enclose_impl ~f:add_ctxt_arg in
  let enclose_intf = Option.map enclose_intf ~f:add_ctxt_arg in
  V2.register_transformation ?extensions ?rules ?enclose_impl ?enclose_intf
    ?impl ?intf ?lint_impl ?lint_intf ?preprocess_impl ?preprocess_intf

let register_code_transformation ~name ?(aliases = []) ~impl ~intf =
  register_transformation name ~impl ~intf ~aliases
[@@warning "-16"]
(* This function triggers a warning 16 as of ocaml 4.12 *)

let register_transformation_using_ocaml_current_ast ?impl ?intf =
  let impl = Option.map impl ~f:add_ctxt_arg in
  let intf = Option.map intf ~f:add_ctxt_arg in
  V2.register_transformation_using_ocaml_current_ast ?impl ?intf

let debug_dropped_attribute name ~old_dropped ~new_dropped =
  let print_diff what a b =
    let diff =
      List.filter a ~f:(fun (name : _ Loc.t) ->
          not
            (List.exists b ~f:(fun (name' : _ Location.loc) ->
                 name.txt == name'.txt)))
    in
    if not (List.is_empty diff) then (
      Printf.eprintf "The following attributes %s after applying %s:\n" what
        name;
      List.iter diff ~f:(fun { Location.txt; loc } ->
          Stdlib.Format.eprintf "- %a: %s\n" Location.print loc txt);
      Stdlib.Format.eprintf "@.")
  in
  print_diff "disappeared" new_dropped old_dropped;
  print_diff "reappeared" old_dropped new_dropped

let get_whole_ast_passes ~embed_errors ~hook ~expect_mismatch_handler ~tool_name
    ~input_name : Transform.transform list =
  let cts =
    match !apply_list with
    | None -> List.rev !Transform.all
    | Some names ->
        List.map names ~f:(fun name ->
            List.find !Transform.all ~f:(fun (ct : Transform.t) ->
                Transform.has_name ct name))
  in
  let ( linters,
        preprocess,
        `Before_instrs before_instrs,
        `After_instrs after_instrs,
        `Rest cts ) =
    Transform.partition_transformations cts
  in
  (* Allow only one preprocessor to assure deterministic order *)
  (if List.length preprocess > 1 then
     let pp =
       String.concat ~sep:", "
         (List.map preprocess ~f:(fun (`Preprocessor t) -> t.meta.name))
     in
     let err =
       Printf.sprintf "At most one preprocessor is allowed, while got: %s" pp
     in
     failwith err);
  let make_generic transforms =
    if !no_merge then
      List.map transforms ~f:(fun v ->
          let t =
            Transform.merge_into_generic_mappers ~embed_errors ~hook ~tool_name
              ~expect_mismatch_handler ~input_name v
          in
          `Generic t)
    else
      (* We merge all context-free rewriters, this also includes enclosers. *)
      let ctx_free_pass, transforms =
        let get_enclosers ~f =
          List.filter_map transforms ~f:(fun (ct : Transform.t) ->
              match f ct with None -> None | Some x -> Some (ct.meta.name, x))
          (* Sort them to ensure deterministic ordering *)
          |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
          |> List.map ~f:snd
        in

        let rules =
          List.map transforms ~f:(fun (ct : Transform.t) -> ct.rules)
          |> List.concat
        and impl_enclosers = get_enclosers ~f:(fun ct -> ct.enclose_impl)
        and intf_enclosers = get_enclosers ~f:(fun ct -> ct.enclose_intf) in
        match (rules, impl_enclosers, intf_enclosers) with
        | [], [], [] -> (None, transforms)
        | _ ->
            let merge_encloser = function
              | [] -> None
              | enclosers ->
                  Some
                    (fun ctxt loc ->
                      let headers, footers =
                        List.map enclosers ~f:(fun f -> f ctxt loc)
                        |> List.split
                      in
                      let headers = List.concat headers in
                      let footers = List.concat (List.rev footers) in
                      (headers, footers))
            in
            let pass =
              Transform.builtin_of_context_free_rewriters ~rules ~embed_errors
                ~hook ~expect_mismatch_handler
                ~enclose_impl:(merge_encloser impl_enclosers)
                ~enclose_intf:(merge_encloser intf_enclosers)
                ~tool_name ~input_name
            in
            (Some (`Generic pass), transforms)
      in
      let generic_transforms =
        List.filter_map
          ~f:(fun (ct : Transform.t) ->
            match (ct.impl, ct.intf) with
            | None, None -> None
            | Some impl, None ->
                Some (`Generic { Transform.meta = ct.meta; pass = `Impl impl })
            | None, Some intf ->
                Some (`Generic { meta = ct.meta; pass = `Intf intf })
            | Some impl, Some intf ->
                Some (`Generic { meta = ct.meta; pass = `Both (impl, intf) }))
          transforms
      in
      Option.to_list ctx_free_pass @ generic_transforms
  in
  let generics = make_generic cts in
  linters @ preprocess @ before_instrs @ generics @ after_instrs

let apply_transforms ~tool_name ~file_path ~field ~lint_field ~dropped_so_far
    ~hook ~expect_mismatch_handler ~input_name ~embed_errors ast =
  let cts =
    get_whole_ast_passes ~tool_name ~embed_errors ~hook ~expect_mismatch_handler
      ~input_name
  in
  let finish (ast, _dropped, lint_errors, errors) =
    ( ast,
      List.map lint_errors ~f:(fun (loc, s) ->
          Common.attribute_of_warning loc s),
      errors )
  in
  let acc =
    List.fold_left cts ~init:(ast, [], [], [])
      ~f:(fun
          (ast, dropped, (lint_errors : _ list), errors)
          (ct : Transform.transform)
        ->
        let input_name =
          match input_name with
          | Some input_name -> input_name
          | None -> "_none_"
        in
        let ctxt =
          Expansion_context.Base.top_level ~tool_name ~file_path ~input_name
        in

        let lint_errors, errors =
          match lint_field ct with
          | None -> (lint_errors, errors)
          | Some f -> (
              try (lint_errors @ f ctxt ast, errors)
              with exn when embed_errors ->
                (lint_errors, exn_to_loc_error exn :: errors))
        in
        match field ct with
        | None -> (ast, dropped, lint_errors, errors)
        | Some f ->
            let (ast, more_errors), errors =
              try (f ctxt ast, errors)
              with exn when embed_errors ->
                ((ast, []), exn_to_loc_error exn :: errors)
            in
            let dropped =
              if !debug_attribute_drop then (
                let new_dropped = dropped_so_far ast in
                let name = (Transform.get_transform_meta ct).name in
                debug_dropped_attribute name ~old_dropped:dropped ~new_dropped;
                new_dropped)
              else []
            in
            (ast, dropped, lint_errors, errors @ more_errors))
  in
  finish acc

(*$*)

let error_to_str_extension error =
  let loc = Location.none in
  let ext = Location.Error.to_extension error in
  Ast_builder.Default.pstr_extension ~loc ext []

(*$ str_to_sig _last_text_block *)

let error_to_sig_extension error =
  let loc = Location.none in
  let ext = Location.Error.to_extension error in
  Ast_builder.Default.psig_extension ~loc ext []

(*$*)

let error_to_extension error ~(kind : Kind.t) =
  match kind with
  | Intf -> Intf_or_impl.Intf [ error_to_sig_extension error ]
  | Impl -> Intf_or_impl.Impl [ error_to_str_extension error ]

let exn_to_extension exn ~(kind : Kind.t) =
  exn_to_loc_error exn |> error_to_extension ~kind

(* +-----------------------------------------------------------------+
   | Actual rewriting of structure/signatures                        |
   +-----------------------------------------------------------------+ *)

let print_passes () =
  let tool_name = "ppxlib_driver" in
  let embed_errors = false in
  let hook = Context_free.Generated_code_hook.nop in
  let expect_mismatch_handler = Context_free.Expect_mismatch_handler.nop in
  let cts =
    get_whole_ast_passes ~embed_errors ~hook ~expect_mismatch_handler ~tool_name
      ~input_name:None
  in
  if !perform_checks then
    Printf.printf "<builtin:freshen-and-collect-attributes>\n";
  List.iter cts ~f:(fun ct ->
      Printf.printf "%s\n" (Transform.get_transform_meta ct).name);
  if !perform_checks then (
    Printf.printf "<builtin:check-unused-attributes>\n";
    if !perform_checks_on_extensions then
      Printf.printf "<builtin:check-unused-extensions>\n")

let sort_errors_by_loc errors =
  List.sort errors ~cmp:(fun error error' ->
      let loc = Location.Error.get_location error in
      let loc' = Location.Error.get_location error' in
      Location.compare loc loc')

(*$*)

let map_structure_gen st ~tool_name ~hook ~expect_mismatch_handler ~input_name
    ~embed_errors =
  Cookies.acknowledge_cookies T;
  if !perform_checks then (
    Attribute.reset_checks ();
    Attribute.collect#structure st);
  let lint lint_errors st =
    let st =
      match lint_errors with
      | [] -> st
      | _ ->
          List.map lint_errors
            ~f:(fun ({ attr_name = { loc; _ }; _ } as attr) ->
              Ast_builder.Default.pstr_attribute ~loc attr)
          @ st
    in
    st
  in
  let with_errors errors st =
    let sorted = sort_errors_by_loc errors in
    List.map sorted ~f:(fun error ->
        Ast_builder.Default.pstr_extension
          ~loc:(Location.Error.get_location error)
          (Location.Error.to_extension error)
          []
        |> ghost#structure_item)
    @ st
  in
  let cookies_and_check st =
    Cookies.call_post_handlers T;
    let errors =
      if !perform_checks then (
        (* TODO: these two passes could be merged, we now have more passes for
           checks than for actual rewriting. *)
        let unused_attributes_errors =
          Attribute.collect_unused_attributes_errors#structure st []
        in
        let unused_extension_errors =
          if !perform_checks_on_extensions then
            Extension.collect_unhandled_extension_errors#structure st []
          else []
        in
        let not_seen_errors = Attribute.collect_unseen_errors () in
        (if !perform_locations_check then
           let open Location_check in
           ignore
             ((enforce_invariants !loc_fname)#structure st
                Non_intersecting_ranges.empty
               : Non_intersecting_ranges.t));
        unused_attributes_errors @ unused_extension_errors @ not_seen_errors)
      else []
    in
    with_errors errors st
  in
  let file_path = get_default_path_str st in
  let st, lint_errors, errors =
    apply_transforms st ~tool_name ~file_path
      ~field:(function
        | `Generic pass -> Transform.get_pass_impl pass | _ -> None)
      ~lint_field:(function
        | `Linter pass -> Transform.get_pass_impl pass | _ -> None)
      ~dropped_so_far:Attribute.dropped_so_far_structure ~hook
      ~expect_mismatch_handler ~input_name ~embed_errors
  in
  st |> lint lint_errors |> cookies_and_check |> with_errors (List.rev errors)

let map_structure st =
  match
    map_structure_gen st
      ~tool_name:(Astlib.Ast_metadata.tool_name ())
      ~hook:Context_free.Generated_code_hook.nop
      ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop
      ~input_name:None ~embed_errors:false
  with
  | ast -> ast

(*$ str_to_sig _last_text_block *)

let map_signature_gen sg ~tool_name ~hook ~expect_mismatch_handler ~input_name
    ~embed_errors =
  Cookies.acknowledge_cookies T;
  if !perform_checks then (
    Attribute.reset_checks ();
    Attribute.collect#signature sg);
  let lint lint_errors sg =
    let sg =
      match lint_errors with
      | [] -> sg
      | _ ->
          List.map lint_errors
            ~f:(fun ({ attr_name = { loc; _ }; _ } as attr) ->
              Ast_builder.Default.psig_attribute ~loc attr)
          @ sg
    in
    sg
  in
  let with_errors errors sg =
    let sorted = sort_errors_by_loc errors in
    List.map sorted ~f:(fun error ->
        Ast_builder.Default.psig_extension
          ~loc:(Location.Error.get_location error)
          (Location.Error.to_extension error)
          []
        |> ghost#signature_item)
    @ sg
  in
  let cookies_and_check sg =
    Cookies.call_post_handlers T;
    let errors =
      if !perform_checks then (
        (* TODO: these two passes could be merged, we now have more passes for
           checks than for actual rewriting. *)
        let unused_attributes_errors =
          Attribute.collect_unused_attributes_errors#signature sg []
        in
        let unused_extension_errors =
          if !perform_checks_on_extensions then
            Extension.collect_unhandled_extension_errors#signature sg []
          else []
        in
        let not_seen_errors = Attribute.collect_unseen_errors () in
        (if !perform_locations_check then
           let open Location_check in
           ignore
             ((enforce_invariants !loc_fname)#signature sg
                Non_intersecting_ranges.empty
               : Non_intersecting_ranges.t));
        unused_attributes_errors @ unused_extension_errors @ not_seen_errors)
      else []
    in
    with_errors errors sg
  in
  let file_path = get_default_path_sig sg in
  let sg, lint_errors, errors =
    apply_transforms sg ~tool_name ~file_path
      ~field:(function
        | `Generic pass -> Transform.get_pass_intf pass | _ -> None)
      ~lint_field:(function
        | `Linter pass -> Transform.get_pass_intf pass | _ -> None)
      ~dropped_so_far:Attribute.dropped_so_far_signature ~hook
      ~expect_mismatch_handler ~input_name ~embed_errors
  in
  sg |> lint lint_errors |> cookies_and_check |> with_errors (List.rev errors)

let map_signature sg =
  match
    map_signature_gen sg
      ~tool_name:(Astlib.Ast_metadata.tool_name ())
      ~hook:Context_free.Generated_code_hook.nop
      ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop
      ~input_name:None ~embed_errors:false
  with
  | ast -> ast

(*$*)

(* +-----------------------------------------------------------------+
   | Entry points                                                    |
   +-----------------------------------------------------------------+ *)

let string_contains_binary_ast s =
  let test magic_number =
    String.is_prefix s ~prefix:(String.sub magic_number ~pos:0 ~len:9)
  in
  test Ast_magic.ast_intf_magic_number || test Ast_magic.ast_impl_magic_number

let versioned_errorf input_version input_file_name =
  Printf.ksprintf (fun msg ->
      let err =
        Location.Error.make ~loc:(Location.in_file input_file_name) msg ~sub:[]
      in
      Error (err, input_version))

let remove_no_error fn = try Stdlib.Sys.remove fn with Sys_error _ -> ()

let protectx x ~f ~finally =
  match f x with
  | v ->
      finally x;
      v
  | exception e ->
      finally x;
      raise e

let with_preprocessed_file fn ~f =
  match !preprocessor with
  | None -> f fn
  | Some pp ->
      protectx (Stdlib.Filename.temp_file "ocamlpp" "") ~finally:remove_no_error
        ~f:(fun tmpfile ->
          match System.run_preprocessor ~pp ~input:fn ~output:tmpfile with
          | Ok () -> f tmpfile
          | Error (failed_command, fall_back_version) ->
              versioned_errorf fall_back_version fn
                "Error while running external preprocessor\nCommand line: %s\n"
                failed_command)

let relocate_mapper =
  object
    inherit [string * string] Ast_traverse.map_with_context

    method! position (old_fn, new_fn) pos =
      if String.equal pos.pos_fname old_fn then { pos with pos_fname = new_fn }
      else pos
  end

(* Set the input name globally. This is used by some ppx rewriters
   such as bisect_ppx. *)
let set_input_name = Astlib.Location.set_input_name

let load_input ~(kind : Kind.t) ~input_name ~relocate fn =
  set_input_name input_name;
  let input_source = if String.equal fn "-" then Ast_io.Stdin else File fn in
  let input_kind = Ast_io.Possibly_source (kind, input_name) in
  match Ast_io.read input_source ~input_kind with
  | Ok { input_name = ast_input_name; input_version; ast } ->
      let ast_kind = Intf_or_impl.kind ast in
      if not (Kind.equal kind ast_kind) then
        versioned_errorf input_version fn
          "File contains a binary %s AST but an %s was expected"
          (Kind.describe ast_kind) (Kind.describe kind)
      else if String.equal ast_input_name input_name || not relocate then (
        set_input_name ast_input_name;
        Ok (ast_input_name, input_version, ast))
      else
        Ok
          ( input_name,
            input_version,
            Intf_or_impl.map_with_context ast relocate_mapper
              (ast_input_name, input_name) )
  | Error (Unknown_version (unknown_magic, fall_back_version)) ->
      versioned_errorf fall_back_version fn
        "File is a binary ast for an unknown version of OCaml with magic \
         number '%s'"
        unknown_magic
  | Error (System_error (error, fall_back_version))
  | Error (Source_parse_error (error, fall_back_version)) ->
      Error (error, fall_back_version)
  | Error Not_a_binary_ast -> assert false

let load_input_run_as_ppx fn =
  (* If there's an error while loading in run_as_ppx mode, the kind of AST (impl/intf) is still unknown.
     That's why, as opposed to load_input, this function raises errors instead of returning a result:
     handling an error by returning an AST with the error packed as extension node wouldn't be possible. *)
  match Ast_io.read (File fn) ~input_kind:Ast_io.Necessarily_binary with
  | Ok { input_name = ast_input_name; input_version; ast } ->
      let ast =
        match !loc_fname with
        | None ->
            set_input_name ast_input_name;
            ast
        | Some input_name ->
            set_input_name input_name;
            if String.equal ast_input_name input_name then ast
            else
              Intf_or_impl.map_with_context ast relocate_mapper
                (ast_input_name, input_name)
      in
      (* With `--as-ppx`, ocaml calls the standalone separately for every structure/signature item
         with the filename as metadata that it gets from the previous call. relocate_mapper only
         relocates positions whose position filename coincides with that metadata filename.
         So always return the metadata filename itself, even if `-loc-filename` is provided. *)
      (ast_input_name, input_version, ast)
  | Error (Unknown_version (unknown_magic, _)) ->
      Location.raise_errorf ~loc:(Location.in_file fn)
        "The input is a binary ast for an unknown version of OCaml with magic \
         number '%s'"
        unknown_magic
  | Error Not_a_binary_ast ->
      Location.raise_errorf ~loc:(Location.in_file fn)
        "Expected a binary AST as input"
  | Error (System_error (error, _)) | Error (Source_parse_error (error, _)) ->
      let open Location.Error in
      Location.set_filename (get_location error) fn |> update_loc error |> raise

let load_source_file fn =
  let s = In_channel.read_all fn in
  if string_contains_binary_ast s then
    Location.raise_errorf ~loc:(Location.in_file fn)
      "ppxlib_driver: cannot use -reconcile with binary AST files";
  s

type output_mode =
  | Pretty_print
  | Dump_ast
  | Dparsetree
  | Reconcile of Reconcile.mode
  | Null

(*$*)
let extract_cookies_str st =
  let st =
    match st with
    | ({
         pstr_desc =
           Pstr_attribute { attr_name = { txt = "ocaml.ppx.context"; _ }; _ };
         _;
       } as prefix)
      :: st ->
        let prefix = Ppxlib_ast.Selected_ast.to_ocaml Structure [ prefix ] in
        assert (
          List.is_empty
            (Astlib.Ast_metadata.drop_ppx_context_str ~restore:true prefix));
        st
    | _ -> st
  in
  (* The cli cookies have to be set after restoring the ppx context,
     since restoring the ppx context resets the cookies *)
  List.iter !Cookies.given_through_cli ~f:(fun (name, expr) ->
      Cookies.set T name expr);
  st

let add_cookies_str st =
  let prefix =
    Astlib.Ast_metadata.add_ppx_context_str ~tool_name:"ppxlib_driver" []
    |> Ppxlib_ast.Selected_ast.of_ocaml Structure
  in
  prefix @ st

(*$ str_to_sig _last_text_block *)
let extract_cookies_sig sg =
  let sg =
    match sg with
    | ({
         psig_desc =
           Psig_attribute { attr_name = { txt = "ocaml.ppx.context"; _ }; _ };
         _;
       } as prefix)
      :: sg ->
        let prefix = Ppxlib_ast.Selected_ast.to_ocaml Signature [ prefix ] in
        assert (
          List.is_empty
            (Astlib.Ast_metadata.drop_ppx_context_sig ~restore:true prefix));
        sg
    | _ -> sg
  in
  (* The cli cookies have to be set after restoring the ppx context,
     since restoring the ppx context resets the cookies *)
  List.iter !Cookies.given_through_cli ~f:(fun (name, expr) ->
      Cookies.set T name expr);
  sg

let add_cookies_sig sg =
  let prefix =
    Astlib.Ast_metadata.add_ppx_context_sig ~tool_name:"ppxlib_driver" []
    |> Ppxlib_ast.Selected_ast.of_ocaml Signature
  in
  prefix @ sg

(*$*)

let extract_cookies (ast : Intf_or_impl.t) : Intf_or_impl.t =
  match ast with
  | Intf x -> Intf (extract_cookies_sig x)
  | Impl x -> Impl (extract_cookies_str x)

let add_cookies (ast : Intf_or_impl.t) : Intf_or_impl.t =
  match ast with
  | Intf x -> Intf (add_cookies_sig x)
  | Impl x -> Impl (add_cookies_str x)

let corrections = ref []
let add_to_list r x = r := x :: !r

let register_correction ~loc ~repl =
  add_to_list corrections
    (Reconcile.Replacement.make_text () ~start:loc.loc_start ~stop:loc.loc_end
       ~repl)

let process_file_hooks = ref []
let register_process_file_hook f = add_to_list process_file_hooks f

module File_property = struct
  type 'a t = {
    name : string;
    mutable data : 'a option;
    sexp_of_t : 'a -> Sexp.t;
  }

  type packed = T : _ t -> packed

  let all = ref []
  let register t = add_to_list all (T t)
  let reset_all () = List.iter !all ~f:(fun (T t) -> t.data <- None)

  let dump_and_reset_all () =
    List.filter_map (List.rev !all) ~f:(fun (T t) ->
        match t.data with
        | None -> None
        | Some v ->
            t.data <- None;
            Some (t.name, t.sexp_of_t v))
end

module Create_file_property
    (Name : sig
      val name : string
    end)
    (T : Sexpable.S) =
struct
  let t : _ File_property.t =
    { name = Name.name; data = None; sexp_of_t = T.sexp_of_t }

  let () = File_property.register t
  let set x = t.data <- Some x
end

let process_ast (ast : Intf_or_impl.t) ~input_name ~tool_name ~hook
    ~expect_mismatch_handler ~embed_errors =
  match ast with
  | Intf x ->
      let ast =
        match
          map_signature_gen x ~tool_name ~hook ~expect_mismatch_handler
            ~input_name:(Some input_name) ~embed_errors
        with
        | ast -> ast
      in
      Intf_or_impl.Intf ast
  | Impl x ->
      let ast =
        match
          map_structure_gen x ~tool_name ~hook ~expect_mismatch_handler
            ~input_name:(Some input_name) ~embed_errors
        with
        | ast -> ast
      in
      Intf_or_impl.Impl ast

let process_file (kind : Kind.t) fn ~input_name ~relocate ~output_mode
    ~embed_errors ~output =
  File_property.reset_all ();
  List.iter (List.rev !process_file_hooks) ~f:(fun f -> f ());
  corrections := [];
  let replacements = ref [] in
  let tool_name = "ppx_driver" in
  let hook : Context_free.Generated_code_hook.t =
    match output_mode with
    | Reconcile (Using_line_directives | Delimiting_generated_blocks) ->
        {
          f =
            (fun context (loc : Location.t) generated ->
              add_to_list replacements
                (Reconcile.Replacement.make () ~context:(Extension context)
                   ~start:loc.loc_start ~stop:loc.loc_end ~repl:generated));
        }
    | _ -> Context_free.Generated_code_hook.nop
  in
  let expect_mismatch_handler : Context_free.Expect_mismatch_handler.t =
    {
      f =
        (fun context (loc : Location.t) generated ->
          add_to_list corrections
            (Reconcile.Replacement.make () ~context:(Floating_attribute context)
               ~start:loc.loc_start ~stop:loc.loc_end ~repl:(Many generated)));
    }
  in

  let input_name, input_version, ast =
    let preprocessed_and_loaded =
      with_preprocessed_file fn ~f:(load_input ~kind ~input_name ~relocate)
    in
    match preprocessed_and_loaded with
    | Ok (input_fname, input_version, ast) -> (
        try
          let ast =
            extract_cookies ast
            |> process_ast ~input_name ~tool_name ~hook ~expect_mismatch_handler
                 ~embed_errors
          in
          (input_fname, input_version, ast)
        with exn when embed_errors ->
          (input_fname, input_version, exn_to_extension exn ~kind))
    | Error (error, input_version) when embed_errors ->
        (input_name, input_version, error_to_extension error ~kind)
    | Error (error, _) ->
        let open Location.Error in
        Location.set_filename (get_location error) fn
        |> update_loc error |> raise
  in
  Option.iter !output_metadata_filename ~f:(fun fn ->
      let metadata = File_property.dump_and_reset_all () in
      Out_channel.write_all fn
        ~data:
          (List.map metadata ~f:(fun (s, sexp) ->
               Sexp.to_string_hum (Sexp.List [ Atom s; sexp ]) ^ "\n")
          |> String.concat ~sep:""));

  let input_contents = lazy (load_source_file fn) in
  let corrected = fn ^ !corrected_suffix in
  let mismatches_found =
    match !corrections with
    | [] ->
        if Stdlib.Sys.file_exists corrected then Stdlib.Sys.remove corrected;
        false
    | corrections ->
        Reconcile.reconcile corrections
          ~contents:(Lazy.force input_contents)
          ~output:(Some corrected) ~input_filename:fn ~input_name
          ~target:Corrected ?styler:!styler ~kind;
        true
  in

  (match output_mode with
  | Null -> ()
  | Pretty_print ->
      with_output output ~binary:false ~f:(fun oc ->
          let ppf = Stdlib.Format.formatter_of_out_channel oc in
          (match ast with
          | Intf ast -> Pprintast.signature ppf ast
          | Impl ast -> Pprintast.structure ppf ast);
          let null_ast =
            match ast with Intf [] | Impl [] -> true | _ -> false
          in
          if not null_ast then Stdlib.Format.pp_print_newline ppf ())
  | Dump_ast ->
      with_output output ~binary:true ~f:(fun oc ->
          Ast_io.write oc
            { input_name; input_version; ast }
            ~add_ppx_context:true)
  | Dparsetree ->
      with_output output ~binary:false ~f:(fun oc ->
          let ppf = Stdlib.Format.formatter_of_out_channel oc in
          let ast = add_cookies ast in
          (match ast with
          | Intf ast -> Sexp.pp_hum ppf (Ast_traverse.sexp_of#signature ast)
          | Impl ast -> Sexp.pp_hum ppf (Ast_traverse.sexp_of#structure ast));
          Stdlib.Format.pp_print_newline ppf ())
  | Reconcile mode ->
      Reconcile.reconcile !replacements
        ~contents:(Lazy.force input_contents)
        ~output ~input_filename:fn ~input_name ~target:(Output mode)
        ?styler:!styler ~kind);

  if
    mismatches_found && match !diff_command with Some "-" -> false | _ -> true
  then (
    Ppxlib_print_diff.print () ~file1:fn ~file2:corrected ~use_color:!use_color
      ?diff_command:!diff_command;
    Stdlib.exit 1)

let output_mode = ref Pretty_print
let output = ref None
let kind = ref None
let input = ref None
let embed_errors = ref false

let set_input fn =
  match !input with
  | None -> input := Some fn
  | Some _ -> raise (Arg.Bad "too many input files")

let set_kind k =
  match !kind with
  | Some k' when not (Kind.equal k k') ->
      raise (Arg.Bad "must specify at most one of -impl or -intf")
  | _ -> kind := Some k

let set_output_mode mode =
  match (!output_mode, mode) with
  | Pretty_print, _ -> output_mode := mode
  | _, Pretty_print -> assert false
  | Dump_ast, Dump_ast | Dparsetree, Dparsetree -> ()
  | Reconcile a, Reconcile b when Poly.equal a b -> ()
  | x, y ->
      let arg_of_output_mode = function
        | Pretty_print -> assert false
        | Dump_ast -> "-dump-ast"
        | Dparsetree -> "-dparsetree"
        | Reconcile Using_line_directives -> "-reconcile"
        | Reconcile Delimiting_generated_blocks -> "-reconcile-with-comments"
        | Null -> "-null"
      in
      raise
        (Arg.Bad
           (Printf.sprintf "%s and %s are incompatible" (arg_of_output_mode x)
              (arg_of_output_mode y)))

let print_transformations () =
  List.iter !Transform.all ~f:(fun (ct : Transform.t) ->
      Printf.printf "%s\n" ct.meta.name)

let parse_apply_list s =
  let names =
    if String.equal s "" then [] else String.split_on_char s ~sep:','
  in
  List.iter names ~f:(fun name ->
      if
        not
          (List.exists !Transform.all ~f:(fun (ct : Transform.t) ->
               Transform.has_name ct name))
      then
        raise
          (Stdlib.Arg.Bad
             (Printf.sprintf "code transformation '%s' does not exist" name)));
  names

type mask = {
  mutable apply : string list option;
  mutable dont_apply : string list option;
}

let mask = { apply = None; dont_apply = None }

let handle_apply s =
  if Option.is_some mask.apply then
    raise (Arg.Bad "-apply called too many times");
  (* This is not strictly necessary but it's more intuitive *)
  if Option.is_some mask.dont_apply then
    raise (Arg.Bad "-apply must be called before -dont-apply");
  mask.apply <- Some (parse_apply_list s)

let handle_dont_apply s =
  if Option.is_some mask.dont_apply then
    raise (Arg.Bad "-apply called too many times");
  mask.dont_apply <- Some (parse_apply_list s)

let interpret_mask () =
  if Option.is_some mask.apply || Option.is_some mask.dont_apply then
    let selected_transform_name ct =
      let is_candidate =
        match mask.apply with
        | None -> true
        | Some names -> List.exists names ~f:(Transform.has_name ct)
      in
      let is_selected =
        match mask.dont_apply with
        | None -> is_candidate
        | Some names ->
            is_candidate && not (List.exists names ~f:(Transform.has_name ct))
      in
      if is_selected then Some ct.meta.name else None
    in
    apply_list :=
      Some (List.filter_map !Transform.all ~f:selected_transform_name)

let set_cookie s =
  match String.lsplit2 s ~on:'=' with
  | None ->
      raise (Arg.Bad "invalid cookie, must be of the form \"<name>=<expr>\"")
  | Some (name, value) ->
      let lexbuf = Lexing.from_string value in
      lexbuf.Lexing.lex_curr_p <-
        {
          Lexing.pos_fname = "<command-line>";
          pos_lnum = 1;
          pos_bol = 0;
          pos_cnum = 0;
        };
      let expr = Parse.expression lexbuf in
      Cookies.given_through_cli := (name, expr) :: !Cookies.given_through_cli

let shared_args =
  [
    ( "-loc-filename",
      Arg.String (fun s -> loc_fname := Some s),
      "<string> File name to use in locations" );
    ( "-reserve-namespace",
      Arg.String Name.Reserved_namespaces.reserve,
      "<string> Mark the given namespace as reserved" );
    ("-no-check", Arg.Clear perform_checks, " Disable checks (unsafe)");
    ("-check", Arg.Set perform_checks, " Enable checks");
    ( "-no-check-on-extensions",
      Arg.Clear perform_checks_on_extensions,
      " Disable checks on extension point only" );
    ( "-check-on-extensions",
      Arg.Set perform_checks_on_extensions,
      " Enable checks on extension point only" );
    ( "-no-locations-check",
      Arg.Clear perform_locations_check,
      " Disable locations check only" );
    ( "-locations-check",
      Arg.Set perform_locations_check,
      " Enable locations check only" );
    ( "-apply",
      Arg.String handle_apply,
      "<names> Apply these transformations in order (comma-separated list)" );
    ( "-dont-apply",
      Arg.String handle_dont_apply,
      "<names> Exclude these transformations" );
    ( "-no-merge",
      Arg.Set no_merge,
      " Do not merge context free transformations (better for debugging \
       rewriters). As a result, the context-free transformations are not all \
       applied before all impl and intf." );
    ("-cookie", Arg.String set_cookie, "NAME=EXPR Set the cookie NAME to EXPR");
    ("--cookie", Arg.String set_cookie, " Same as -cookie");
  ]

let () =
  List.iter shared_args ~f:(fun (key, spec, doc) -> add_arg key spec ~doc)

let as_pp () =
  set_output_mode Dump_ast;
  embed_errors := true

let standalone_args =
  [
    ( "-as-ppx",
      Arg.Unit (fun () -> raise (Arg.Bad "-as-ppx must be the first argument")),
      " Run as a -ppx rewriter (must be the first argument)" );
    ( "--as-ppx",
      Arg.Unit (fun () -> raise (Arg.Bad "--as-ppx must be the first argument")),
      " Same as -as-ppx" );
    ("-as-pp", Arg.Unit as_pp, " Shorthand for: -dump-ast -embed-errors");
    ("--as-pp", Arg.Unit as_pp, " Same as -as-pp");
    ( "-o",
      Arg.String (fun s -> output := Some s),
      "<filename> Output file (use '-' for stdout)" );
    ("-", Arg.Unit (fun () -> set_input "-"), " Read input from stdin");
    ( "-dump-ast",
      Arg.Unit (fun () -> set_output_mode Dump_ast),
      " Dump the marshaled ast to the output file instead of pretty-printing it"
    );
    ( "--dump-ast",
      Arg.Unit (fun () -> set_output_mode Dump_ast),
      " Same as -dump-ast" );
    ( "-dparsetree",
      Arg.Unit (fun () -> set_output_mode Dparsetree),
      " Print the parsetree (same as ocamlc -dparsetree)" );
    ( "-embed-errors",
      Arg.Set embed_errors,
      " Embed errors in the output AST (default: true when -as-pp, false \
       otherwise)" );
    ( "-null",
      Arg.Unit (fun () -> set_output_mode Null),
      " Produce no output, except for errors" );
    ( "-impl",
      Arg.Unit (fun () -> set_kind Impl),
      "<file> Treat the input as a .ml file" );
    ("--impl", Arg.Unit (fun () -> set_kind Impl), "<file> Same as -impl");
    ( "-intf",
      Arg.Unit (fun () -> set_kind Intf),
      "<file> Treat the input as a .mli file" );
    ("--intf", Arg.Unit (fun () -> set_kind Intf), "<file> Same as -intf");
    ( "-debug-attribute-drop",
      Arg.Set debug_attribute_drop,
      " Debug attribute dropping" );
    ( "-print-transformations",
      Arg.Set request_print_transformations,
      " Print linked-in code transformations, in the order they are applied" );
    ( "-print-passes",
      Arg.Set request_print_passes,
      " Print the actual passes over the whole AST in the order they are \
       applied" );
    ( "-ite-check",
      Arg.Unit
        (fun () ->
          Printf.eprintf
            "Warning: the -ite-check flag is deprecated and has no effect.\n%!";
          Extra_warnings.care_about_ite_branch := true),
      " (no effect -- kept for compatibility)" );
    ( "-pp",
      Arg.String (fun s -> preprocessor := Some s),
      "<command>  Pipe sources through preprocessor <command> (incompatible \
       with -as-ppx)" );
    ( "-reconcile",
      Arg.Unit (fun () -> set_output_mode (Reconcile Using_line_directives)),
      " (WIP) Pretty print the output using a mix of the input source and the \
       generated code" );
    ( "-reconcile-with-comments",
      Arg.Unit
        (fun () -> set_output_mode (Reconcile Delimiting_generated_blocks)),
      " (WIP) same as -reconcile but uses comments to enclose the generated \
       code" );
    ("-no-color", Arg.Clear use_color, " Don't use colors when printing errors");
    ( "-diff-cmd",
      Arg.String (fun s -> diff_command := Some s),
      " Diff command when using code expectations (use - to disable diffing)" );
    ( "-pretty",
      Arg.Set pretty,
      " Instruct code generators to improve the prettiness of the generated \
       code" );
    ("-styler", Arg.String (fun s -> styler := Some s), " Code styler");
    ( "-output-metadata",
      Arg.String (fun s -> output_metadata_filename := Some s),
      "FILE Where to store the output metadata" );
    ( "-corrected-suffix",
      Arg.Set_string corrected_suffix,
      "SUFFIX Suffix to append to corrected files" );
  ]

let get_args ?(standalone_args = standalone_args) () =
  standalone_args @ List.rev !args

let standalone_main () =
  let usage = Printf.sprintf "%s [extra_args] [<files>]" exe_name in
  let args = get_args () in
  Arg.parse (Arg.align args) set_input usage;
  interpret_mask ();
  if !request_print_transformations then (
    print_transformations ();
    Stdlib.exit 0);
  if !request_print_passes then (
    print_passes ();
    Stdlib.exit 0);
  match !input with
  | None ->
      Printf.eprintf "%s: no input file given\n%!" exe_name;
      Stdlib.exit 2
  | Some fn ->
      let kind =
        match !kind with
        | Some k -> k
        | None -> (
            match Kind.of_filename fn with
            | Some k -> k
            | None ->
                Printf.eprintf
                  "%s: don't know what to do with '%s', use -impl or -intf.\n"
                  exe_name fn;
                Stdlib.exit 2)
      in
      let input_name, relocate =
        match !loc_fname with None -> (fn, false) | Some fn -> (fn, true)
      in
      process_file kind fn ~input_name ~relocate ~output_mode:!output_mode
        ~output:!output ~embed_errors:!embed_errors

let rewrite_binary_ast_file input_fn output_fn =
  let input_name, input_version, ast = load_input_run_as_ppx input_fn in
  let ast =
    try
      let ast = extract_cookies ast in
      let tool_name = Astlib.Ast_metadata.tool_name () in
      let hook = Context_free.Generated_code_hook.nop in
      let expect_mismatch_handler = Context_free.Expect_mismatch_handler.nop in
      process_ast ast ~input_name ~tool_name ~hook ~expect_mismatch_handler
        ~embed_errors:true
    with exn -> exn_to_extension exn ~kind:(Intf_or_impl.kind ast)
  in
  with_output (Some output_fn) ~binary:true ~f:(fun oc ->
      Ast_io.write oc { input_name; input_version; ast } ~add_ppx_context:true)

let parse_input passed_in_args ~valid_args ~incorrect_input_msg =
  try
    Arg.parse_argv passed_in_args (Arg.align valid_args)
      (fun _ -> raise (Arg.Bad "anonymous arguments not accepted"))
      incorrect_input_msg
  with
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      Stdlib.exit 2
  | Arg.Help msg ->
      Printf.eprintf "%s" msg;
      Stdlib.exit 0

let run_as_ppx_rewriter_main ~standalone_args ~usage input =
  let valid_args = get_args ~standalone_args () in
  match List.rev @@ Array.to_list @@ input with
  | output_fn :: input_fn :: flags_and_prog_name
    when List.length flags_and_prog_name > 0 ->
      let prog_name_and_flags = List.rev flags_and_prog_name |> Array.of_list in
      parse_input prog_name_and_flags ~valid_args ~incorrect_input_msg:usage;
      interpret_mask ();
      rewrite_binary_ast_file input_fn output_fn;
      Stdlib.exit 0
  | [ help; _ ] when String.equal help "-help" || String.equal help "--help" ->
      parse_input input ~valid_args ~incorrect_input_msg:usage;
      assert false
  | _ ->
      Printf.eprintf "Usage: %s\n%!" usage;
      Stdlib.exit 2

let standalone_run_as_ppx_rewriter () =
  let n = Array.length Stdlib.Sys.argv in
  let usage =
    Printf.sprintf "%s -as-ppx [extra_args] <infile> <outfile>" exe_name
  in
  let argv = Array.make (n - 1) "" in
  argv.(0) <- Stdlib.Sys.argv.(0);
  for i = 1 to n - 2 do
    argv.(i) <- Stdlib.Sys.argv.(i + 1)
  done;
  let standalone_args =
    List.map standalone_args ~f:(fun (arg, spec, _doc) ->
        (arg, spec, " Unused with -as-ppx"))
  in
  run_as_ppx_rewriter_main ~standalone_args ~usage argv

let standalone () =
  Astlib.init_error_reporting_style_using_env_vars ();
  try
    if
      Array.length Stdlib.Sys.argv >= 2
      &&
      match Stdlib.Sys.argv.(1) with
      | "-as-ppx" | "--as-ppx" -> true
      | _ -> false
    then standalone_run_as_ppx_rewriter ()
    else standalone_main ();
    Stdlib.exit 0
  with exn ->
    Location.report_exception Stdlib.Format.err_formatter exn;
    Stdlib.exit 1

let run_as_ppx_rewriter () =
  let usage = Printf.sprintf "%s [extra_args] <infile> <outfile>" exe_name in
  let input = Stdlib.Sys.argv in
  try run_as_ppx_rewriter_main ~standalone_args:[] ~usage input
  with exn ->
    Location.report_exception Stdlib.Format.err_formatter exn;
    Stdlib.exit 1

let pretty () = !pretty

let enable_checks () =
  (* We do not enable the locations check here, we currently require that one
     to be specifically enabled. *)
  perform_checks := true;
  perform_checks_on_extensions := true

let enable_location_check () = perform_locations_check := true
let disable_location_check () = perform_locations_check := false
let map_structure st = map_structure st
