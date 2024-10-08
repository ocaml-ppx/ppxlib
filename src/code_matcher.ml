(*$ open Ppxlib_cinaps_helpers $*)
open! Import
module Format = Stdlib.Format
module Filename = Stdlib.Filename

(* TODO: make the "deriving." depend on the matching attribute name. *)
let end_marker_sig =
  Attribute.Floating.declare "deriving.end" Signature_item
    Ast_pattern.(pstr nil)
    ()

let end_marker_str =
  Attribute.Floating.declare "deriving.end" Structure_item
    Ast_pattern.(pstr nil)
    ()

module type T1 = sig
  type 'a t
end

module Make (M : sig
  type t
  type compiler_t

  val get_loc : t -> Location.t
  val end_marker : (t, unit) Attribute.Floating.t

  module Transform (T : T1) : sig
    val apply :
      < structure_item : structure_item T.t
      ; signature_item : signature_item T.t
      ; .. > ->
      t T.t
  end

  val parse : Lexing.lexbuf -> t list
  val to_sexp : t -> Sexp.t
  val to_compiler : t -> compiler_t
  val pp_compiler : Format.formatter -> compiler_t -> unit
end) =
struct
  let extract_prefix ~pos l =
    let rec loop acc = function
      | [] ->
          let loc =
            { Location.loc_start = pos; loc_end = pos; loc_ghost = false }
          in
          Error
            ( Location.Error.createf ~loc "ppxlib: [@@@@@@%s] attribute missing"
                (Attribute.Floating.name M.end_marker),
              [] )
      | x :: l -> (
          match Attribute.Floating.convert_res [ M.end_marker ] x with
          | Ok None -> loop (x :: acc) l
          | Ok (Some ()) -> Ok (List.rev acc, (M.get_loc x).loc_start)
          | Error e -> Error e
          | exception Failure _ -> loop (x :: acc) l)
    in
    loop [] l

  let remove_loc =
    object
      inherit Ast_traverse.map
      method! location _ = Location.none
      method! location_stack _ = []
    end

  module M_map = M.Transform (struct
    type 'a t = 'a -> 'a
  end)

  let remove_loc x = M_map.apply remove_loc x
  let rec last prev = function [] -> prev | x :: l -> last x l

  let diff_asts ~generated ~round_trip =
    let with_temp_file f =
      Exn.protectx
        (Filename.temp_file "ppxlib" "")
        ~finally:Stdlib.Sys.remove ~f
    in
    with_temp_file (fun fn1 ->
        with_temp_file (fun fn2 ->
            with_temp_file (fun out ->
                let dump fn ast =
                  Out_channel.with_file fn ~f:(fun oc ->
                      let ppf = Format.formatter_of_out_channel oc in
                      Sexp.pp_hum ppf (M.to_sexp ast);
                      Format.pp_print_flush ppf ())
                in
                dump fn1 generated;
                dump fn2 round_trip;
                let cmd =
                  Printf.sprintf
                    "patdiff -ascii -alt-old generated -alt-new \
                     'generated->printed->parsed' %s %s &> %s"
                    (Filename.quote fn1) (Filename.quote fn2)
                    (Filename.quote out)
                in
                let ok =
                  Stdlib.Sys.command cmd = 1
                  ||
                  let cmd =
                    Printf.sprintf
                      "diff --label generated --label \
                       'generated->printed->parsed' %s %s &> %s"
                      (Filename.quote fn1) (Filename.quote fn2)
                      (Filename.quote out)
                  in
                  Stdlib.Sys.command cmd = 1
                in
                if ok then In_channel.read_all out
                else "<no differences produced by diff>")))

  let parse_string s =
    match M.parse (Lexing.from_string s) with [ x ] -> x | _ -> assert false

  (* To round trip our AST we convert it to the compiler's version, print it as
     source using the compiler pretty-printers, parse it back using the
     compiler's parser and migrate it back to our version.

     Skipping the first migration can lead to errors because some subtleties may
     be lost by older parsers. For instance in OCaml 5.02 [fun x y -> z] and
     [fun x -> fun y -> z] have different representation but in OCaml 5.01 they
     both parse to the same AST. Running the migration to the compiler AST first
     anotates the AST using attributes allowing the final migration to preserve
     such differences. *)
  let round_trip ast =
    let compiler_ast = M.to_compiler ast in
    remove_loc
      (parse_string (Format.asprintf "%a@." M.pp_compiler compiler_ast))

  let rec match_loop ~end_pos ~mismatch_handler ~expected ~source =
    match (expected, source) with
    | [], [] -> ()
    | [], x :: l ->
        let loc =
          { (M.get_loc x) with loc_end = (M.get_loc (last x l)).loc_end }
        in
        mismatch_handler loc []
    | _, [] ->
        let loc =
          { Location.loc_ghost = false; loc_start = end_pos; loc_end = end_pos }
        in
        mismatch_handler loc expected
    | x :: expected, y :: source ->
        let loc = M.get_loc y in
        let x = remove_loc x in
        let y = remove_loc y in
        if Poly.( <> ) x y then (
          let round_trip = round_trip x in
          if Poly.( <> ) x round_trip then
            Location.raise_errorf ~loc
              "ppxlib: the corrected code doesn't round-trip.\n\
               This is probably a bug in the OCaml printer:\n\
               %s"
              (diff_asts ~generated:x ~round_trip);
          mismatch_handler loc [ x ]);
        match_loop ~end_pos ~mismatch_handler ~expected ~source

  let do_match ~pos ~expected ~mismatch_handler source =
    let open Result in
    extract_prefix ~pos source >>| fun (source, end_pos) ->
    match_loop ~end_pos ~mismatch_handler ~expected ~source
end

(*$*)
module Str = Make (struct
  type t = structure_item
  type compiler_t = Ppxlib_ast.Compiler_version.Ast.Parsetree.structure_item

  let get_loc x = x.pstr_loc
  let end_marker = end_marker_str

  module Transform (T : T1) = struct
    let apply o = o#structure_item
  end

  let parse = Parse.implementation
  let to_sexp = Ast_traverse.sexp_of#structure_item
  let to_compiler = Ppxlib_ast.Selected_ast.To_ocaml.copy_structure_item
  let pp_compiler = Astlib.Compiler_pprintast.structure_item
end)

(*$ str_to_sig _last_text_block *)
module Sig = Make (struct
  type t = signature_item
  type compiler_t = Ppxlib_ast.Compiler_version.Ast.Parsetree.signature_item

  let get_loc x = x.psig_loc
  let end_marker = end_marker_sig

  module Transform (T : T1) = struct
    let apply o = o#signature_item
  end

  let parse = Parse.interface
  let to_sexp = Ast_traverse.sexp_of#signature_item
  let to_compiler = Ppxlib_ast.Selected_ast.To_ocaml.copy_signature_item
  let pp_compiler = Astlib.Compiler_pprintast.signature_item
end)

(*$*)

let match_structure_res = Str.do_match

let match_structure ~pos ~expected ~mismatch_handler l =
  match_structure_res ~pos ~expected ~mismatch_handler l
  |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)

let match_signature_res = Sig.do_match

let match_signature ~pos ~expected ~mismatch_handler l =
  match_signature_res ~pos ~expected ~mismatch_handler l
  |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)
