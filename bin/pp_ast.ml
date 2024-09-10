open Ppxlib

module Kind = struct
  type t = Signature | Structure | Expression | Pattern | Core_type

  let to_utils_kind = function
    | Structure -> Ppxlib_private.Utils.Kind.Impl
    | Signature -> Ppxlib_private.Utils.Kind.Intf
    | _ -> assert false
end

module Ast = struct
  type t =
    | Str of structure
    | Sig of signature
    | Exp of expression
    | Pat of pattern
    | Typ of core_type
end

let parse_node ~kind ~input_name fn =
  let all_source =
    match fn with
    | "-" -> Stdppx.In_channel.input_all stdin
    | _ -> Stdppx.In_channel.(with_file fn ~f:input_all)
  in
  let lexbuf = Lexing.from_string all_source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_name };
  Astlib.Location.set_input_lexbuf (Some lexbuf);
  match (kind : Kind.t) with
  | Expression -> Ast.Exp (Parse.expression lexbuf)
  | Pattern -> Ast.Pat (Parse.pattern lexbuf)
  | Core_type -> Ast.Typ (Parse.core_type lexbuf)
  | Signature | Structure -> assert false

let load_input ~kind ~input_name fn =
  match (kind : Kind.t) with
  | Structure | Signature -> (
      let kind = Kind.to_utils_kind kind in
      match Driver.load_input ~kind ~input_name ~relocate:false fn with
      | Error (loc_err, _ver) -> Location.Error.raise loc_err
      | Ok (_ast_input_name, _version, ast) -> (
          match (ast : Ppxlib_private.Utils.Intf_or_impl.t) with
          | Impl str -> Ast.Str str
          | Intf sig_ -> Ast.Sig sig_))
  | Expression | Pattern | Core_type -> parse_node ~kind ~input_name fn

let pp_ast ~config ast =
  match (ast : Ast.t) with
  | Str str -> Pp_ast.structure ~config Format.std_formatter str
  | Sig sig_ -> Pp_ast.signature ~config Format.std_formatter sig_
  | Exp exp -> Pp_ast.expression ~config Format.std_formatter exp
  | Pat pat -> Pp_ast.pattern ~config Format.std_formatter pat
  | Typ typ -> Pp_ast.core_type ~config Format.std_formatter typ

let named f = Cmdliner.Term.(app (const f))

let show_attrs =
  let doc = "Show atributes in the pretty printed output" in
  named
    (fun x -> `Show_attrs x)
    Cmdliner.Arg.(value & flag & info ~doc [ "show-attrs" ])

let show_locs =
  let doc = "Show locations in the pretty printed output" in
  named
    (fun x -> `Show_locs x)
    Cmdliner.Arg.(value & flag & info ~doc [ "show-locs" ])

let loc_mode =
  let full_locs =
    let doc =
      "Display locations in long form. Has no effect without --show-locs."
    in
    (`Full, Cmdliner.Arg.info ~doc [ "full-locs" ])
  in
  named (fun x -> `Loc_mode x) Cmdliner.Arg.(value & vflag `Short [ full_locs ])

let kind =
  let make_vflag (flag, (kind : Kind.t), doc) =
    (Some kind, Cmdliner.Arg.info ~doc [ flag ])
  in
  let kinds =
    List.map make_vflag
      [
        ("str", Structure, "Treat the input as a $(b,.ml) file");
        ("sig", Signature, "Treat the input as a $(b,.mli) file");
        ("exp", Expression, "Treat the input as a single OCaml expression");
        ("pat", Pattern, "Treat the input as a single OCaml pattern");
        ("typ", Core_type, "Treat the input as a single OCaml core_type");
      ]
  in
  named (fun x -> `Kind x) Cmdliner.Arg.(value & vflag None kinds)

let input =
  let docv = "INPUT" in
  let doc =
    "The $(docv) AST. Can be a binary AST file or a source file. Pass $(b,-) \
     to read from stdin instead."
  in
  named
    (fun x -> `Input x)
    Cmdliner.Arg.(required & pos 0 (some string) None & info ~doc ~docv [])

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run (`Show_attrs show_attrs) (`Show_locs show_locs) (`Loc_mode loc_mode)
    (`Kind kind) (`Input fn) =
  let open Stdppx.Result in
  let kind =
    match kind with
    | Some k -> Ok k
    | None -> (
        match Ppxlib_private.Utils.Kind.of_filename fn with
        | Some Intf -> Ok Kind.Signature
        | Some Impl -> Ok Kind.Structure
        | None ->
            errorf
              "Could not guess kind from input %S\n\
              \ Please use relevant CLI flag" fn)
  in
  kind >>= fun kind ->
  let input_name = match fn with "-" -> "<stdin>" | _ -> fn in
  let ast = load_input ~kind ~input_name fn in
  let config = Pp_ast.Config.make ~show_attrs ~show_locs ~loc_mode () in
  pp_ast ~config ast;
  Format.printf "%!\n";
  Ok ()

let tool_name = "ppxlib-pp-ast"

let info =
  let open Cmdliner in
  Cmd.info tool_name ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"Pretty prints ppxlib's versioned ASTs from OCaml sources"

let term =
  Cmdliner.Term.(const run $ show_attrs $ show_locs $ loc_mode $ kind $ input)

let () =
  let exit_code = Cmdliner.Cmd.eval_result (Cmdliner.Cmd.v info term) in
  exit exit_code
