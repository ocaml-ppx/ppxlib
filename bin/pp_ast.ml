open Ppxlib

module Kind = struct
  type t = Signature | Structure | Expression | Pattern | Core_type

  let to_utils_kind = function
    | Structure -> Ppxlib__Utils.Kind.Impl
    | Signature -> Ppxlib__Utils.Kind.Intf
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
          match (ast : Ppxlib__Utils.Intf_or_impl.t) with
          | Impl str -> Ast.Str str
          | Intf sig_ -> Ast.Sig sig_))
  | Expression | Pattern | Core_type -> parse_node ~kind ~input_name fn

let pp_ast ast =
  match (ast : Ast.t) with
  | Str str -> Pp_ast.structure Format.std_formatter str
  | Sig sig_ -> Pp_ast.signature Format.std_formatter sig_
  | Exp exp -> Pp_ast.expression Format.std_formatter exp
  | Pat pat -> Pp_ast.pattern Format.std_formatter pat
  | Typ typ -> Pp_ast.core_type Format.std_formatter typ

let input = ref None
let kind = ref None

let set_input fn =
  match !input with
  | None -> input := Some fn
  | Some _ -> raise (Arg.Bad "too many input files")

let set_kind k =
  match !kind with
  | Some _ -> raise (Arg.Bad "must specify at most one of --impl or --intf")
  | _ -> kind := Some k

let exe_name = Stdlib.Filename.basename Stdlib.Sys.executable_name

let args =
  [
    ("-", Arg.Unit (fun () -> set_input "-"), " Read input from stdin");
    ( "--str",
      Arg.Unit (fun () -> set_kind Kind.Structure),
      "<file> Treat the input as a .ml file" );
    ( "--sig",
      Arg.Unit (fun () -> set_kind Kind.Signature),
      "<file> Treat the input as a .mli file" );
    ( "--exp",
      Arg.Unit (fun () -> set_kind Kind.Expression),
      "<file> Treat the input as a single OCaml expression" );
    ( "--pat",
      Arg.Unit (fun () -> set_kind Kind.Pattern),
      "<file> Treat the input as a single OCaml pattern" );
    ( "--typ",
      Arg.Unit (fun () -> set_kind Kind.Core_type),
      "<file> Treat the input as a single OCaml core_type" );
  ]

let main () =
  let usage = Printf.sprintf "%s [extra_args] [<file>/-]" exe_name in
  Arg.parse (Arg.align args) set_input usage;
  match !input with
  | None ->
      Printf.eprintf "%s: no input file given\n%!" exe_name;
      Stdlib.exit 2
  | Some fn ->
      let kind =
        match !kind with
        | Some k -> k
        | None -> (
            match Ppxlib__Utils.Kind.of_filename fn with
            | Some Intf -> Signature
            | Some Impl -> Structure
            | None ->
                Printf.eprintf
                  "%s: Could not guess kind from filename %S\n\
                  \ Please use relevant CLI flag" exe_name fn;
                Stdlib.exit 2)
      in
      let input_name = match fn with "-" -> "<stdin>" | _ -> fn in
      let ast = load_input ~kind ~input_name fn in
      pp_ast ast;
      Format.printf "%!\n"

let () = main ()
