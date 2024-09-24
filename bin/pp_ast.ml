open Ppxlib

module Input = struct
  type t = Stdin | File of string | Source of string

  let to_lexbuf t =
    let all_source =
      match t with
      | Stdin -> Stdppx.In_channel.input_all stdin
      | File fn -> Stdppx.In_channel.(with_file fn ~f:input_all)
      | Source s -> s
    in
    Lexing.from_string all_source

  let from_string = function
    | "-" -> Stdin
    | s when Sys.file_exists s -> File s
    | s -> Source s

  let to_driver_fn = function
    | Stdin -> "-"
    | File fn -> fn
    | Source _ -> assert false
end

let load_input ~kind ~input_name input =
  match ((kind : Pp_ast.Kind.t), (input : Input.t)) with
  | (Structure | Signature), (Stdin | File _) -> (
      let kind =
        match kind with
        | Pp_ast.Kind.Structure -> Ppxlib_private.Utils.Kind.Impl
        | Signature -> Ppxlib_private.Utils.Kind.Intf
        | _ -> assert false
      in
      let fn = Input.to_driver_fn input in
      match Driver.load_input ~kind ~input_name ~relocate:false fn with
      | Error (loc_err, _ver) -> Location.Error.raise loc_err
      | Ok (_ast_input_name, _version, ast) -> (
          match (ast : Ppxlib_private.Utils.Intf_or_impl.t) with
          | Impl str -> Pp_ast.Ast.Str str
          | Intf sig_ -> Pp_ast.Ast.Sig sig_))
  | (Expression | Pattern | Core_type), _ | _, Source _ ->
      Pp_ast.parse_node ~kind (input |> Input.to_lexbuf)

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
  let make_vflag (flag, (kind : Pp_ast.Kind.t), doc) =
    (Some kind, Cmdliner.Arg.info ~doc [ flag ])
  in
  let kinds =
    List.map make_vflag
      [
        ("str", Pp_ast.Kind.Structure, "Treat the input as a $(b,.ml) file");
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
    "The $(docv) AST. Can be a binary AST file, a source file or a valid OCaml \
     source string. Pass $(b,-) to read from stdin instead."
  in
  named
    (fun x -> `Input x)
    Cmdliner.Arg.(required & pos 0 (some string) None & info ~doc ~docv [])

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run (`Show_attrs show_attrs) (`Show_locs show_locs) (`Loc_mode loc_mode)
    (`Kind kind) (`Input input) =
  let open Stdppx.Result in
  let kind =
    match kind with
    | Some k -> Ok k
    | None -> (
        match Ppxlib_private.Utils.Kind.of_filename input with
        | Some Intf -> Ok Pp_ast.Kind.Signature
        | Some Impl -> Ok Pp_ast.Kind.Structure
        | None ->
            errorf
              "Could not guess kind from input %S. Please use relevant CLI \
               flag."
              input)
  in
  kind >>= fun kind ->
  let input = Input.from_string input in
  let input_name =
    match input with Stdin -> "<stdin>" | File fn -> fn | Source _ -> "<cli>"
  in
  let ast = load_input ~kind ~input_name input in
  let config = Pp_ast.Config.make ~show_attrs ~show_locs ~loc_mode () in
  Pp_ast.pp_ast ~config ast Format.std_formatter;
  Format.printf "%!\n";
  Ok ()

let term =
  Cmdliner.Term.(const run $ show_attrs $ show_locs $ loc_mode $ kind $ input)

let tool_name = "ppxlib-pp-ast"

let info =
  let p fmt = Printf.ksprintf (fun s -> `P s) fmt in
  let open Cmdliner in
  Cmd.info tool_name ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"Pretty prints ppxlib's ASTs from OCaml sources"
    ~man:
      [
        `S "DESCRIPTION";
        p
          "$(b,%s) can be used to pretty print the OCaml AST for a given piece \
           of source code."
          tool_name;
        `P
          "It prints the AST version used by ppxlib internally. This means the \
           code will be parsed (or the AST unmarshalled) using the installed \
           compiler and eventually migrated to ppxlib's expected version.";
        `P
          "It can read the source code directly from the command line or read \
           it from a file/the standard input. In the latter case you can pass \
           regular $(b,.ml) or $(b,.mli) files, marshalled AST files as the \
           ones produced by the ppxlib driver or a fragment of OCaml source \
           corresponding to an expression, a pattern or a core_type.";
        `P
          "When the input is not an $(b,.ml) or $(b,.mli) file you will have \
           to explicitly pass the expected AST node using flags such as \
           $(b,--str) or $(b,--exp).";
        `P "By default the output looks like this:";
        p "$(b,\\$ %s --exp \"x + 2\")" tool_name;
        `Noblank;
        `Pre
          (String.concat "\n"
             [
               {|Pexp_apply|};
               {|  ( Pexp_ident (Lident "+")|};
               {|  , [ ( Nolabel, Pexp_ident (Lident "x"))|};
               {|    ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))|};
               {|    ]|};
               {|  )|};
             ]);
        `P
          "If you are already familiar with the OCaml AST you will note that \
           it prints a somewhat lighter version to keep the output concise. \
           Locations and attributes are not printed. Some specific record \
           types, such as $(b,expression) or $(b,pattern), are \"skipped\" to \
           avoid too much nesting. This does not hurt comprehension of the \
           underlying AST as these records only wrap a variant type to attach \
           metadata to it. The tool supports a set of flags you can use to \
           force printing of such metadata.";
        `S "EXAMPLES";
        p "$(b,%s test.ml)" tool_name;
        p "$(b,cat test.ml | %s -)" tool_name;
        p "$(b,%s test.pp.ml)" tool_name;
        p "$(b,%s --exp \"x + 2\")" tool_name;
        p "$(b,%s --typ \"(int, string\\) Result.t\")" tool_name;
        p "$(b,%s --show-locs --full-locs --pat \"_::tl\")" tool_name;
      ]

let () =
  let exit_code = Cmdliner.Cmd.eval_result (Cmdliner.Cmd.v info term) in
  exit exit_code
