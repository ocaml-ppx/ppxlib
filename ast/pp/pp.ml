let () =
  match Sys.argv with
  | [| _; ocaml_version_str; fname |] ->
      (* The 414 and the 500 AST coincide.
         So, to avoid unnecessary AST migrations, we re-use the 414 AST for 500. *)
      let ocaml_version_str =
        match String.split_on_char '.' ocaml_version_str with
        | "5" :: "00" :: _ -> "4.14.0"
        | _ -> ocaml_version_str
      in
      let ocaml_version =
        match Supported_version.of_string ocaml_version_str with
        | Some v -> string_of_int (Supported_version.to_int v)
        | None ->
            Printf.eprintf "Unknown OCaml version %s\n" ocaml_version_str;
            exit 1
      in
      let ic = open_in_bin fname in
      Printf.printf "# 1 %S\n" fname;
      Pp_rewrite.rewrite ocaml_version (Lexing.from_channel ic)
  | _ ->
      Printf.eprintf "%s: <ocaml-version> <file-name>\n" Sys.executable_name;
      exit 2
