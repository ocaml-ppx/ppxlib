let () =
  match Sys.argv with
  | [|_; ocaml_version_str; fname|] ->
    let ocaml_version =
      (match Scanf.sscanf ocaml_version_str "%u.%u" (fun a b -> (a, b)) with
       | (4, 02) -> "402"
       | (4, 03) -> "403"
       | (4, 04) -> "404"
       | (4, 05) -> "405"
       | (4, 06) -> "406"
       | (4, 07) -> "407"
       | (4, 08) -> "408"
       | (4, 09) -> "409"
       | (4, 10) -> "410"
       | (4, 11) -> "411"
       | _ ->
         Printf.eprintf "Unknown OCaml version %s\n" ocaml_version_str;
         exit 1)
    in
    let is_current =
      (Filename.basename fname = Printf.sprintf "ast_%s.ml" ocaml_version)
    in
    let ic = open_in_bin fname in
    Printf.printf "# 1 %S\n" fname;
    Pp_rewrite.rewrite is_current ocaml_version (Lexing.from_channel ic)
  | _ ->
    Printf.eprintf "%s: <ocaml-version> <file-name>\n"
      Sys.executable_name;
    exit 2
