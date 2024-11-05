let is_keyword = function
  | "and" -> true
  | "as" -> true
  | "assert" -> true
  | "begin" -> true
  | "class" -> true
  | "constraint" -> true
  | "do" -> true
  | "done" -> true
  | "downto" -> true
  | "else" -> true
  | "end" -> true
  | "exception" -> true
  | "external" -> true
  | "false" -> true
  | "for" -> true
  | "fun" -> true
  | "function" -> true
  | "functor" -> true
  | "if" -> true
  | "in" -> true
  | "include" -> true
  | "inherit" -> true
  | "initializer" -> true
  | "lazy" -> true
  | "let" -> true
  | "match" -> true
  | "method" -> true
  | "module" -> true
  | "mutable" -> true
  | "new" -> true
  | "nonrec" -> true
  | "object" -> true
  | "of" -> true
  | "open" -> true
  | "or" -> true
  (* | "parser" -> true *)
  | "private" -> true
  | "rec" -> true
  | "sig" -> true
  | "struct" -> true
  | "then" -> true
  | "to" -> true
  | "true" -> true
  | "try" -> true
  | "type" -> true
  | "val" -> true
  | "virtual" -> true
  | "when" -> true
  | "while" -> true
  | "with" -> true
  | "lor" -> true
  | "lxor" -> true
  | "mod" -> true
  | "land" -> true
  | "lsl" -> true
  | "lsr" -> true
  | "asr" -> true
  | _ -> false

let apply_keyword_edition () =
  match Sys.getenv "OCAMLPARAM" with
  | s ->
      let items =
        if String.equal s "" then []
        else
          (* cf. Compenv.parse_args *)
          match s.[0] with
          | (':' | '|' | ';' | ' ' | ',') as c ->
              List.tl (String.split_on_char c s)
          | _ -> String.split_on_char ',' s
      in
      let fold_settings acc item =
        let len = String.length item in
        if len >= 9 && String.sub item 0 9 = "keywords=" then
          Some (String.sub item 9 (len - 9))
        else acc
      in
      let keyword_edition = List.fold_left fold_settings None items in
      (*IF_AT_LEAST 503 let () = if Option.is_some keyword_edition then Clflags.keyword_edition := keyword_edition in*)
      (*IF_NOT_AT_LEAST 503 let () = ignore keyword_edition in*)
      ()
  | exception Not_found -> ()
