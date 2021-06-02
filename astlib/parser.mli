type token = Ocaml_common.Parser.token

val use_file :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase list

val toplevel_phrase :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase

val parse_pattern :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.pattern

val parse_expression :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.expression

val parse_core_type :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.core_type

val interface :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.signature_item list

val implementation :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.structure_item list
