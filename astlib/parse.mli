(** Entry points in the parser *)

val implementation : Lexing.lexbuf -> Parsetree.structure_item list

val interface : Lexing.lexbuf -> Parsetree.signature_item list

val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase

val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list

val core_type : Lexing.lexbuf -> Parsetree.core_type

val expression : Lexing.lexbuf -> Parsetree.expression

val pattern : Lexing.lexbuf -> Parsetree.pattern
