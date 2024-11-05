val is_keyword : string -> bool
(** Check if a string is an OCaml keyword. *)

val apply_keyword_edition : unit -> unit
(** Processes any keywords= sections from the OCAMLPARAM environment variable
    and initialises the compiler's lexer with the correct keyword set. *)
