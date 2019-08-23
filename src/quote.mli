open Import

type t

val create : unit -> t

val sanitize : t -> expression -> expression

val quote : t -> expression -> expression
