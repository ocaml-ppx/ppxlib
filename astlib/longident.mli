type t = Ocaml_common.Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten : t -> string list

val parse : string -> t
