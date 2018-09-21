(** Overrides the Longident module of OCaml *)

open! Import

type t = longident =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

include Comparable.S with type t := t

val flatten_exn : t -> string list
val last_exn : t -> string
val parse : string -> t

(** [parse_with_operator] handles correctly dotted operators
    ([+.+]) that [parse] parses to [Ldot(+,Lident +)] *)
val parse_with_operator: string -> t
val name : t -> string
