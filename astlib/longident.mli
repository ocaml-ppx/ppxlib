(** Long identifiers, used in parsetrees. *)
open Location

(** The long identifier type *)
type t = (*IF_AT_LEAST 504 Ocaml_common.Longident.t = *)
  | Lident of string
  | Ldot of t loc * string loc
  | Lapply of t loc * t loc

val flatten : t -> string list
(** Flatten a long identifier built upon [Lident] and [Ldot]. Raise when hitting
    [Lapply].*)

val parse : string -> t
(** Parse a string into a long identifier built upon [Lident] and [Ldot]. *)
