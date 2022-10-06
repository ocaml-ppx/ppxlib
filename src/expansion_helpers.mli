(** Various helpers for expansion. *)

open Import

(** {2 Mangling} *)

(** Derive mangled names from type names in a deriver. *)

type affix =
  [ `Prefix of string  (** [`Prefix p] adds prefix [p]. *)
  | `Suffix of string  (** [`Suffix s] adds suffix [s]. *)
  | `PrefixSuffix of string * string
    (** [`PrefixSuffix (p, s)] adds both prefix [p] and suffix [s]. *) ]
(** Specification for name mangling. *)

val mangle : ?fixpoint:string -> affix -> string -> string
(** [mangle ~fixpoint affix s] derives a mangled name from [s] with the mangling
    specified by [affix]. If [s] is equal to [fixpoint] (["t"] by default), then
    [s] is omitted from the mangled name. *)

val mangle_type_decl : ?fixpoint:string -> affix -> type_declaration -> string
(** [mangle_type_decl ~fixpoint affix td] does the same as {!mangle}, but for
    the name of [td]. *)

val mangle_lid : ?fixpoint:string -> affix -> Longident.t -> Longident.t
(** [mangle_lid ~fixpoint affix lid] does the same as {!mangle}, but for the
    last component of [lid]. *)
