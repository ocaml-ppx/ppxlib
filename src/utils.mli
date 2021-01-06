open Import

val with_output : label option -> binary:bool -> f:(out_channel -> 'a) -> 'a

module Kind : sig
  type t = Intf | Impl

  val of_filename : string -> t option

  val describe : t -> string

  val equal : t -> t -> bool
end

module Ast_io : sig
  type t

  type read_error =
  | Not_a_binary_ast of string
  (* The input doesn't contain a binary AST. The argument
      corresponds to the bytes from the input that were consumed. *)
  | Unknown_version of string
  (* The input contains a binary AST for an unknown version of
      OCaml.  The argument is the unknown magic number. *)

  val read : in_channel -> (string * t, read_error) result

  val write : out_channel -> string -> t -> unit
end

module Intf_or_impl  : sig
  type t =
  | Intf of signature
  | Impl of structure

  val map : t -> Ast_traverse.map -> t

  val map_with_context : t -> 'a Ast_traverse.map_with_context -> 'a -> t

  val kind : t -> Kind.t

  val of_ast_io : Ast_io.t -> t

  val to_ast_io : t -> add_ppx_context:bool -> Ast_io.t
end
