module Ext_name : sig
  val ptyp_labeled_tuple : string
end

module To_503 : sig
  open Ast_503.Parsetree

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list
end

module To_502 : sig
  open Ast_502.Parsetree

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list
end
