module Ext_name : sig
  val ptyp_labeled_tuple : string
  val pexp_labeled_tuple : string
end

module To_503 : sig
  open Ast_503.Parsetree

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list

  val encode_pexp_labeled_tuple :
    loc:Location.t -> (string option * expression) list -> expression_desc

  val decode_pexp_labeled_tuple :
    loc:Location.t -> payload -> (string option * expression) list
end

module To_502 : sig
  open Ast_502.Parsetree

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list

  val encode_pexp_labeled_tuple :
    loc:Location.t -> (string option * expression) list -> expression_desc

  val decode_pexp_labeled_tuple :
    loc:Location.t -> payload -> (string option * expression) list
end
