module Ext_name : sig
  val pexp_struct_item : string
  val ptyp_functor : string
  val preserve_ppat_constraint : string
  val ptype_kind_external : string
end

module To_504 : sig
  open Ast_504.Asttypes
  open Ast_504.Parsetree

  val encode_pexp_struct_item :
    loc:Location.t -> structure_item * expression -> expression_desc

  val decode_pexp_struct_item :
    loc:Location.t -> payload -> structure_item * expression

  val encode_ptyp_functor :
    loc:Location.t ->
    arg_label * string loc * package_type * core_type ->
    core_type_desc

  val decode_ptyp_functor :
    loc:Location.t ->
    payload ->
    arg_label * string loc * package_type * core_type

  val encode_ptype_kind_external : string -> attribute
  val decode_ptype_kind_external : attribute -> string option

  val must_preserve_ppat_constraint : attributes -> attributes option
  (** Returns [None] if the list does not contain
      [@ppxlib.migration.preserve_ppat_constraint_505] or [Some l] where [l] is
      the remainder of the attributes. Should be used to determine whether a
      [Ppat_constraint (Ppat_unpack m, Ptyp_package s)] node should be preserved
      when migrated to 5.5 or turned into [Ppat_unpack (m, Some s)]. The
      attribute should be attached to the [Ppat_unpack] node within the
      [Ppat_constraint]. *)

  val preserve_ppat_constraint : pattern -> core_type -> pattern_desc
end
