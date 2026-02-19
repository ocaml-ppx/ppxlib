module Ext_name = struct
  let pexp_struct_item = "ppxlib.migration.pexp_struct_item_505"
  let ptyp_functor = "ppxlib.migration.ptyp_functor_505"
  let ppat_unpack = "ppxlib.migration.ppat_unpack_505"
  let ptype_kind_external = "ppxlib.migration.ptype_kind_external_505"
end

let invalid_encoding ~loc name =
  Location.raise_errorf ~loc "Invalid %s encoding" name

module To_504 = struct
  open Ast_504.Asttypes
  open Ast_504.Parsetree

  let encode_pexp_struct_item ~loc ((si, e) : structure_item * expression) =
    let expr_as_structure_item =
      let pstr_desc = Pstr_eval (e, []) in
      { pstr_desc; pstr_loc = loc }
    in
    let payload =
      let items = [ si; expr_as_structure_item ] in
      PStr items
    in
    let name = { txt = Ext_name.pexp_struct_item; loc } in
    Pexp_extension (name, payload)

  let decode_pexp_struct_item ~loc payload =
    match payload with
    | PStr [ si; { pstr_desc = Pstr_eval (e, []); _ } ] -> (si, e)
    | _ -> invalid_encoding ~loc Ext_name.pexp_struct_item

  let mk_core_type ?(ptyp_attributes = []) ptyp_desc =
    {
      ptyp_desc;
      ptyp_loc = Location.none;
      ptyp_loc_stack = [];
      ptyp_attributes;
    }

  let encode_ptyp_functor ~loc
      ((arg, name, pkg, typ) :
        arg_label * string loc * package_type * core_type) =
    let as_type =
      let pkg = mk_core_type (Ptyp_package pkg) in
      let attr = { attr_name = name; attr_payload = PStr []; attr_loc = loc } in
      mk_core_type ~ptyp_attributes:[ attr ] (Ptyp_arrow (arg, pkg, typ))
    in
    let payload = PTyp as_type in
    let name = { txt = Ext_name.ptyp_functor; loc } in
    Ptyp_extension (name, payload)

  let decode_ptyp_functor ~loc payload =
    match payload with
    | PTyp
        {
          ptyp_desc = Ptyp_arrow (arg, { ptyp_desc = Ptyp_package pkg }, typ);
          ptyp_attributes = [ attr ];
        } ->
        (arg, attr.attr_name, pkg, typ)
    | _ -> invalid_encoding ~loc Ext_name.ptyp_functor

  let encode_ptype_kind_external name =
    let name_attr =
      {
        attr_name = { txt = name; loc = Location.none };
        attr_loc = Location.none;
        attr_payload = PStr [];
      }
    in
    let si =
      { pstr_desc = Pstr_attribute name_attr; pstr_loc = Location.none }
    in
    {
      attr_name = { txt = Ext_name.ptype_kind_external; loc = Location.none };
      attr_loc = Location.none;
      attr_payload = PStr [ si ];
    }

  let decode_ptype_kind_external = function
    | {
        attr_name = { txt; _ };
        attr_payload =
          PStr
            [
              {
                pstr_desc = Pstr_attribute { attr_name = { txt = name; _ }; _ };
              };
            ];
      }
      when String.equal txt Ext_name.ptype_kind_external ->
        Some name
    | _ -> None
end
