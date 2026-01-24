module Ext_name = struct
  let pexp_struct_item = "ppxlib.migration.pexp_struct_item_505"
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
end
