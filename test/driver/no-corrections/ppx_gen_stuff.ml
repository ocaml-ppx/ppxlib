open Ppxlib

let attr = Attribute.declare_flag "gen_stuff" Attribute.Context.type_declaration

let expand ~ctxt _rec_flag _type_decl _values =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [%str let stuff = 4]

let rules = [ Context_free.Rule.attr_str_type_decl_expect attr expand ]
let () = Driver.V2.register_transformation ~rules "gen_stuff"
