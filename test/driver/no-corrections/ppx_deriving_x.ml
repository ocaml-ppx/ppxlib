open Ppxlib

let str_type_decl =
  Deriving.Generator.V2.make_noarg (fun ~ctxt _type_decl ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      [%str let x = 2])

let _ = Deriving.add ~str_type_decl "x"
