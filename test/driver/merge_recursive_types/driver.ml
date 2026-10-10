open Ppxlib
module List = ListLabels
open Ast_builder.Default

let generate_impl ~ctxt (_rec_flag, type_declarations) prefix =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let make_name t = match prefix with Some p -> p ^ t | None -> t in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      let name = td.ptype_name in
      let pat = ppat_var ~loc { name with loc } in
      let expr = estring ~loc (make_name name.txt) in
      let binding = value_binding ~pat ~expr ~loc in
      let str = pstr_value ~loc Nonrecursive [ binding ] in
      [ str ])
  |> List.concat

let prefix =
  let pattern = Ast_pattern.(estring __) in
  Deriving.Args.arg "prefix" pattern

let impl_generator =
  Deriving.Generator.V2.make Deriving.Args.(empty +> prefix) generate_impl

let my_deriver = Deriving.add "type_name" ~str_type_decl:impl_generator

let () =
  ignore my_deriver;
  Driver.standalone ()
