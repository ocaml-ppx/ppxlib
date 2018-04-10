open Ppxlib

let _attr : _ Attribute.t =
  Attribute.declare "blah"
    Attribute.Context.type_declaration
    Ast_pattern.(__)
    ignore

let _attr : _ Attribute.t =
  Attribute.declare "bleh"
    Attribute.Context.type_declaration
    Ast_pattern.(__)
    ignore

let _attr : _ Attribute.t =
  Attribute.declare "bleh"
    Attribute.Context.expression
    Ast_pattern.(__)
    ignore


(* Attribute drops *)

let faulty_transformation = object
  inherit Ast_traverse.map as super

  method! expression e =
    match e.pexp_desc with
    | Pexp_constant c ->
      Ast_builder.Default.pexp_constant ~loc:e.pexp_loc c
    | _ -> super#expression e
end

let () =
  Driver.register_transformation "faulty" ~impl:faulty_transformation#structure

let () =
  Driver.standalone ()
