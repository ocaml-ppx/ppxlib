open Ppxlib

let rule =
  Context_free.Rule.extension
    (Extension.V3.declare "iam1" Extension.Context.expression
       Ast_pattern.(pstr nil)
       (fun ~ctxt ->
         let loc = Expansion_context.Extension.extension_point_loc ctxt in
         [%expr 1]))

let () = Driver.register_transformation ~rules:[ rule ] "iam1"

let () =
  Driver.register_transformation ~impl:(fun str -> str) "IdentityInDisguise"

let () = Driver.standalone ()
