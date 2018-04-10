open Ppxlib;;
open Ast_builder.Default;;

Driver.register_transformation "blah"
  ~rules:[ Context_free.Rule.extension
             (Extension.declare "foo"
                Expression
                Ast_pattern.(pstr nil)
                (fun ~loc ~path:_ -> eint ~loc 42))
         ; Context_free.Rule.extension
             (Extension.declare "@foo.bar"
                Expression
                Ast_pattern.(pstr nil)
                (fun ~loc ~path:_ -> eint ~loc 42))
         ]
;;

let () =
  Driver.standalone ()
