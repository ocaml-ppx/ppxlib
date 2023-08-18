open Ppxlib

let expand e =
  let return n = Some (Ast_builder.Default.eint ~loc:e.pexp_loc n) in
  match e.pexp_desc with
  | Pexp_apply (_, _arg_list) -> Location.raise_errorf "error special function"
  | _ -> return 0

let expand2 e =
  let return n = Some (Ast_builder.Default.eint ~loc:e.pexp_loc n) in
  match e.pexp_desc with
  | Pexp_apply (_, _arg_list2) ->
      Location.raise_errorf "error special function 2"
  | _ -> return 0

let rule = Context_free.Rule.special_function "n_args" expand
let rule2 = Context_free.Rule.special_function "n_args2" expand2;;

Driver.register_transformation ~rules:[ rule; rule2 ] "special_function_demo"

let () = Driver.standalone ()
