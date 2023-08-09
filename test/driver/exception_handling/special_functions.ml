open Ppxlib

let expand e =
  let loc = e.pexp_loc in
  Location.raise_errorf ~loc "This is an example error"

let rule = Context_free.Rule.special_function "n_args" expand

let () =
  Driver.register_transformation ~rules:[ rule ] "special_function_demo";
  Driver.standalone ()
