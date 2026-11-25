open Ppxlib

let plus_one_str str =
  let i = int_of_string str in
  string_of_int (i + 1)

let t =
  object
    inherit Ast_traverse.map as super

    method! expression_desc ed =
      match ed with
      | Pexp_constant (Pconst_integer (s, None)) ->
          Pexp_constant (Pconst_integer (plus_one_str s, None))
      | _ -> super#expression_desc ed
  end

let impl str = t#structure str

let () =
  Driver.register_transformation ~impl "ppx_plus_one";
  Driver.standalone ()
