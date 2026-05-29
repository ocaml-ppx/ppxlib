open Ppxlib

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression_desc ed =
      match ed with
      | Pexp_extension ({ txt = "exp"; loc }, PStr []) ->
          Pexp_ident { txt = Lident "a"; loc }
      | _ -> super#expression_desc ed
  end

let impl str = mapper#structure str
let () = Driver.register_transformation ~impl "exp_to_a"
