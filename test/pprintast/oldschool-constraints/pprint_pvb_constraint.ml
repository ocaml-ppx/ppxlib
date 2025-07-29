open Ppxlib

let loc = Location.none

let ast =
  let vbs =
    let pat = [%pat? f] in
    let expr = [%expr ()] in
    let constraint_ =
      Pvc_constraint
        {
          locally_abstract_univars = [];
          typ =
            Ast_builder.Default.ptyp_poly ~loc
              [ Loc.make ~loc "a" ]
              [%type: 'a -> unit];
        }
    in
    [ Ast_builder.Default.Latest.value_binding ~loc ~pat ~expr ~constraint_ () ]
  in
  Ast_builder.Default.pstr_value ~loc Nonrecursive vbs

let () = Format.printf "%a\n" Pprintast.structure_item ast
