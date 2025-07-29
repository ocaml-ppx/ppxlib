open Ppxlib

let loc = Location.none

let ast =
  let vbs =
    let pat =
      Ast_builder.Default.ppat_constraint ~loc
        [%pat? f]
        (Ast_builder.Default.ptyp_poly ~loc
           [ Loc.make ~loc "a" ]
           [%type: 'a -> unit])
    in
    let expr = [%expr fun _ -> ()] in
    [ Ast_builder.Default.Latest.value_binding ~loc ~pat ~expr () ]
  in
  Ast_builder.Default.pstr_value ~loc Nonrecursive vbs

let () = Format.printf "%a\n" Pprintast.structure_item ast
