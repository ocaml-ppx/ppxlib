open Ppxlib

let loc = Location.none

let zero ~loc : Ppxlib_ast.Ast.expression =
  {
    pexp_desc =
      Pexp_constant
        { pconst_desc = Pconst_integer ("0", None); pconst_loc = loc };
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let _ =
  print_endline
    ("\nAST with AST pure tree build: "
    ^ Astlib.Pprintast.string_of_expression (zero ~loc))

let one ~loc =
  Ast_builder.Default.pexp_constant ~loc
    { pconst_desc = Parsetree.Pconst_integer ("1", None); pconst_loc = loc }

let _ =
  print_endline
    ("\nAST with AST build pexp_constant: "
    ^ Astlib.Pprintast.string_of_expression (one ~loc))

let two ~loc = Ast_builder.Default.eint ~loc 2

let _ =
  print_endline
    ("\nAST with AST build eint: "
    ^ Astlib.Pprintast.string_of_expression (two ~loc))

let three ~loc = [%expr 3]

let _ =
  print_endline
    ("\nAST with AST build eint: "
    ^ Astlib.Pprintast.string_of_expression (three ~loc))

let let_expression =
  let expression =
    Ast_builder.Default.pexp_constant ~loc:Location.none
      { pconst_desc = Parsetree.Pconst_integer ("3", None); pconst_loc = loc }
  in
  let pattern =
    Ast_builder.Default.ppat_var ~loc:Location.none
      (Ast_builder.Default.Located.mk ~loc:Location.none "foo")
  in
  let let_binding =
    Ast_builder.Default.value_binding ~loc:Location.none ~pat:pattern
      ~expr:expression
  in

  Ast_builder.Default.pexp_let ~loc:Location.none Nonrecursive [ let_binding ]
    (Ast_builder.Default.eunit ~loc:Location.none)

let _ =
  print_endline
    ("\nLet expression with Ast_builder: "
    ^ Astlib.Pprintast.string_of_expression let_expression)

let let_expression =
  [%expr
    let foo = 3 in
    ()]

let _ =
  print_endline
    ("\nLet expression with metaquot: "
    ^ Astlib.Pprintast.string_of_expression let_expression)

let anti_quotation_expr expr = [%expr 1 + [%e expr]]

let _ =
  print_endline
    ("\nLet expression with metaquot and anti-quotation: "
    ^ Astlib.Pprintast.string_of_expression (anti_quotation_expr (one ~loc)))
