open Ppxlib

let test title fn = Alcotest.test_case title `Quick fn

let assert_string left right =
  Alcotest.check Alcotest.string "should be equal" right left

let sprint_ast_expr =
  let ast = Pp_ast.sprint "42" in
  test "sprint AST expression" @@ fun () ->
  assert_string ast "Pexp_constant (Pconst_integer ( \"42\", None))"

let sprint_ast_pat =
  let ast = Pp_ast.sprint ~kind:Pp_ast.Kind.Pattern "42" in
  test "sprint AST pattern" @@ fun () ->
  assert_string ast "Ppat_constant (Pconst_integer ( \"42\", None))"

let sprint_ast_core_type =
  let ast = Pp_ast.sprint ~kind:Pp_ast.Kind.Core_type "string" in
  test "sprint AST core type" @@ fun () ->
  assert_string ast "Ptyp_constr ( Lident \"string\", [])"

let sprint_ast_sig =
  let ast = Pp_ast.sprint ~kind:Pp_ast.Kind.Signature "val x: int" in
  test "sprint AST signature" @@ fun () ->
  assert_string ast
    "[ Psig_value\n\
    \    { pval_name = \"x\"\n\
    \    ; pval_type = Ptyp_constr ( Lident \"int\", [])\n\
    \    ; pval_prim = []\n\
    \    ; pval_attributes = __attrs\n\
    \    ; pval_loc = __loc\n\
    \    }\n\
     ]"

let sprint_ast_str =
  let ast = Pp_ast.sprint ~kind:Pp_ast.Kind.Structure "let x = 42" in
  test "sprint AST structure" @@ fun () ->
  assert_string ast
    "[ Pstr_value\n\
    \    ( Nonrecursive\n\
    \    , [ { pvb_pat = Ppat_var \"x\"\n\
    \        ; pvb_expr = Pexp_constant (Pconst_integer ( \"42\", None))\n\
    \        ; pvb_attributes = __attrs\n\
    \        ; pvb_loc = __loc\n\
    \        }\n\
    \      ]\n\
    \    )\n\
     ]"

let () =
  Alcotest.run ~show_errors:true ~compact:true ~tail_errors:`Unlimited "ppxlib"
    [
      ( "Pprintast",
        [
          sprint_ast_expr;
          sprint_ast_pat;
          sprint_ast_str;
          sprint_ast_sig;
          sprint_ast_core_type;
        ] );
    ]
