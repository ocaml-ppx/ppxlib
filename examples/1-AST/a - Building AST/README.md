# Building AST

<small>:link: [Docs](https://ocaml-ppx.github.io/ppxlib/ppxlib/generating-code.html)</small>

This section has code examples to help you understand it better.  
To run the examples:

```sh
make demo-building_ast
```

### Table of Contents

- [Description](#description)
- [Building ASTs with Pure OCaml](#building-asts-with-pure-ocaml)
  - [Example: Building a Simple Integer AST Manually](#example-building-a-simple-integer-ast-manually)
- [Building ASTs with `AST_builder`](#building-asts-with-ast_builder)
  - [Example 1: Using `pexp_constant` for Integer AST](#example-1-using-pexp_constant-for-integer-ast)
  - [Example 2: Using `eint` for Simplified Integer AST](#example-2-using-eint-for-simplified-integer-ast)
- [Using Metaquot for AST Construction](#using-metaquot-for-ast-construction)
  - [Example: Building an Integer AST with Metaquot](#example-building-an-integer-ast-with-metaquot)
  - [Using Anti-Quotations in Metaquot](#using-anti-quotations-in-metaquot)
    - [Example: Inserting Dynamic Expressions with Anti-Quotations](#example-inserting-dynamic-expressions-with-anti-quotations)
- [Building Complex Expressions](#building-complex-expressions)
  - [Example 1: Constructing a Let Expression with `AST_builder`](#example-1-constructing-a-let-expression-with-ast_builder)
  - [Example 2: Constructing a Let Expression with Metaquot](#example-2-constructing-a-let-expression-with-metaquot)
- [Conclusion](#conclusion)

## Description

Building an AST (Abstract Syntax Tree) is a fundamental part of creating a PPX in OCaml. You'll need to construct an AST to represent the code you want to generate or transform.

For example, if you want to generate the following code:

```ocaml
let zero = [%int 0]
```

and replace the extension point `[%int 0]` with `0` to produce `let zero = 0`, you’ll need to build an AST that represents this transformation.

There are several methods to build an AST. We’ll discuss three approaches:

- **Building ASTs with Pure OCaml**
- **Building ASTs with `AST_builder`**
- **Using Metaquot for AST Construction**

## Building ASTs with Low-Level Builders

The most fundamental way to build an AST is to manually construct it using Low-Level Builders data structures.

### Example: Building a Simple Integer AST Manually

[:link: Sample Code](./building_ast.ml#L5-L16)

To construct an AST for a simple integer value `0`:

```ocaml
let zero ~loc : Ppxlib_ast.Ast.expression =
  {
    pexp_desc = Pexp_constant (Pconst_integer ("0", None));
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }
```

While this method provides full control over the AST, it is verbose and less maintainable.

## Building ASTs with `AST_builder`

PPXLib provides the `AST_builder` module, which simplifies the process of building ASTs by providing helper functions.

### Example 1: Using `pexp_constant` for Integer AST

[:link: Sample Code](./building_ast.ml#L18-L24)

Using `pexp_constant`, you can construct an integer AST like this:

```ocaml
let one ~loc =
  Ast_builder.Default.pexp_constant ~loc (Parsetree.Pconst_integer ("1", None))
```

This method is more readable and concise compared to the pure OCaml approach.

### Example 2: Using `eint` for Simplified Integer AST

[:link: Sample Code](./building_ast.ml#L26-L31)

For even more simplicity, use `eint`:

```ocaml
let two ~loc = Ast_builder.Default.eint ~loc 2
```

> [!TIP]
> `eint` is an abbreviation for expression (`e`) integer (`int`).

## Using Metaquot for AST Construction

Metaquot is a syntax extension that allows you to write ASTs in a more natural and readable way.

### Example: Building an Integer AST with Metaquot

[:link: Sample Code](./building_ast.ml#L33-L38)

With Metaquot, you can construct an integer AST like this:

```ocaml
let three ~loc = [%expr 3]
```

> [!TIP]    
> Metaquot is highly readable and intuitive but is static. For dynamic values, use Anti-Quotations.

### Using Anti-Quotations in Metaquot

Anti-Quotations allow you to insert dynamic expressions into your Metaquot ASTs.

#### Example: Inserting Dynamic Expressions with Anti-Quotations

[:link: Sample Code](./building_ast.ml#L72-L77)

To insert a dynamic expression into a Metaquot AST:

```ocaml
let anti_quotation_expr expr = [%expr 1 + [%e expr]]
```

For example, to insert the AST for `1`:

```ocaml
let _ =
  print_endline
    ("\nLet expression with metaquot and anti-quotation: "
    ^ Astlib.Pprintast.string_of_expression (anti_quotation_expr (one ~loc)))
```

## Building Complex Expressions

Beyond simple expressions, you may need to build more complex ASTs, such as `let` expressions.

### Example 1: Constructing a Let Expression with `AST_builder`

[:link: Sample Code](./building_ast.ml#L40-L60)

To build a `let` expression that binds the value `3` to the variable `foo`:

```ocaml
let let_expression =
  let expression =
    Ast_builder.Default.pexp_constant ~loc:Location.none
      (Pconst_integer ("3", None))
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
```

### Example 2: Constructing a Let Expression with Metaquot

[:link: Sample Code](./building_ast.ml#L62-L70)

Alternatively, with Metaquot:

```ocaml
let let_expression =
  [%expr
    let foo = 3 in
    ()]
```

This approach is shorter and easier to understand.

## Conclusion

In this section, we explored three methods for building ASTs:

- **Pure OCaml**: The most basic but verbose approach.
- **Using `AST_builder`**: A more readable and maintainable option.
- **Using Metaquot**: The most intuitive method, especially when combined with Anti-Quotations for dynamic values.

Each method has its strengths, so choose the one that best fits your needs. Understanding all three will give you greater flexibility in creating effective and maintainable PPXs.

### [On the next section, we will learn how to destructure an AST.](../b%20-%20Destructing%20AST/README.md)
