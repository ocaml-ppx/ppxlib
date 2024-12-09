# Destructuring AST

<small>:link: [Docs](https://ocaml-ppx.github.io/ppxlib/ppxlib/matching-code.html)</small>

This section has code examples to help you understand it better.
To run the examples:

```sh
make demo-destructing_ast
```

### Table of Contents

- [Description](#description)
- [AST Structure Pattern Matching](#ast-structure-pattern-matching)
  - [Example: Matching Integer Payload Manually](#example-matching-integer-payload-manually)
- [Using `Ast_pattern` High-Level Destructors](#using-ast_pattern-high-level-destructors)
  - [Example 1: Matching Integer Payload with `Ast_pattern`](#example-1-matching-integer-payload-with-ast_pattern)
  - [Example 2: Simplifying Matching with `eint`](#example-2-simplifying-matching-with-eint)
- [Using Metaquot](#using-metaquot)
  - [Example 1: Matching Integer Payload with Metaquot](#example-1-matching-integer-payload-with-metaquot)
  - [Example 2: Matching Complex Expressions with Metaquot and Anti-Quotations](#example-2-matching-complex-expressions-with-metaquot-and-anti-quotations)
- [Conclusion](#conclusion)

## Description

Destructuring an AST (Abstract Syntax Tree) is essential when creating a PPX (preprocessor extension) in OCaml. To generate or transform code, you must first break down the AST to understand and manipulate its structure.

For example, if you want to transform this code:

```ocaml
let one = [%one]
```

into:

```ocaml
let one = 1
```

You’ll need to destructure the AST representing the extension point (`[%one]`) to replace it with `1`.
There are several ways to destructure an AST. We’ll explore three methods:

- **AST Structure Pattern Matching**
- **Using `Ast_pattern` High-Level Destructors**
- **Using Metaquot**

## AST Structure Pattern Matching

The most fundamental method for destructuring an AST in PPXLib is by directly matching on the AST’s structure.

### Example: Matching Integer Payload Manually

[:link: Sample Code](./destructuring_ast.ml#L11-L26)

Let’s say we want to destructure an AST representing the integer `1`:

```ocaml
let match_int_payload ~loc payload =
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_integer (value, None)); _ }, _);
          _;
        };
      ] -> (
      try Ok (value |> int_of_string)
      with Failure _ ->
        Error (Location.Error.createf ~loc "Value is not a valid integer"))
  | _ -> Error (Location.Error.createf ~loc "Wrong pattern")
```

1. **Pattern Matching the Payload**:
    - Begins by matching the `payload` with the expected structure.
    - The pattern expects a structure (`PStr`) containing a single item.
2. **Destructuring the Structure Item**:
    - Matches the `pstr_desc` field, expecting an evaluated expression (`Pstr_eval`).
    - The expression should be a constant integer (`Pexp_constant` with `Pconst_integer`).
    - Captures the integer value as a string in `value`.
3. **Handling the Matched Value**:
    - Converts the `value` to an integer and returns `Ok` if successful.
    - If conversion fails, returns an error message.
4. **Handling Mismatched Patterns**:
    - If the `payload` doesn’t match the expected structure, it returns an error.

While this method is powerful, it can be verbose and difficult to maintain as patterns become more complex.

## Using `Ast_pattern` High-Level Destructors

To make AST destructuring more readable, PPXLib provides the `Ast_pattern` module, which offers high-level destructors.

### Example 1: Matching Integer Payload with `Ast_pattern`

[:link: Sample Code](./destructuring_ast.ml#L37-L40)

Let’s destructure the same integer `1` AST using `Ast_pattern`:

```ocaml
open Ppxlib

let match_int_payload =
  let open Ast_pattern in
  pstr (pstr_eval (pexp_constant (pconst_integer (string "1") none)) nil ^:: nil)
```

This code achieves the same result as the previous example but in a more concise and readable way.

- **`PStr`** becomes `pstr`
- **`Pstr_eval`** becomes `pstr_eval`
- **`Pexp_constant`** becomes `pexp_constant`
- **`Pconst_integer`** becomes `pconst_integer`

### Example 2: Simplifying Matching with `eint`

[:link: Sample Code](./destructuring_ast.ml#L40-L49)

You can further simplify it:

```ocaml
let match_int_payload =
  let open Ast_pattern in
  pstr (pstr_eval (eint (int 1)) nil ^:: nil)
```

Using `eint` instead of `pexp_constant` and `pconst_integer` provides better type safety. The `int` wildcard captures the integer value.

## Using Metaquot

Metaquot is a syntax extension that allows you to write and destructure ASTs more intuitively.

### Example 1: Matching Integer Payload with Metaquot

[:link: Sample Code](./destructuring_ast.ml#L51-L60)

Let’s destructure the same integer `1` AST with Metaquot:

```ocaml
let match_int_payload expr =
  match expr with
  | [%expr 1] -> Ok 1
  | _ -> Error (Location.Error.createf ~loc:expr.pexp_loc "Wrong pattern")
```

### Example 2: Matching Complex Expressions with Metaquot and Anti-Quotations

[:link: Sample Code](./destructuring_ast.ml#L79-L90)

For example, to match any expression of the form `1 + <int>`:

```ocaml
let match_int_payload expr =
  match expr with
  | [%expr 1 + [%e? e]] -> (
      match e with
      | { pexp_desc = Pexp_constant (Pconst_integer (value, None)); _ } ->
          Ok (1 + int_of_string value)
      | _ -> Error (Location.Error.createf ~loc:e.pexp_loc "Invalid integer"))
  | _ -> Error (Location.Error.createf ~loc:expr.pexp_loc "Wrong pattern")
```

Metaquot simplifies the process, making the AST patterns more readable, especially for complex structures.

## Conclusion

In this section, we explored different methods to destructure an AST using PPXLib:

- **AST Structure Pattern Matching**: Powerful but verbose.
- **Using `Ast_pattern` High-Level Destructors**: More readable and maintainable.
- **Using Metaquot**: Intuitive and effective for both simple and complex patterns.

There’s no right way to destructure an AST, choose the approach that best fits your use case. Understanding all these methods is valuable for creating robust and maintainable PPXs.

### [On the next section, we will learn how to write a PPX.](../../2%20-%20Writing%20PPXs/README.md)