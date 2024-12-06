# Global Transformations

This section contains code examples to help you understand how to implement global transformations in OCaml using PPXLib.
To run the examples:

```sh
make demo-global
```

### Table of Contents

- [Description](#description)
- [Types of Global Transformations](#types-of-global-transformations)
- [Using `Ast_traverse`](#using-ast_traverse)
  - [How It Works](#how-it-works)
  - [Key Points](#key-points)
- [Lint](#lint)
  - [Example 1: Linting Variable Names to Have the Prefix `demo_`](#example-1-linting-variable-names-to-have-the-prefix-demo_)
- [Preprocess](#preprocess)
  - [Example 1: Extending a Module with the `[@enum]` Attribute](#example-1-extending-a-module-with-the-enum-attribute)
- [Global Transformation](#global-transformation)
  - [Example 1: Extending a Module with the `[@enum2 opt]` Attribute](#example-1-extending-a-module-with-the-enum2-opt-attribute)
- [Conclusion](#conclusion)

## Description

As we saw in the [Writing PPXs section](../README.md), global transformations are a powerful way to automate tasks that affect entire modules or large sections of code. By extending the principles of context-free transformations to operate at the module level, you can implement transformations that significantly reduce boilerplate and improve code consistency.

### Types of Global Transformations

- Lint
- Preprocess
- Instrumentation - Before
- Global Transformation
- Instrumentation - After

For now, in this section, we are going to focus on **Lint**, **Preprocess**, and **Global Transformation** because they are the most common phases to register a global transformation.
In the future, we plan to add **Instrumentation - Before** and **Instrumentation - After**.

## Using `Ast_traverse`

To help with global transformations, we'll use the `Ast_traverse` module from PPXLib in all examples. `Ast_traverse` makes it easier to walk through and change the AST in a structured way.

### How It Works:

`Ast_traverse` is helpful for navigating and modifying complex structures like the AST.

Here are the main types of traversals you can do with `Ast_traverse`:

- **Iterators**: Go through the AST and perform actions on each node, often for side effects like checking for specific patterns or enforcing rules.

- **Maps**: Traverse the AST and replace nodes where needed. This is useful for making changes to the AST and returning a modified version.

- **Folds**: Traverse the AST while keeping track of some data (an accumulator) that gets updated at each node, such as counting nodes or gathering specific information.

- **Lifts**: Transform an AST node into a different type by working from the bottom up, often used to convert AST structures into other forms.

### Key Points:

- **Inherit from `Ast_traverse` classes**: Depending on your needs, you can inherit from classes like `Ast_traverse.iter` for iterators or `Ast_traverse.map` for maps. This gives you a base to start from.

- **Override specific methods**: Customize your traversal by overriding methods that handle specific AST nodes, like `module_binding` or `structure_item`.

- **Register with `Driver.register_transformation`**: After defining your traversal, register it with the PPX driver. This ensures your transformations are applied during compilation.

Using `Ast_traverse` simplifies global transformations, letting you efficiently modify large sections of code or entire modules without needing to handle all the details manually.

## Lint

Linting is a form of static analysis that checks code for potential errors, bugs, or style issues. PPXLib provides a mechanism to implement linting rules using PPX. It takes as input the whole AST and outputs a list of "lint" errors. For linting, we are going to use the `Ast_traverse.fold` as we want to provide a list of errors.

### Example 1: Linting Variable Names to Have the Prefix `demo_`

[:link: Sample Code](./context_free.ml#L1-L4)

Let's create a linting rule that ensures that all `value_binding`s have the prefix `demo_`.

#### Consider the following example:

```ocaml
(* This will raise a lint error *)
let name = "John Doe"

(* This will not raise a lint error *)
let demo_name = "John Doe"
```

#### Steps to Implement This Transformation:

- **Understand the AST Structure:**
  We want to match all `value_binding`s. To do this, it’s helpful to see the structure of the AST for a `value_binding`. For that, you can use [AST Explorer](https://astexplorer.net/#/gist/d479d32127d6fcb418622ee84b9aa3b2/27d0a140f268bae1a32c8882d55c0b26c7e03fe9). If you’re not familiar with reading ASTs, check out the [AST section](../../1%20-%20AST/README.md).

- **Ast_traverse.fold:**
  We are going to use `Ast_traverse.fold` to provide a list of errors. Since we want to match all `value_binding` names, we’ll override the `value_binding` method in the AST traversal object, and for each `value_binding`, we’ll check if the variable name starts with `demo_` using `value_binding.pvb_pat.ppat_desc`.

  ```ocaml
  let traverse =
  object
    (* Inherit from Ast_traverse.fold with the Lint_error.t list as the accumulator *)
    inherit [Driver.Lint_error.t list] Ast_traverse.fold

    (* Override the value_binding method to lint the variable name *)
    (* the value_binding method is called for each value_binding in the AST *)
    method! value_binding vb acc =
      let loc = vb.pvb_loc in
        match ast with
        (* Match all pattern variables and get their names *)
        | Ppat_var { txt = name; _ } ->
            (* Check if the variable name starts with demo_ *)
            if String.starts_with name ~prefix:"demo_" then acc
            else
              (* If not, add a lint error to the accumulator *)
              Driver.Lint_error.of_string loc
                "Oops, variable names must start with demo_"
              :: acc
        | _ -> acc
  end
  ```

- **Register the Lint Rule with the PPX Driver:**
  Register with `~lint_impl`.

  ```ocaml
  let _ = Driver.register_transformation "lint" ~lint_impl:traverse#structure
  ```

## Preprocess

Preprocessing is the first phase that alters the AST.


> [!WARNING]
> You should only register a transformation in this phase if it is really necessary. You can use the Global Transformation phase instead.

### Example 1: Extending a Module with the `[@enum]` Attribute

[:link: Sample Code](./context_free.ml#L9-L18)

Let’s say we want to extend a module with automatically generated `to_string` and `from_string` functions based on a variant type using the `[@enum]` attribute.

#### Consider the following example:

```ocaml
module GameEnum = struct
  type t = Rock | Paper | Scissors
end [@enum]
(* Output:
module GameEnum = struct
  type t = Rock | Paper | Scissors
  let to_string = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"
  let from_string = function
    | "Rock" -> Rock
    | "Paper" -> Paper
    | "Scissors" -> Scissors
    | _ -> failwith "Invalid string"
end *)
```

#### Steps to Implement This Global Transformation:

- **Understand the AST Structure:**
  We want to match a `module_expr` with the `[@enum]` attribute and generate `to_string` and `from_string` functions based on the variant type within the module.

- **Ast_traverse.map:**
  We are going to use `Ast_traverse.map` because we want to modify the AST. We’ll override the `module_expr` method in the AST traversal object to append the generated `to_string` and `from_string` functions to the module's structure.

  ```ocaml
  let traverse =
    object
      inherit Ast_traverse.map as super

      (* Override the module_expr method to generate to_string and from_string functions *)
      method! module_expr mod_exp =
        (* Call the super method to traverse the module expression *)
        let mod_exp = super#module_expr mod_exp in
        (* Check if the module expression has the [@enum] attribute *)
        match mod_exp.pmod_attributes with
        | [ { attr_name = { txt = "enum"; _ }; _ } ]
          -> (
            (* match the module expression structure to get the type name and variants *)
            match mod_exp.pmod_desc with
            | Pmod_structure
                ([ { pstr_desc = Pstr_type (name, variants); _ } ] as str) ->
                (* We are not going to show the enum function because we already covered it in the previous Context-free section *)
                let type_ =
                  enum ~loc:mod_exp.pmod_loc (name, variants) ()
                in
                (* Append the generated functions to the module structure *)
                Ast_builder.Default.pmod_structure ~loc:mod_exp.pmod_loc (str @ type_)
            | _ -> mod_exp)
        | _ -> mod_exp
    end
  ```

- **Register the Deriver with the PPX Driver:**

  ```ocaml
  let _ = Driver.register_transformation "enum" ~impl:traverse#structure
  ```

## Global Transformation

The Global Transformation phase can be confusing because everything we’ve discussed in this section falls under global transformations. However, the Global Transformation phase specifically refers to the phase that happens after the Context-free phase.

This is the most common phase to register a global transformation that alters the AST.

The API of the global transformation is the same as the preprocess, and to make it simple, we are going to use the same example as the preprocess, but with payload.

### Example 1: Extending a Module with the `[@enum2 opt]` Attribute

[:link: Sample Code](./context_free.ml#L27-L47)

Let’s extend the previous example to add support for an `opt` argument that modifies the behavior of the `from_string` function to return an `option` type instead of raising an exception.

#### Consider the following example:

```ocaml
module GameEnum2 = struct
  type t = Rock | Paper | Scissors
end [@enum2 opt]
(* Output:
module GameEnum2 = struct
  type t = Rock | Paper | Scissors
  let to_string = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"
  let from_string = function
    | "Rock" -> Some Rock
    | "Paper" -> Some Paper
    | "Scissors" -> Some Scissors
    | _ -> None
end *)
```

#### Steps to Implement This Global Transformation:

- **This example is an extension of the previous one.**
  The only thing that changes is the `from_string` function, which now returns an `option` type instead of raising an exception. To do this, we need to get the attribute's payload.

  ```ocaml
  (* Check if the module expression has the @enum2 attribute and get the attribute's payload *)
  | [ { attr_name = { txt = "enum2"; _ }; attr_payload = payload; _ } ]
          -> (
            (* match the module expression structure to get the type name and variants *)
            let opt =
              match payload with PStr [%str opt] -> true | _ -> false
            in
            match mod_exp.pmod_desc with
            | Pmod_structure
                ([ { pstr_desc = Pstr_type (name, variants); _ } ] as str) ->
                (* We are not going to show the enum function because we already covered it in the previous Context-free section *)
                let type_ =
                  enum ~loc:mod_exp.pmod_loc ~opt (name, variants) ()
                in
                Ast_builder.Default.pmod_structure ~loc:mod_exp.pmod_loc (str @ type_)
            | _ -> mod_exp)
        | _ -> mod_exp
  ```

- **Register the Deriver with the PPX Driver:**
  The difference here compared to the preprocess is that we are going to use the `~impl` instead of `~preprocess_impl`.

  ```ocaml
  let _ = Driver.register_transformation "enum2" ~impl:traverse#structure
  ```

## Conclusion

Global transformations in OCaml using PPXLib allow you to automate repetitive tasks and enforce coding patterns across your entire codebase. By using phases like **Preprocess**, **Global Transformation**, and **Lint**, you can reduce boilerplate code, maintain consistency, and catch potential issues early.

We looked at how `Ast_traverse` helps in navigating and modifying the AST for tasks like generating `to_string` and `from_string` functions or implementing linting rules. The examples showed how to extend modules with attributes like `[@enum]` and `[@enum2 opt]`.

Understanding these concepts and using the right transformation phase ensures your code is cleaner, more consistent, and easier to maintain.

### [In the next section, we will explore advanced use cases of global transformations.](../c%20-%20Advanced%20Global%20Transformations/README.md)