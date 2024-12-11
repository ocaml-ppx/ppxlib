# Context-Free Transformations

This section has code examples to help you understand it better.
To run the examples:

```sh
make demo-context_free
```

### Table of Contents

- [Description](#description)
- [Types of Context-Free Transformations](#types-of-context-free-transformations)
- [Extenders](#extenders)
  - [Example 1: A Simple Extender](#example-1-a-simple-extender)
  - [Example 2: A More Complex Extender with Payload](#example-2-a-more-complex-extender-with-payload)
- [Derivers](#derivers)
  - [Example 1: Enum Deriver](#example-1-enum-deriver)
  - [Example 2: Enum Deriver with args](#example-2-enum-deriver-with-args)

## Description

Context-free transformations allow you to read and modify code locally, without needing to consider the global context. In practice, this means that a portion of the Abstract Syntax Tree (AST) is provided to the transformation, and the transformation returns a new AST with the applied modifications.

### Types of Context-Free Transformations

There are two main types of context-free transformations:

- **[Extenders](#extenders)**: These modify the extension node by generating new code.
- **[Derivers](#derivers)**: These append code after the item without changing the original item.

## Extenders
<small>:page_facing_up: [Doc](https://ocaml-ppx.github.io/ppxlib/ppxlib/driver.html#def_extenders)</small><br>
<small>⬅️ Extenders work with extension nodes. If you have any doubts about attributes, please review the [AST Extension Node section](../../1%20-%20AST/README.md#ast_extension_node).</small>

Extenders allow you to replace an extension node with new content. However, they do not have direct access to the surrounding code context, so they cannot modify the surrounding code.

If an extender is broken or missing, the code will not compile. Therefore, it is important to ensure that the extender is correctly implemented.

An extension node is a node in the AST that represents an extension point. For example, in the code `let x = [%foo]`, `[%foo]` is an extension node.

Let's look at some examples to understand how this works.

With extenders, we need to:

- **Hook the extension.**
- **Transform the payload** (if there is one).
- **Create a new AST.**

### Example 1: A Simple Extender 
[:link: Sample Code](./context_free.ml#L5-L17)

Consider the following code:

```ocaml
let one = [%one]
(* Output: let one = 1 *)
```

Here, `[%one]` is replaced with the integer value `1`. This is a basic example of an extender transformation.

#### Steps to Implement This Extender:

- **Declare the extension name:**

  ```ocaml
  let extender_name = "one"
  ```

- **Define the extender extractor:**
  Since there is no payload (additional data), we define the extractor as:

  ```ocaml
  let extender_extracter = Ast_pattern.(pstr nil)
  ```

- **Create the new AST:**
  We define the expression that will replace `[%one]`:

  ```ocaml
  let expression ~loc = [%expr 1]
  ```

  Alternatively, you can use:

  ```ocaml
  let expression ~loc = Ast_builder.Default.eint ~loc 1
  ```

- **Declare the extender and register it:**

  ```ocaml
  (* Define the expansion logic *)
  let expand ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    expression ~loc

  (* Define the extension *)
  let extension =
    Extension.V3.declare extender_name Extension.Context.expression
      extender_extracter
      expand

  (* Register the extender *)
  let rule = Ppxlib.Context_free.Rule.extension extension
  let () = Driver.register_transformation ~rules:[ rule ] extender_name
  ```

### Example 2: A More Complex Extender with Payload 
[:link: Sample Code](./context_free.ml#L22-L47)

Let's look at a more complex example, where we replace `[%emoji "grin"]` with an emoji:

```ocaml
let grin = [%emoji "grin"]
(* Output: let grin = "😀" *)
```

#### Steps to Implement This Extender:

- **Declare the extension name and extractor:**
  Here, the payload is a string (the alias of the emoji):

  ```ocaml
  let extender_name = "emoji"
  let extender_extracter = Ast_pattern.(single_expr_payload (estring __))
  ```

- **Create the new AST:**  
  We define the expression to replace the alias with the corresponding emoji:

  ```ocaml
  let expression ~loc ~emoji = [%expr [%e estring ~loc emoji]]
  ```

- **Define the expansion logic:**  
  We need to map the alias to an emoji and return the appropriate AST. If the alias isn't found, we return an error:

  ```ocaml
  let emojis =
    [
      { emoji = "😀"; alias = "grin" };
      { emoji = "😃"; alias = "smiley" };
      { emoji = "😄"; alias = "smile" };
    ]

  let expand ~ctxt emoji_text =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in

    let find_emoji_by_alias alias =
      List.find_opt (fun emoji -> alias = emoji.alias) emojis
    in

    match find_emoji_by_alias emoji_text with
    | Some value -> expression ~loc ~emoji:value.emoji
    | None ->
        let ext =
          Location.error_extensionf ~loc "No emoji found for alias %s" emoji_text
        in
        Ast_builder.Default.pexp_extension ~loc ext
  ```

- **Declare the extender:**

  ```ocaml
  let extension =
    Extension.V3.declare extender_name Extension.Context.expression
      extender_extracter
      expand
  ```

---

## Derivers
<small>:page_facing_up: [Doc](https://ocaml-ppx.github.io/ppxlib/ppxlib/driver.html#def_derivers)</small><br>
<small>⬅️ A deriver is a custom attribute provided by PPXlib. If you have any doubts about attributes, please review the AST Attributes section.</small>

Derivers differ from extenders in that they append new code after an existing item rather than replacing parts of it. The new code can work in conjunction with the original item or independently, depending on the transformation needed. They are specified using the [@@deriving] attribute.

A simple and common example of a deriver is the `enum` deriver:

```ocaml
type t = A | B [@@deriving enum]
(* Output:
type t = A | B
let to_string = function
  | A -> "A"
  | B -> "B"
let from_string = function
  | "A" -> A
  | "B" -> B
  | _ -> raise (Invalid_argument "Argument doesn't match t variants")
*)
```

In this example, the deriver `enum` automatically generates `to_string` and `from_string` functions for a variant type.

Derivers are generally more complex to register than extenders, but PPXLib simplifies this with the `Deriving.add` function, which handles the registration. This function uses `Driver.register_transformation` under the hood.

It can be attached to various types of structures and signatures. For instance, to create a deriver for a type declaration, you would use the `~str_type_decl` argument. If the deriver should also work for signature items, you would use the `~sig_type_decl` argument.

The full list of arguments for `Deriving.add` can be found in the [documentation](https://ocaml-ppx.github.io/ppxlib/ppxlib/Ppxlib/Deriving/index.html#val-add).

### Example 1: Enum Deriver
[:link: Sample Code](./context_free.ml#L51-L125)

The following example is more complex. Take your time; it’s explained step by step.

Let's say we want to add `to_string` and `from_string` functions to a simple variant type:

```ocaml
type t = A | B [@@deriving enum]
(* Output:
type t = A | B
let to_string = function
  | A -> "A"
  | B -> "B"
let from_string = function
  | "A" -> A
  | "B" -> B
  | _ -> raise (Invalid_argument "Argument doesn't match t variants")
*)
```

#### Steps to Implement This Deriver:

- **Declare the deriver name:**

  ```ocaml
  let deriver_name = "enum"
  ```

- **Define the arguments for the deriver:**
  For this example, we don't have any arguments:

  ```ocaml
  let args () = Deriving.Args.(empty)
  ```

- **Build the new AST:**
  We'll match the AST we want to transform and generate the `to_string` and `from_string` functions.

  - **Match the type declaration with pattern matching:**

    ```ocaml
    let enum ~ctxt ast =
       let loc = Expansion_context.Deriver.derived_item_loc ctxt in
       match ast with
       | ( _,
           [
             {
               ptype_name = { txt = type_name; _ };
               ptype_kind = Ptype_variant variants;
               _;
             };
           ] ) -> (*...*)
    ```

  - **Create functions to generate the patterns:**
    All we are going to do here is what we covered in [Building AST](../../1%20-%20AST/a%20-%20Building%20AST/README.md). So it shouldn't be a problem to understand this part.

    - **Creating the `to_string` function:**

      ```ocaml
      let function_name suffix = type_name ^ suffix in
      let arg_pattern = [%pat? value] in
      let function_name_pattern =
        [%pat? [%p ppat_var ~loc { txt = function_name "_to_string"; loc }]]
      in
      let to_string_expr =
        [%stri
          let [%p function_name_pattern] =
          fun [%p arg_pattern] ->
            [%e
              pexp_match ~loc [%expr value]
                (List.map
                  (fun { pcd_name = { txt = value; _ }; _ } ->
                    case
                      ~lhs:
                        (ppat_construct ~loc (Located.lident ~loc value) None)
                      ~guard:None ~rhs:(expr_string value))
                variants)]]
      ```

    - **Build the `from_string` function:**

      ```ocaml
      let else_case =
        case
          ~lhs:[%pat? [%p ppat_any ~loc]]
          ~guard:None
          ~rhs:
            [%expr raise (Invalid_argument "Argument doesn't match variants")]
      in
      let from_string_expr =
        [%stri
          let [%p function_name_pattern] =
          fun [%p arg_pattern] ->
            [%e
              pexp_match ~loc [%expr value]
                (List.map
                  (fun { pcd_name = { txt = value; _ }; _ } ->
                    case
                      ~lhs:
                        (ppat_constant ~loc (Pconst_string (value, loc, None)))
                      ~guard:None ~rhs:
                        (pexp_construct ~loc (Located.lident ~loc value) None))
                  variants
                @ [ else_case ])]]
      ```

    - **Combine and return the functions:**

      ```ocaml
      let enum ~ctxt ast =
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match ast with
        | ( _,
            [
              {
                ptype_name = { txt = type_name; _ };
                ptype_kind = Ptype_variant variants;
                _;
              };
            ] ) ->
            let function_name suffix = type_name ^ suffix in
            let arg_pattern = [%pat? value] in
            let expr_string = Ast_builder.Default.estring ~loc in
            let function_name_pattern =
              [%pat? [%p ppat_var ~loc { txt = function_name "_to_string"; loc }]]
            in
            let to_string_expr =
              [%stri
                let [%p function_name_pattern] =
                fun [%p arg_pattern] ->
                  [%e
                    pexp_match ~loc [%expr value]
                      (List.map
                        (fun { pcd_name = { txt = value; _ }; _ } ->
                          case
                            ~lhs:
                              (ppat_construct ~loc (Located.lident ~loc value) None)
                            ~guard:None ~rhs:(expr_string value))
                        variants)]]
            in
            (* Uncomment to see the generated code *)
            (* print_endline (Astlib.Pprintast.string_of_structure [ to_string_expr ]); *)
            let else_case =
              case
                ~lhs:[%pat? [%p ppat_any ~loc]]
                ~guard:None
                ~rhs:
                  [%expr
                    [%e
                      pexp_apply ~loc
                        [%expr
                          raise
                            (Invalid_argument
                              [%e
                                estring ~loc
                                  ("Argument doesn't match " ^ type_name
                                  ^ " variants")])]
                        []]]
            in
            let function_name_pattern =
              [%pat? [%p ppat_var ~loc { txt = function_name "_from_string"; loc }]]
            in
            let from_string_expr =
              [%stri
                let [%p function_name_pattern] =
                fun [%p arg_pattern] ->
                  [%e
                    pexp_match ~loc [%expr value]
                      (List.map
                        (fun { pcd_name = { txt = value; _ }; _ } ->
                          case
                            ~lhs:
                              (ppat_constant ~loc (Pconst_string (value, loc, None)))
                            ~guard:None ~rhs:
                              (pexp_construct ~loc (Located.lident ~loc value) None))
                        variants
                      @ [ else_case ])]]
            in
            (* Uncomment to see the generated code *)
            (* print_endline (Astlib.Pprintast.string_of_structure [ from_string_expr ]); *)
            [ from_string_expr; to_string_expr ]
        | _ ->
            [%str
              [%ocaml.error "Ops, enum2 must be a type with variant without args"]]
      ```

- **Declare the deriver:**

  ```ocaml
  let generator () =
    Deriving.Generator.V2.make (args ()) (fun ~ctxt ->
        enum ~loc:Expansion_context.Deriver.derived_item_loc ctxt)
  let _ = Deriving.add deriver_name ~str_type_decl:(generator ())
  ```

### Example 2: Enum Deriver with args
[:link: Sample Code](./context_free.ml#L126-L216)

Let's say we want to add `to_string` and `from_string` functions to a variant type, but we want to have it with options instead of raise:

```ocaml
type t = A | B [@@deriving enum2 ~opt]
(* Output:
type t = A | B
let to_string = function
  | A -> "A"
  | B -> "B"
let from_string = function
  | "A" -> Some A
  | "B" -> Some B
  | _ -> None
*)
```

#### Steps to Implement This Deriver:

This is the same as the previous example, but we need to add a new argument to the deriver:

- **Declare the deriver name and arguments:**

  ```ocaml
  let deriver_name = "enum"
  let args () = Deriving.Args.(empty +> arg "opt" bool)
  ```

- **Build the new AST:**
  There will no much difference on the enum code, we just need to check if the `opt` argument is `true` and add the `option` return to the `from_string` function and change the else to `None`:

  ```ocaml
  let else_case =
    case
      ~lhs:[%pat? [%p ppat_any ~loc]]
      ~guard:None
      ~rhs:
        (match opt with
        | true -> [%expr None]
        | _ ->
            [%expr
              raise (Invalid_argument "Argument doesn't match variants")])
  in
  let function_name_pattern =
    [%pat? [%p ppat_var ~loc { txt = function_name "_from_string"; loc }]]
  in
  let from_string_expr =
    [%stri
      let [%p function_name_pattern] =
        fun [%p arg_pattern] ->
        [%e
          pexp_match ~loc [%expr value]
            (List.map
                (fun { pcd_name = { txt = value; _ }; _ } ->
                  case
                    ~lhs:
                      (ppat_constant ~loc (Pconst_string (value, loc, None)))
                    ~guard:None
                    ~rhs:
                      (match opt with
                      | true ->
                          [%expr
                            Some
                              [%e
                                pexp_construct ~loc
                                  (Located.lident ~loc value)
                                  None]]
                      | _ ->
                          pexp_construct ~loc
                            (Located.lident ~loc value)
                            None))
                variants
            @ [ else_case ])]]
  ```

## Conclusion

Context-free transformations are a powerful tool in OCaml for modifying code locally. By understanding how to implement extenders and derivers, you can enhance your code generation capabilities and simplify repetitive tasks. With the examples provided, you should have a solid foundation for creating your own context-free transformations using PPXLib.

### [On the next section, we will learn more about global transformations.](../b%20-%20Global/README.md)

---

**WIP** :construction:

Todo:
- [ ] Special Functions
- [ ] Constant Rewriting

