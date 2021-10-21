open Ppxlib

(* --------------------------- Test Setup ----------------------------------- *)

(* These tests check that the inside of payloads is properly expanded or not
   expanded by the driver. *)

let int_of_payload ~loc ~transformation_name payload =
  (match payload with
   | PStr
      [ {pstr_desc =
          Pstr_eval
            ( {pexp_desc = Pexp_constant (Pconst_integer (i, None)); _ }
            , _)
        ; _} ] ->
     int_of_string i
   | _ -> Location.raise_errorf ~loc "Invalid %s payload!" transformation_name)

[%%expect{|
val int_of_payload :
  loc:location -> transformation_name:string -> payload -> int = <fun>
|}]

(* A legacy transformation, rewriting [%legacy_add_one <int>] as
   [<int + 1>], written as a whole AST transformation *)
let legacy_add_one =
  object
    inherit Ast_traverse.map as super

    method! expression expr = 
      match expr.pexp_desc with
      | Pexp_extension ({txt = "legacy_add_one"; _}, payload) ->
        let loc = expr.pexp_loc in
        let i = int_of_payload ~loc ~transformation_name:"add_one" payload in
        Ast_builder.Default.eint ~loc (i + 1)
      | _ -> super#expression expr
    end

let () =
  Driver.register_transformation
    ~impl:legacy_add_one#structure
    "legacy_add_one"

[%%expect{|
val legacy_add_one : Ast_traverse.map = <obj>
|}]

(* A legacy attribute-based generator implemented as a whole AST transformation.
   [type _ = _ [@@gen_x <int>]] generates an extra [let x = <int>]. *)
let legacy_deriver =
  let get_gen_x attrs =
      List.find_map
        (function
          | {attr_name = {txt = "gen_x"; _}; attr_payload; attr_loc} ->
            Some (attr_payload, attr_loc)
          | _ -> None)
        attrs
  in
  object(self)
    inherit Ast_traverse.map

    method! structure str =
      List.concat_map
        (fun stri ->
           match stri.pstr_desc with
           | Pstr_type (_, [{ptype_attributes = (_::_ as attrs); _}]) ->
             (match get_gen_x attrs with
              | Some (payload, loc) ->
                let i = int_of_payload ~loc ~transformation_name:"gen_x" payload in
                let stri = self#structure_item stri in
                let value = Ast_builder.Default.eint ~loc i in
                let x_binding = [%stri let x = [%e value]] in
                [stri; x_binding]
              | None -> [self#structure_item stri])
           | _ -> [self#structure_item stri])
        str
  end

let () =
  Driver.register_transformation
    ~impl:legacy_deriver#structure
    "legacy_deriver"

[%%expect{|
val legacy_deriver : Ast_traverse.map = <obj>
|}]

(* An expression extension that simply expands to its payload.
   I.e. [[%id 1]] expands to [1]. *)
let id =
  Extension.V3.declare
    "id"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt:_ expr -> expr)
  |> Context_free.Rule.extension

let () = Driver.register_transformation ~rules:[id] "id"

[%%expect{|
val id : Context_free.Rule.t = <abstr>
|}]

(* ------------------------- Actual Test ----------------------------------- *)

(* Context free transformations are applied inside payload of extensions or
   attributes that aren't themselves expanded by context-free rules

   The examples below are expected to succeed as the extension inside the payload
   should be expanded during the context-free rule pass, that happens before
   whole AST transformations. *)
let x = [%legacy_add_one [%id 1]]

[%%expect{|
val x : int = 2
|}]

type t = unit
[@@gen_x [%id 1]]

[%%expect{|
type t = unit
val x : int = 1
|}]

(* --------------------------- Test Setup ----------------------------------- *)

(* The same transformation as [legacy_add_one] but written as a context-free
   rule *)
let add_one =
  Extension.V3.declare
    "add_one"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (eint __))
    (fun ~ctxt i ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       Ast_builder.Default.eint ~loc (i + 1))
  |> Context_free.Rule.extension

let () = Driver.register_transformation ~rules:[add_one] "add_one"

[%%expect{|
val add_one : Context_free.Rule.t = <abstr>
|}]

(* A deriver that accepts an integer [value] argument, specifying the value
   of the derived integer [x].

   E.g. [type t = _ [@@deriving x ~value:1]] will derive
   [let x = 1]. *)
let deriver =
  let expand ~ctxt _type_decl value =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let value =
      match value with
      | None -> [%expr 0]
      | Some i -> Ast_builder.Default.eint ~loc i
    in
    [%str let x = [%e value]]
  in
  let args =
    let open Deriving.Args in
    let value = arg "value" Ast_pattern.(eint __) in
    empty +> value
  in
  let str_type_decl =
    Deriving.Generator.V2.make args expand
  in
  Deriving.add ~str_type_decl "x"

[%%expect{|
val deriver : Deriving.t = <abstr>
|}]

(* ------------------------- Actual Test ----------------------------------- *)

(* Context-free transformations cannot be applied inside the payload of
   extensions that are themselves expanded by a context-free rule,
   simply because the outermost extension is expanded first.

   The example below should trigger an error as the payload is expected to be an
   integer expression and the extension in its payload should NOT be expanded.

   This is an expected and relatively sane behaviour. As Carl Eastlund pointed
   out, it might make sense at some point to allow expander to ask ppxlib to
   expand a node explicitly via a callback but it shouldn't be done by default.
   *)
let y = [%add_one [%id 1]]

[%%expect{|
Line _, characters 18-25:
Error: constant expected
|}]

(* Context-free transformations should not be applied inside the payload of
   attributes interpreted by other context-free rules. This is a bug introduced
   in https://github.com/ocaml-ppx/ppxlib/pull/279.

   The example below should trigger an error as the [value] argument in the
   paylaod is expected to be an integer expression and the extension in its
   payload should NOT be expanded.

   Here, just as in extensions, we might eventually provide a callback to expand
   nodes explicitly. *)
type u = unit
[@@deriving x ~value:[%id 1]]

[%%expect{|
type u = t
val x : int = 1
|}]
