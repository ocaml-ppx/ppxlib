open Ppxlib

let () = Driver.enable_checks ()

let x = 1 [@@foo]
[%%expect{|
Line _, characters 13-16:
Error: Attribute `foo' was not used
|}]

let f x = 1 [@@deprecatd "..."]
[%%expect{|

Line _, characters 15-24:
Error: Attribute `deprecatd' was not used.
       Hint: Did you mean deprecated?
|}]

let attr : _ Attribute.t =
  Attribute.declare "blah"
    Attribute.Context.type_declaration
    Ast_pattern.(__)
    ignore
[%%expect{|

val attr : (type_declaration, unit) Attribute.t = <abstr>
|}]

type t = int [@blah]
[%%expect{|

Line _, characters 15-19:
Error: Attribute `blah' was not used.
       Hint: `blah' is available for type declarations but is used here in
       the
       context of a core type.
       Did you put it at the wrong level?
|}]

let attr : _ Attribute.t =
  Attribute.declare "blah"
    Attribute.Context.expression
    Ast_pattern.(__)
    ignore
[%%expect{|

val attr : (expression, unit) Attribute.t = <abstr>
|}]

type t = int [@blah]
[%%expect{|

Line _, characters 15-19:
Error: Attribute `blah' was not used.
       Hint: `blah' is available for expressions and type declarations but is
       used
       here in the context of a core type.
       Did you put it at the wrong level?
|}]

let _ = () [@blah]
[%%expect{|

Line _, characters 13-17:
Error: Attribute `blah' was not used
|}]

(* Attribute drops *)

let faulty_transformation = object
  inherit Ast_traverse.map as super

  method! expression e =
    match e.pexp_desc with
    | Pexp_constant c ->
      Ast_builder.Default.pexp_constant ~loc:e.pexp_loc c
    | _ -> super#expression e
end
[%%expect{|

val faulty_transformation : Ast_traverse.map = <obj>
|}]

let () =
  Driver.register_transformation "faulty" ~impl:faulty_transformation#structure

let x = (42 [@foo])
[%%expect{|

Line _, characters 14-17:
Error: Attribute `foo' was silently dropped
|}]

type t1 = < >
type t2 = < t1 >
type t3 = < (t1[@foo]) >
[%%expect{|

type t1 = <  >

type t2 = <  >

Line _, characters 17-20:
Error: Attribute `foo' was not used
|}]

(* Reserved Namespaces *)

(* ppxlib checks that unreserved attributes aren't dropped *)

let x = (42 [@bar])
[%%expect{|

Line _, characters 14-17:
Error: Attribute `bar' was silently dropped
|}]

let x = (42 [@bar.baz])
[%%expect{|

Line _, characters 14-21:
Error: Attribute `bar.baz' was silently dropped
|}]

(* But reserving a namespace disables those checks. *)

let () = Reserved_namespaces.reserve "bar"

let x = (42 [@bar])
let x = (42 [@bar.baz])
[%%expect{|

val x : int = 42

val x : int = 42
|}]

let x = (42 [@bar_not_proper_sub_namespace])
[%%expect{|

Line _, characters 14-42:
Error: Attribute `bar_not_proper_sub_namespace' was silently dropped
|}]

(* The namespace reservation process understands dots as namespace
   separators. *)

let () = Reserved_namespaces.reserve "baz.qux"

let x = (42 [@baz])
[%%expect{|

Line _, characters 14-17:
Error: Attribute `baz' was silently dropped
|}]

let x = (42 [@baz.qux])
[%%expect{|

val x : int = 42
|}]

let x = (42 [@baz.qux.quux])
[%%expect{|

val x : int = 42
|}]

let x = (42 [@baz.qux_not_proper_sub_namespace])
[%%expect{|

Line _, characters 14-46:
Error: Attribute `baz.qux_not_proper_sub_namespace' was silently dropped
|}]

(* You can reserve multiple subnamespaces under the same namespace *)

let () = Reserved_namespaces.reserve "baz.qux2"

let x = (42 [@baz.qux])
let x = (42 [@baz.qux2])
[%%expect{|

val x : int = 42

val x : int = 42
|}]

let x = (42 [@baz.qux3])
[%%expect{|

Line _, characters 14-22:
Error: Attribute `baz.qux3' was silently dropped
|}]
