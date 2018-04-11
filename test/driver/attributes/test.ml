#use "topfind";;
#require "base";;

open Base
open Ppxlib

let () = Driver.enable_checks ()

let x = 1 [@@foo]
[%%expect{|
File "test/driver/attributes/test.ml", line 9, characters 13-16:
Error: Attribute `foo' was not used
|}]

let f x = 1 [@@deprecatd "..."]
[%%expect{|
File "test/driver/attributes/test.ml", line 15, characters 15-24:
Error: Attribute `deprecatd' was not used.
Hint: Did you mean deprecated?
|}]

let attr : _ Attribute.t =
  Attribute.declare "blah"
    Attribute.Context.type_declaration
    Ast_pattern.(__)
    ignore
[%%expect{|
val attr : (Ppxlib__.Import.type_declaration, Base.unit) Ppxlib.Attribute.t =
  <abstr>
|}]

type t = int [@blah]
[%%expect{|
File "test/driver/attributes/test.ml", line 32, characters 15-19:
Error: Attribute `blah' was not used.
Hint: `blah' is available for type declarations but is used here in the
context of a core type.
Did you put it at the wrong level?
|}]

let attr : _ Attribute.t =
  Attribute.declare "blah"
    Attribute.Context.expression
    Ast_pattern.(__)
    ignore
[%%expect{|
val attr : (Ppxlib__.Import.expression, Base.unit) Ppxlib.Attribute.t =
  <abstr>
|}]

type t = int [@blah]
[%%expect{|
File "test/driver/attributes/test.ml", line 51, characters 15-19:
Error: Attribute `blah' was not used.
Hint: `blah' is available for expressions and type declarations but is used
here in the context of a core type.
Did you put it at the wrong level?
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
val faulty_transformation : Ppxlib.Ast_traverse.map = <obj>
|}]

let () =
  Driver.register_transformation "faulty" ~impl:faulty_transformation#structure

let x = (42 [@foo])
[%%expect{|
File "test/driver/attributes/test.ml", line 78, characters 14-17:
Error: Attribute `foo' was silently dropped
|}]
