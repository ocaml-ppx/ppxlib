open Ppxlib


let foo =
  Deriving.add "foo"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ _ -> [%str let foo = 42]))
    ~sig_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ _ -> [%sig: val foo : int]))
[%%expect{|
val foo : Deriving.t = <abstr>
|}]

let bar =
  Deriving.add "bar"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      ~deps:[foo]
                      (fun ~loc ~path:_ _ -> [%str let bar = foo + 1]))
[%%expect{|
val bar : Deriving.t = <abstr>
|}]

let foobar =
  Deriving.add_alias "foobar" [bar; foo]
[%%expect{|
val foobar : Deriving.t = <abstr>
|}]

let foobar_find =
  Deriving.add_alias "foobar_find" [Deriving.find "bar"; Deriving.find "foo"]
[%%expect{|
val foobar_find : Deriving.t = <abstr>
|}]

let baz =
  Deriving.find "baz"
[%%expect{|
Exception: Not_found.
|}]

let mtd =
  Deriving.add "mtd"
    ~sig_module_type_decl:(
      Deriving.Generator.make_noarg
        (fun ~loc ~path:_ _ -> [%sig: val y : int]))
    ~str_module_type_decl:(
      Deriving.Generator.make_noarg
        (fun ~loc ~path:_ _ -> [%str let y = 42]))
[%%expect{|
val mtd : Deriving.t = <abstr>
|}]

type t = int [@@deriving bar]
[%%expect{|
Line _, characters 25-28:
Error: Deriver foo is needed for bar, you need to add it before in the list
|}]

type t = int [@@deriving bar, foo]
[%%expect{|
Line _, characters 25-33:
Error: Deriver foo is needed for bar, you need to add it before in the list
|}]

type nonrec int = int [@@deriving foo, bar]
[%%expect{|
type nonrec int = int
val foo : int = 42
val bar : int = 43
|}]

type nonrec int = int [@@deriving foobar]
[%%expect{|
type nonrec int = int
val foo : int = 42
val bar : int = 43
|}]

type nonrec int = int [@@deriving foobar_find]
[%%expect{|
type nonrec int = int
val foo : int = 42
val bar : int = 43
|}]

module type Foo_sig = sig
  type t [@@deriving foo]
end
[%%expect{|
module type Foo_sig = sig type t val foo : int end
|}]

module type X = sig end [@@deriving mtd]
[%%expect{|
module type X = sig end
val y : int = 42
|}]

module Y : sig
  module type X = sig end [@@deriving mtd]
end = struct
  module type X = sig end
  let y = 42
end
[%%expect{|
module Y : sig module type X = sig end val y : int end
|}]
