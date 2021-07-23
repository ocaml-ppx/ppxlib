(* Test for the ppx_import old syntax compat support *)

open Ppxlib

let id =
  Extension.__declare_ppx_import
    "id"
    (fun ~ctxt:_ td -> td)
[%%expect{|
val id : Extension.t = <abstr>
|}]

Driver.register_transformation
  ~rules:[Context_free.Rule.extension id]
  "id"
[%%expect{|
- : unit = ()
|}]

(* The expander receives the type decl with the extension point removed, it should preserve
   attibutes *)
type t = [%id: int]
[%%expect{|
type t = int
|}]

(* It also should work in signatures by default *)
module type T = sig
  type t = [%id: int]
end
[%%expect{|
module type T = sig type t = int end
|}]

let foo =
  Deriving.add "foo"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ _ -> [%str let foo = 42]))
    ~sig_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ _ -> [%sig: val foo : int]))
[%%expect{|
val foo : Deriving.t = <abstr>
|}]

(* It should properly compose with [@@deriving] *)
type t = [%id: int]
[@@deriving foo]
[%%expect{|
type t = int
val foo : t = 42
|}]

module type T = sig
  type t = [%id: int]
  [@@deriving foo]
end
[%%expect{|
module type T = sig type t = int val foo : t end
|}]

(* Extra things to test:
   - the contxt is right
   - the context for inner nodes is right as well *)
[%%expect{|
|}]
