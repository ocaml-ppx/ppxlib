class c__suffix = object end

class type ct__suffix = object end

type t__suffix = unit

let foo__suffix () = ()

module M__suffix = struct end

module type S__suffix = sig end

let prefix_foo_suffix = ()

class class_ = c [@test.clx "suffix"]

class class_field =
  object
    val foo = () [@@alert "-1"] [@@test.clf "suffix"] [@@alert "-2"]
  end

class type class_type = ct[@test.clt "suffix"]

class type class_type_field = object
  val x : int [@@alert "-1"] [@@test.ctf "suffix"] [@@alert "-2"]
end

let _ : (t[@alert "-1"] [@test.typ "suffix"] [@alert "-2"]) = ()
let _e : (t[@test.typ "suffix"]) = ()
let _ = foo [@alert "-1"] [@test.exp "suffix"] [@alert "-2"]

(* Explicit test for the ident in a function application because it acts differently due
   to "special functions". *)

let _ = (foo [@alert "-1"] [@test.exp "suffix"] [@alert "-2"]) ()

include M [@alert "-1"] [@test.mod_exp "suffix"] [@alert "-2"]
module F : S [@alert "-1"] [@test.mod_typ "suffix"] [@alert "-2"] = struct end

let _ = match () with (a [@test.pat "suffix"]) -> ignore a__suffix

module type S = sig
  [%%foo] [@@test.sig.ext "suffix"]
end

[%%foo] [@@test.str.ext "suffix"]

module _ = struct
  "" [@@test.str.evl "suffix"]
end

let _ =
  foo
  [@alert "-1"]
  [@suffix "_suffix"]
  [@alert "-2"]
  [@prefix "prefix_"]
  [@alert "-3"]
