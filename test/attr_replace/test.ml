class c__suffix = object end

class type ct__suffix = object end

type t__suffix = unit

let foo__suffix () = ()

module M__suffix = struct end

module type S__suffix = sig end

let prefix_foo_suffix = ()

[@@@expand_inline class class_ = c [@test.clx "suffix"]]

class class_ = c__suffix

[@@@end]

[@@@expand_inline
class class_field =
  object
    val foo = () [@@alert "-1"] [@@test.clf "suffix"] [@@alert "-2"]
  end]

class class_field =
  object
    val foo__suffix = () [@@alert "-1"] [@@alert "-2"]
  end

[@@@end]
[@@@expand_inline class type class_type = ct[@test.clt "suffix"]]

class type class_type = ct__suffix

[@@@end]

[@@@expand_inline
class type class_type_field = object
  val x : int [@@alert "-1"] [@@test.ctf "suffix"] [@@alert "-2"]
end]

class type class_type_field = object
  val x__suffix : int [@@alert "-1"] [@@alert "-2"]
end

[@@@end]

[@@@expand_inline
let _ : (t[@alert "-1"] [@test.typ "suffix"] [@alert "-2"]) = ()]

let _ : (t__suffix[@alert "-1"] [@alert "-2"]) = ()

[@@@end]
[@@@expand_inline let _e : (t[@test.typ "suffix"]) = ()]

let _e : t__suffix = ()

[@@@end]
[@@@expand_inline let _ = foo [@alert "-1"] [@test.exp "suffix"] [@alert "-2"]]

let _ = foo__suffix [@alert "-1"] [@alert "-2"]

[@@@end]

(* Explicit test for the ident in a function application because it acts differently due
   to "special functions". *)
[@@@expand_inline
let _ = (foo [@alert "-1"] [@test.exp "suffix"] [@alert "-2"]) ()]

let _ = (foo__suffix [@alert "-1"] [@alert "-2"]) ()

[@@@end]

[@@@expand_inline
include M [@alert "-1"] [@test.mod_exp "suffix"] [@alert "-2"]]

include M__suffix [@alert "-1"] [@alert "-2"]

[@@@end]

[@@@expand_inline
module F : S [@alert "-1"] [@test.mod_typ "suffix"] [@alert "-2"] = struct end]

module F : S__suffix [@alert "-1"] [@alert "-2"] = struct end

[@@@end]

[@@@expand_inline
let _ = match () with (a [@test.pat "suffix"]) -> ignore a__suffix]

let _ = match () with a__suffix -> ignore a__suffix

[@@@end]

[@@@expand_inline
module type S = sig
  [%%foo] [@@test.sig.ext "suffix"]
end]

module type S = sig
  val foo : unit
end

[@@@end]
[@@@expand_inline [%%foo] [@@test.str.ext "suffix"]]

let foo = ()

[@@@end]

[@@@expand_inline
module _ = struct
  "" [@@test.str.evl "suffix"]
end]

module _ = struct
  ""
end

[@@@end]

[@@@expand_inline
let _ =
  foo
  [@alert "-1"]
  [@suffix "_suffix"]
  [@alert "-2"]
  [@prefix "prefix_"]
  [@alert "-3"]]

let _ = prefix_foo_suffix [@alert "-1"] [@alert "-2"] [@alert "-3"]

[@@@end]
