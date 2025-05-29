class c__suffix = object  end
class type ct__suffix = object  end
type t__suffix = unit
let foo__suffix () = ()
module M__suffix = struct  end
module type S__suffix  = sig  end
let prefix_foo_suffix = ()
class class_ = c__suffix
class class_field =
  object val foo__suffix = ()[@@alert "-1"][@@alert "-2"] end
class type class_type = ct__suffix
class type class_type_field =
  object val  x__suffix : int[@@alert "-1"][@@alert "-2"] end
let _ = ()
let _e = ()
let _ = ((foo__suffix)[@alert "-1"][@alert "-2"])
let _ = ((foo__suffix)[@alert "-1"][@alert "-2"]) ()
include ((M__suffix)[@alert "-1"][@alert "-2"])
module F : ((S__suffix)[@alert "-1"][@alert "-2"]) = struct  end 
let _ = match () with | a__suffix -> ignore a__suffix
module type S  = sig val foo : unit end
let foo = ()
module _ = struct ;;"" end
let _ = ((prefix_foo_suffix)[@alert "-1"][@alert "-2"][@alert "-3"])
