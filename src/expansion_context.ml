type t =
  { loc : Location.t
  ; code_path : Code_path.t
  }

let make ~loc ~code_path = {loc; code_path}

let loc t = t.loc
let code_path t = t.code_path

let with_loc_and_path f =
  fun ~ctxt -> f ~loc:ctxt.loc ~path:(Code_path.to_string_path ctxt.code_path)
