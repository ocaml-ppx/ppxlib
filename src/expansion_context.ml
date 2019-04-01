module Extension = struct
  type t =
    { extension_point_loc : Location.t
    ; code_path : Code_path.t
    }

  let make ~extension_point_loc ~code_path = {extension_point_loc; code_path}

  let extension_point_loc t = t.extension_point_loc
  let code_path t = t.code_path

  let with_loc_and_path f =
    fun ~ctxt -> f ~loc:ctxt.extension_point_loc ~path:(Code_path.to_string_path ctxt.code_path)
end

module Deriver = struct
  type t =
    { derived_item_loc : Location.t
    ; code_path : Code_path.t
    }

  let make ~derived_item_loc ~code_path = {derived_item_loc; code_path}

  let derived_item_loc t = t.derived_item_loc
  let code_path t = t.code_path

  let with_loc_and_path f =
    fun ~ctxt -> f ~loc:ctxt.derived_item_loc ~path:(Code_path.to_string_path ctxt.code_path)
end
