open! Import

type t =
  { file_path : string
  ; main_module_name : string
  ; val_path : string list
  }

let top_level ~file_path =
  let main_module_name =
    file_path
    |> Caml.Filename.basename
    |> Caml.Filename.remove_extension
    |> String.capitalize
  in
  {file_path; main_module_name; val_path = []}

let file_path t = t.file_path
let main_module_name t = t.main_module_name
let val_path t = List.rev t.val_path

let fully_qualified_path t = 
  String.concat ~sep:"." (t.main_module_name :: (val_path t))

let enter mod_or_val_name t =
  {t with val_path = mod_or_val_name :: t.val_path}

let to_string_path t =
  let val_path = val_path t in
  let sub_module_path =
    let is_module_name s = String.(s <> "") && Char.is_uppercase (String.get s 0) in
    List.filter ~f:is_module_name val_path
  in
  String.concat ~sep:"." (t.file_path :: sub_module_path)

let with_string_path f ~loc ~path = f ~loc ~path:(to_string_path path)
