let magic_length = String.length Astlib.Config.ast_impl_magic_number
let buf = Bytes.create magic_length

let len =
  set_binary_mode_in stdin true;
  input stdin buf 0 magic_length

let s = Bytes.sub_string buf 0 len
let () = Printf.printf "Magic number: %s" s
