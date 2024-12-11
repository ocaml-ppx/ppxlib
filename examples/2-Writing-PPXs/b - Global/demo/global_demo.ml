let demo_name = "Global Demo"
let _ = demo_name

(* Uncomment the code bellow to see the lint error *)
(* let name = "John Doe" *)

(* module enum *)
let _ = print_endline "\n# Enum"

module GameEnum = struct
  type t = Rock | Paper | Scissors
end [@enum]

let _ = print_endline (GameEnum.to_string Rock)
let _ = print_endline (GameEnum.to_string (GameEnum.from_string "Paper"))

let _ =
  try
    Printf.printf "Stick to string: %s\n"
      (GameEnum.to_string (GameEnum.from_string "Stick"))
  with _ -> Printf.printf "Stick is not a valid value\n"

(* module enum *)
let _ = print_endline "\n# Enum with option"

module GameEnum2 = struct
  type t = Rock | Paper | Scissors
end [@enum2 opt]

let _ = print_endline (GameEnum2.to_string Rock)

let _ =
  match GameEnum2.from_string "Paper" with
  | Some value ->
      Printf.printf "Paper to string: %s\n" (GameEnum2.to_string value)
  | None -> Printf.printf "Paper is not a valid value\n"

let _ =
  match GameEnum2.from_string "Stick" with
  | Some value ->
      Printf.printf "Stick to string: %s\n" (GameEnum2.to_string value)
  | None -> Printf.printf "Stick is not a valid value\n"

(* Uncomment the code bellow to see the error *)
(* module GameEnumError = struct
  type _t = Rock | Paper | Scissors

  module GameEnum = struct
    type t = Rock | Paper | Scissors
  end [@enum]
end [@enum] *)
