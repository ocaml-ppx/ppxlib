let one = [%one]
let _ = Printf.printf "One: %d\n" one
let grin = [%emoji "grin"]
let smiley = [%emoji "smiley"]
let _ = print_endline ("grin: " ^ grin)
let _ = print_endline ("smiley: " ^ smiley)

(* enum with raise *)
let _ = print_endline "\n# Enum with raise"

type game = Rock | Paper | Scissors [@@deriving enum]

let _ = Printf.printf "Rock to string: %s\n" (game_to_string Rock)

let _ =
  Printf.printf "Paper to string: %s\n"
    (game_to_string (game_from_string "Paper"))

let _ =
  try
    Printf.printf "Stick to string: %s\n"
      (game_to_string (game_from_string "Stick"))
  with _ -> Printf.printf "Stick is not a valid value\n"

(* enum with option *)
let _ = print_endline "\n# Enum with option"

type game2 = Rock | Paper | Scissors [@@deriving enum2 ~opt]

let _ = Printf.printf "Rock to string: %s\n" (game2_to_string Rock)

let _ =
  match game2_from_string "Paper" with
  | Some value -> Printf.printf "Paper to string: %s\n" (game2_to_string value)
  | None -> Printf.printf "Paper is not a valid value\n"

let _ =
  match game2_from_string "Stick" with
  | Some value -> Printf.printf "Stick to string: %s\n" (game2_to_string value)
  | None -> Printf.printf "Stick is not a valid value\n"

(* Uncomment the code bellow to see the error *)
(* type bar = string [@@deriving enum2] *)
