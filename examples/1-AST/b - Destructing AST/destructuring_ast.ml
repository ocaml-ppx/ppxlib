open Ppxlib

let loc = Location.none
let one ~loc = [%expr 1]

let structure_item loc =
  let expr = one ~loc in
  Ast_builder.Default.pstr_eval ~loc expr []

let match_int_payload ~loc payload =
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc =
                    Pexp_constant
                      { pconst_desc = Pconst_integer (value, None); _ };
                  _;
                },
                _ );
          _;
        };
      ] -> (
      try Ok (value |> int_of_string)
      with Failure _ ->
        Error (Location.Error.createf ~loc "Value is not a valid integer"))
  | _ -> Error (Location.Error.createf ~loc "Wrong pattern")

let test_match_pstr_eval () =
  let structure_item = structure_item loc in
  let structure = [ structure_item ] in
  match match_int_payload ~loc (PStr structure) with
  | Ok _ -> Printf.printf "\nMatched 1 using Ast_pattern"
  | Error _ -> Printf.printf "\nDid not match pstr_eval"

let _ = test_match_pstr_eval ()

let match_int_payload =
  let open Ast_pattern in
  pstr (pstr_eval (pexp_constant (pconst_integer (string "1") none)) nil ^:: nil)

let test_match_pstr_eval () =
  let structure_item = structure_item loc in
  let structure = [ structure_item ] in
  try
    Ast_pattern.parse match_int_payload loc (PStr structure) Printf.printf
      "\nMatched 1 using Ast_pattern"
  with _ -> Printf.printf "\nDid not match 1 payload using Ast_pattern"

let _ = test_match_pstr_eval ()

let match_int_payload =
  let open Ast_pattern in
  pstr (pstr_eval (eint (int 1)) nil ^:: nil)

let test_match_pstr_eval () =
  let structure_item = structure_item loc in
  let structure = [ structure_item ] in
  try
    Ast_pattern.parse match_int_payload loc (PStr structure) Printf.printf
      "\nMatched 1 using Ast_patter with eint"
  with _ ->
    Printf.printf "\nDid not match 1 payload using Ast_pattern with eint"

let _ = test_match_pstr_eval ()

let match_int_payload expr =
  match expr with
  | [%expr 1] -> Ok 1
  | _ ->
      Error
        (Location.Error.createf ~loc:expr.pexp_loc
           "Value is not a valid integer")

let test_match_pstr_eval () =
  let expr = one ~loc in
  match match_int_payload expr with
  | Ok _ -> Printf.printf "\nMatched 1 using metaquot"
  | Error _ -> Printf.printf "\nDid not match 1 using metaquot"

let _ = test_match_pstr_eval ()
let let_expression = [%expr 1 + 4]

let match_int_payload expr =
  match expr with
  | [%expr 1 + [%e? e]] -> (
      match e with
      | {
       pexp_desc =
         Pexp_constant { pconst_desc = Pconst_integer (value, None); _ };
       _;
      } ->
          Ok (1 + int_of_string value)
      | _ ->
          Error
            (Location.Error.createf ~loc:e.pexp_loc
               "Value is not a valid integer"))
  | _ -> Error (Location.Error.createf ~loc:expr.pexp_loc "Wrong pattern")

let test_match_pstr_eval () =
  match match_int_payload let_expression with
  | Ok value ->
      Printf.printf "\nMatched 1 + <int> using metaquot and anti-quotation: %s"
        (value |> string_of_int)
  | Error _ ->
      Printf.printf
        "\nDid not match matched 1 + <int> using metaquot and anti-quotation"

let _ = test_match_pstr_eval ()
