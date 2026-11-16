open Ppxlib.Location

let catch_as_compiler_exception =
  try raise_errorf ~loc:none "foo" with
  | Ocaml_common.Location.Error _ -> "caught"
  | _ -> "uncaught"
[%%expect{|
val catch_as_compiler_exception : string = "caught"
|}]

let catch_as_ppxlib_exception =
  try raise_errorf ~loc:none "foo" with
  | Error _ -> "caught"
  | _ -> "uncaught"
[%%expect{|
val catch_as_ppxlib_exception : string = "caught"
|}]

let messages error =
  let open Ppxlib in
  let sub_message = function
    | {
        pstr_desc =
          Pstr_extension
            ( ( _,
                PStr
                  [
                    {
                      pstr_desc =
                        Pstr_eval
                          ( {
                              pexp_desc = Pexp_constant (Pconst_string (msg, _, _));
                              _;
                            },
                            _ );
                      _;
                    };
                  ] ),
              _ );
        _;
      } ->
        msg
    | _ -> "unexpected sub-message"
  in
  match Location.Error.to_extension error with
  | _, PStr (_ :: sub) -> (Location.Error.message error, List.map sub_message sub)
  | _ -> (Location.Error.message error, [])
[%%expect{|
val messages : Error.t -> string * string list = <fun>
|}]

let error_with_sub_messages =
  try
    raise_errorf ~loc:none
      ~sub:[ (none, "first note"); (none, "second note") ]
      "foo %d" 42
  with Error error -> messages error
[%%expect{|
val error_with_sub_messages : string * string list =
  ("foo 42", ["first note"; "second note"])
|}]

let catch_with_sub_messages_as_compiler_exception =
  try raise_errorf ~loc:none ~sub:[ (none, "note") ] "foo" with
  | Ocaml_common.Location.Error _ -> "caught"
  | _ -> "uncaught"
[%%expect{|
val catch_with_sub_messages_as_compiler_exception : string = "caught"
|}]

let error_without_sub_messages =
  try raise_errorf ~loc:none "foo %d" 42 with
  | Error error -> messages error
[%%expect{|
val error_without_sub_messages : string * string list = ("foo 42", [])
|}]
