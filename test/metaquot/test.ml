let loc = Ppxlib.Location.none
[%%expect{|
val loc : Warnings.loc =
  {Ppxlib.Location.loc_start =
    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
   loc_end =
    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
   loc_ghost = true}
|}]

(* unannotated quotations *)

let _ = [%expr ()]
[%%expect{|
Line _, characters 8-18:
Error: Unbound record field pexp_desc
|}]

let _ = [%pat? ()]
[%%expect{|
Line _, characters 8-18:
Error: Unbound record field ppat_desc
|}]

let _ = [%type: unit]
[%%expect{|
Line _, characters 8-21:
Error: Unbound record field ptyp_desc
|}]

let _ = [%stri let _ = ()]
[%%expect{|
Line _, characters 8-26:
Error: Unbound record field pstr_desc
|}]

let _ = [%sigi: include S]
[%%expect{|
Line _, characters 8-26:
Error: Unbound record field psig_desc
|}]

let _ = [%str let _ = ()]
[%%expect{|
Line _, characters 8-25:
Error: Unbound record field pstr_desc
|}]

let _ = [%sig: include S]
[%%expect{|
Line _, characters 8-25:
Error: Unbound record field psig_desc
|}]

(* mistyped escapes (not producing ASTs at all) *)

let _ = [%expr [%e ()]]
[%%expect{|
- : unit = ()
|}]

let _ = [%pat? [%p ()]]
[%%expect{|
- : unit = ()
|}]

let _ = [%type: [%t ()]]
[%%expect{|
- : unit = ()
|}]

let _ = [%stri [%%i ()]]
[%%expect{|
- : unit = ()
|}]

let _ = [%sigi: [%%i ()]]
[%%expect{|
- : unit = ()
|}]
