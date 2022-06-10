#require "base";;

open Base
open Ppxlib

let sexp_of_code_path code_path =
  Sexp.message
    "code_path"
    [ "main_module_name", sexp_of_string (Code_path.main_module_name code_path)
    ; "submodule_path", sexp_of_list sexp_of_string (Code_path.submodule_path code_path)
    ; "value", sexp_of_option sexp_of_string (Code_path.value code_path)
    ; "fully_qualified_path", sexp_of_string (Code_path.fully_qualified_path code_path)
    ]

let () =
  Driver.register_transformation "test"
    ~extensions:[
      Extension.V3.declare "code_path"
        Expression
        Ast_pattern.(pstr nil)
        (fun ~ctxt ->
           let loc = Expansion_context.Extension.extension_point_loc ctxt in
           let code_path = Expansion_context.Extension.code_path ctxt in
           Ast_builder.Default.estring ~loc
             (Sexp.to_string (sexp_of_code_path code_path)))
    ]
[%%expect{|
val sexp_of_code_path : Code_path.t -> Sexp.t = <fun>
|}]

let s =
  let module A = struct
    module A' = struct
      let a =
        let module B = struct
          module B' = struct
            let b =
              let module C = struct
                module C' = struct
                  let c = [%code_path]
                end
              end
              in C.C'.c
          end
        end
        in B.B'.b
    end
  end
  in A.A'.a
;;
[%%expect{|
val s : string =
  "(code_path(main_module_name Test)(submodule_path())(value(s))(fully_qualified_path Test.s))"
|}]

let module M = struct
  let m = [%code_path]
  end
  in
  M.m
[%%expect{|
- : string =
"(code_path(main_module_name Test)(submodule_path())(value())(fully_qualified_path Test))"
|}]
