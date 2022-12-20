(* The only difference between 5.0 and 5.1 from a Parsetree point of view are the magic numbers *)

module Asttypes = struct
  include Ast_500.Asttypes
end

module Parsetree = struct
  include Ast_500.Parsetree
end

module Config = struct
  let ast_impl_magic_number = "Caml1999M032"
  let ast_intf_magic_number = "Caml1999N032"
end
