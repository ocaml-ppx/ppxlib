The 501 parsetree contains a parsing modificacion.
[compare_on.exe <file>] checks if there's a diff between the
AST's resulting from
1. parsing <file> on 5.1.0 directly
2. parsing <file> on 5.1.0, migrating down to 5.0.0 and migrating back to 5.1.0
We only expect a diff in one special case.

  $ echo "let x : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

Here we expect a diff (downwards migrating should yield the same as in the example right above).
However, something is wrong.
  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

Coercion support isn't implemented yet.
  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
  7,18c7,20
  <           Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  <         <coercion>
  <         None
  <         core_type (file.ml[1,0+9]..[1,0+18])
  <           Ptyp_variant closed=Closed
  <           [
  <             Rtag "A" true
  <               []
  <             Rtag "B" true
  <               []
  <           ]
  <           None
  ---
  >           Ppat_constraint
  >           pattern (file.ml[1,0+4]..[1,0+5])
  >             Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  >           core_type (file.ml[1,0+9]..[1,0+18])
  >             Ptyp_poly
  >             core_type (file.ml[1,0+9]..[1,0+18])
  >               Ptyp_variant closed=Closed
  >               [
  >                 Rtag "A" true
  >                   []
  >                 Rtag "B" true
  >                   []
  >               ]
  >               None
  20c22,25
  <           Pexp_variant "A"
  ---
  >           Pexp_coerce
  >           expression (file.ml[1,0+21]..[1,0+23])
  >             Pexp_variant "A"
  >             None
  21a27,35
  >           core_type (file.ml[1,0+9]..[1,0+18])
  >             Ptyp_variant closed=Closed
  >             [
  >               Rtag "A" true
  >                 []
  >               Rtag "B" true
  >                 []
  >             ]
  >             None

Coercion support isn't implemented yet.
  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
  7,10c7,34
  <           Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  <         <coercion>
  <         Some
  <           core_type (file.ml[1,0+8]..[1,0+12])
  ---
  >           Ppat_constraint
  >           pattern (file.ml[1,0+4]..[1,0+5])
  >             Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  >           core_type (file.ml[1,0+16]..[1,0+25])
  >             Ptyp_poly
  >             core_type (file.ml[1,0+16]..[1,0+25])
  >               Ptyp_variant closed=Closed
  >               [
  >                 Rtag "A" true
  >                   []
  >                 Rtag "B" true
  >                   []
  >               ]
  >               None
  >         expression (file.ml[1,0+28]..[1,0+30])
  >           Pexp_coerce
  >           expression (file.ml[1,0+28]..[1,0+30])
  >             Pexp_variant "A"
  >             None
  >           Some
  >             core_type (file.ml[1,0+8]..[1,0+12])
  >               Ptyp_variant closed=Closed
  >               [
  >                 Rtag "A" true
  >                   []
  >               ]
  >               None
  >           core_type (file.ml[1,0+16]..[1,0+25])
  14a39,40
  >               Rtag "B" true
  >                 []
  17,28d42
  <         core_type (file.ml[1,0+16]..[1,0+25])
  <           Ptyp_variant closed=Closed
  <           [
  <             Rtag "A" true
  <               []
  <             Rtag "B" true
  <               []
  <           ]
  <           None
  <         expression (file.ml[1,0+28]..[1,0+30])
  <           Pexp_variant "A"
  <           None

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

Coercion support isn't implemented yet.
  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
  7,10c7,56
  <           Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  <         <coercion>
  <         Some
  <           core_type (file.ml[1,0+8]..[1,0+22])
  ---
  >           Ppat_constraint
  >           pattern (file.ml[1,0+4]..[1,0+5])
  >             Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  >           core_type (file.ml[1,0+26]..[1,0+33])
  >             Ptyp_poly
  >             core_type (file.ml[1,0+26]..[1,0+33])
  >               Ptyp_object Closed
  >                 method m
  >                   core_type (file.ml[1,0+29]..[1,0+32])
  >                     Ptyp_constr "int" (file.ml[1,0+29]..[1,0+32])
  >                     []
  >         expression (file.ml[1,0+36]..[1,0+72])
  >           Pexp_coerce
  >           expression (file.ml[1,0+36]..[1,0+72])
  >             Pexp_object
  >             class_structure
  >               pattern (file.ml[1,0+42]..[1,0+42]) ghost
  >                 Ppat_any
  >               [
  >                 class_field (file.ml[1,0+43]..[1,0+55])
  >                   Pcf_method Public
  >                     "m" (file.ml[1,0+50]..[1,0+51])
  >                     Concrete Fresh
  >                     expression (file.ml[1,0+54]..[1,0+55]) ghost
  >                       Pexp_poly
  >                       expression (file.ml[1,0+54]..[1,0+55])
  >                         Pexp_constant PConst_int (0,None)
  >                       None
  >                 class_field (file.ml[1,0+56]..[1,0+68])
  >                   Pcf_method Public
  >                     "n" (file.ml[1,0+63]..[1,0+64])
  >                     Concrete Fresh
  >                     expression (file.ml[1,0+67]..[1,0+68]) ghost
  >                       Pexp_poly
  >                       expression (file.ml[1,0+67]..[1,0+68])
  >                         Pexp_constant PConst_int (1,None)
  >                       None
  >               ]
  >           Some
  >             core_type (file.ml[1,0+8]..[1,0+22])
  >               Ptyp_object Closed
  >                 method m
  >                   core_type (file.ml[1,0+11]..[1,0+14])
  >                     Ptyp_constr "int" (file.ml[1,0+11]..[1,0+14])
  >                     []
  >                 method n
  >                   core_type (file.ml[1,0+18]..[1,0+21])
  >                     Ptyp_constr "int" (file.ml[1,0+18]..[1,0+21])
  >                     []
  >           core_type (file.ml[1,0+26]..[1,0+33])
  13,18c59,60
  <                 core_type (file.ml[1,0+11]..[1,0+14])
  <                   Ptyp_constr "int" (file.ml[1,0+11]..[1,0+14])
  <                   []
  <               method n
  <                 core_type (file.ml[1,0+18]..[1,0+21])
  <                   Ptyp_constr "int" (file.ml[1,0+18]..[1,0+21])
  ---
  >                 core_type (file.ml[1,0+29]..[1,0+32])
  >                   Ptyp_constr "int" (file.ml[1,0+29]..[1,0+32])
  20,50d61
  <         core_type (file.ml[1,0+26]..[1,0+33])
  <           Ptyp_object Closed
  <             method m
  <               core_type (file.ml[1,0+29]..[1,0+32])
  <                 Ptyp_constr "int" (file.ml[1,0+29]..[1,0+32])
  <                 []
  <         expression (file.ml[1,0+36]..[1,0+72])
  <           Pexp_object
  <           class_structure
  <             pattern (file.ml[1,0+42]..[1,0+42]) ghost
  <               Ppat_any
  <             [
  <               class_field (file.ml[1,0+43]..[1,0+55])
  <                 Pcf_method Public
  <                   "m" (file.ml[1,0+50]..[1,0+51])
  <                   Concrete Fresh
  <                   expression (file.ml[1,0+54]..[1,0+55]) ghost
  <                     Pexp_poly
  <                     expression (file.ml[1,0+54]..[1,0+55])
  <                       Pexp_constant PConst_int (0,None)
  <                     None
  <               class_field (file.ml[1,0+56]..[1,0+68])
  <                 Pcf_method Public
  <                   "n" (file.ml[1,0+63]..[1,0+64])
  <                   Concrete Fresh
  <                   expression (file.ml[1,0+67]..[1,0+68]) ghost
  <                     Pexp_poly
  <                     expression (file.ml[1,0+67]..[1,0+68])
  <                       Pexp_constant PConst_int (1,None)
  <                     None
  <             ]

Coercion support isn't implemented yet.
  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
  7,15c7,17
  <           Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  <         <coercion>
  <         None
  <         core_type (file.ml[1,0+9]..[1,0+16])
  <           Ptyp_object Closed
  <             method m
  <               core_type (file.ml[1,0+12]..[1,0+15])
  <                 Ptyp_constr "int" (file.ml[1,0+12]..[1,0+15])
  <                 []
  ---
  >           Ppat_constraint
  >           pattern (file.ml[1,0+4]..[1,0+5])
  >             Ppat_var "x" (file.ml[1,0+4]..[1,0+5])
  >           core_type (file.ml[1,0+9]..[1,0+16])
  >             Ptyp_poly
  >             core_type (file.ml[1,0+9]..[1,0+16])
  >               Ptyp_object Closed
  >                 method m
  >                   core_type (file.ml[1,0+12]..[1,0+15])
  >                     Ptyp_constr "int" (file.ml[1,0+12]..[1,0+15])
  >                     []
  17,40c19,51
  <           Pexp_object
  <           class_structure
  <             pattern (file.ml[1,0+25]..[1,0+25]) ghost
  <               Ppat_any
  <             [
  <               class_field (file.ml[1,0+26]..[1,0+38])
  <                 Pcf_method Public
  <                   "m" (file.ml[1,0+33]..[1,0+34])
  <                   Concrete Fresh
  <                   expression (file.ml[1,0+37]..[1,0+38]) ghost
  <                     Pexp_poly
  <                     expression (file.ml[1,0+37]..[1,0+38])
  <                       Pexp_constant PConst_int (0,None)
  <                     None
  <               class_field (file.ml[1,0+39]..[1,0+51])
  <                 Pcf_method Public
  <                   "n" (file.ml[1,0+46]..[1,0+47])
  <                   Concrete Fresh
  <                   expression (file.ml[1,0+50]..[1,0+51]) ghost
  <                     Pexp_poly
  <                     expression (file.ml[1,0+50]..[1,0+51])
  <                       Pexp_constant PConst_int (1,None)
  <                     None
  <             ]
  ---
  >           Pexp_coerce
  >           expression (file.ml[1,0+19]..[1,0+55])
  >             Pexp_object
  >             class_structure
  >               pattern (file.ml[1,0+25]..[1,0+25]) ghost
  >                 Ppat_any
  >               [
  >                 class_field (file.ml[1,0+26]..[1,0+38])
  >                   Pcf_method Public
  >                     "m" (file.ml[1,0+33]..[1,0+34])
  >                     Concrete Fresh
  >                     expression (file.ml[1,0+37]..[1,0+38]) ghost
  >                       Pexp_poly
  >                       expression (file.ml[1,0+37]..[1,0+38])
  >                         Pexp_constant PConst_int (0,None)
  >                       None
  >                 class_field (file.ml[1,0+39]..[1,0+51])
  >                   Pcf_method Public
  >                     "n" (file.ml[1,0+46]..[1,0+47])
  >                     Concrete Fresh
  >                     expression (file.ml[1,0+50]..[1,0+51]) ghost
  >                       Pexp_poly
  >                       expression (file.ml[1,0+50]..[1,0+51])
  >                         Pexp_constant PConst_int (1,None)
  >                       None
  >               ]
  >           None
  >           core_type (file.ml[1,0+9]..[1,0+16])
  >             Ptyp_object Closed
  >               method m
  >                 core_type (file.ml[1,0+12]..[1,0+15])
  >                   Ptyp_constr "int" (file.ml[1,0+12]..[1,0+15])
  >                   []
