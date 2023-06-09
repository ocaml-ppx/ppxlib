The 501 parsetree contains a parsing modificacion.
[compare_on.exe <file>] checks if there's a diff between the
AST's resulting from
1. parsing <file> on 5.1.0 directly
2. parsing <file> on 5.1.0, migrating down to 5.0.0 and migrating back to 5.1.0
We only expect a diff in one special case.

  $ echo "let x : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let _ : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

Here might be a problem in the upward migration: the 5.1.0 parser parses the constraint as a pattern constraint.
However, the upward migration makes a value binding constraint out of it.
  $ echo "let ((x,y) : (int*int)) = (assert false: int * int)" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
  6,25c6,23
  <         pattern (file.ml[1,0+4]..[1,0+23])
  <           Ppat_constraint
  <           pattern (file.ml[1,0+5]..[1,0+10])
  <             Ppat_tuple
  <             [
  <               pattern (file.ml[1,0+6]..[1,0+7])
  <                 Ppat_var "x" (file.ml[1,0+6]..[1,0+7])
  <               pattern (file.ml[1,0+8]..[1,0+9])
  <                 Ppat_var "y" (file.ml[1,0+8]..[1,0+9])
  <             ]
  <           core_type (file.ml[1,0+14]..[1,0+21])
  <             Ptyp_tuple
  <             [
  <               core_type (file.ml[1,0+14]..[1,0+17])
  <                 Ptyp_constr "int" (file.ml[1,0+14]..[1,0+17])
  <                 []
  <               core_type (file.ml[1,0+18]..[1,0+21])
  <                 Ptyp_constr "int" (file.ml[1,0+18]..[1,0+21])
  <                 []
  <             ]
  ---
  >         pattern (file.ml[1,0+5]..[1,0+10])
  >           Ppat_tuple
  >           [
  >             pattern (file.ml[1,0+6]..[1,0+7])
  >               Ppat_var "x" (file.ml[1,0+6]..[1,0+7])
  >             pattern (file.ml[1,0+8]..[1,0+9])
  >               Ppat_var "y" (file.ml[1,0+8]..[1,0+9])
  >           ]
  >         core_type (file.ml[1,0+14]..[1,0+21])
  >           Ptyp_tuple
  >           [
  >             core_type (file.ml[1,0+14]..[1,0+17])
  >               Ptyp_constr "int" (file.ml[1,0+14]..[1,0+17])
  >               []
  >             core_type (file.ml[1,0+18]..[1,0+21])
  >               Ptyp_constr "int" (file.ml[1,0+18]..[1,0+21])
  >               []
  >           ]

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
  compile error after migrations
  Fatal error: exception File "astlib/migrate_500_501.ml", line 290, characters 9-15: Assertion failed
  File "file.ml", line 1:
  Error: Error while running external preprocessor
  Command line: ./identity_driver.exe -as-ppx '/tmp/build_f26de1_dune/camlppx64fd0f' '/tmp/build_f26de1_dune/camlppxe1d493'
  

Here we expect a diff (downwards migrating should yield the same as in the example right above).
However, something is wrong.
  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe
