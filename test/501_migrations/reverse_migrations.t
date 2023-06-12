The 501 parsetree contains a parsing modificacion.
[compare_on.exe <file> ./reverse_migrations.exe] checks if there's a diff between the
AST's resulting from
1. parsing <file> on 5.0.0 directly
2. parsing <file> on 5.0.0, migrating up to 5.1.0 and migrating back to 5.0.0

  $ echo "let x : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let _ : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

The downward migration isn't able to recover the whole pattern location range,
since it doesn't track the location of the closing brackets.
  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+24]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+23]) ghost

Here we're expecting a similar location diff as above. However, the downward
migration is faulty: it turns [let (x) : int = 5] (constraint only on pattern)
into [let x : int = 5] (contraint on both pattern an expression).
  $ echo "let (x) : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  9a10,18
  >           core_type (file.ml[1,0+10]..[1,0+13]) ghost
  >             Ptyp_poly
  >             core_type (file.ml[1,0+10]..[1,0+13])
  >               Ptyp_constr "int" (file.ml[1,0+10]..[1,0+13])
  >               []
  >         expression (file.ml[1,0+4]..[1,0+17])
  >           Pexp_constraint
  >           expression (file.ml[1,0+16]..[1,0+17])
  >             Pexp_constant PConst_int (5,None)
  13,14d21
  <         expression (file.ml[1,0+16]..[1,0+17])
  <           Pexp_constant PConst_int (5,None)

Let's make sure that in the examples with diffs,
the location invariants are still fulfilled.

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let (x) : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
