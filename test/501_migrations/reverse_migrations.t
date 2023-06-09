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

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+24]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+23]) ghost

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+15]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])

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

The diffs are on locations. If the location modification
at least preserved the location invariants, it might be acceptable.
However, in several cases it doesn't.

  $ echo "let x : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let (x as y) : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ cat > file.ml << EOF
  > type t = {a : int}
  > let {a} = {a = 5}
  > EOF
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null


  $ echo "let _ : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
  File "file.ml", line 1, characters 4-5:
  1 | let f : 'a . 'a = (fun (type a) -> (assert false : a))
          ^
  Error: invalid output from ppx:
         this pattern is built from a core type whose location is outside of this node's.
  Child core type found at:
  File "file.ml", line 1, characters 13-15:
  [1]

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
