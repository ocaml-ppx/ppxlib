The 501 parsetree contains a parsing modificacion.
[compare_on.exe <file> ./reverse_migrations.exe] checks if there's a diff between the
AST's resulting from
1. parsing <file> on 5.0.0 directly
2. parsing <file> on 5.0.0, migrating up to 5.1.0 and migrating back to 5.0.0

  $ echo "let x : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+11]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+8]..[1,0+11]) ghost
  ---
  >           core_type (file.ml[1,0+8]..[1,0+11])
  15c15
  <         expression (file.ml[1,0+4]..[1,0+15])
  ---
  >         expression (file.ml[1,0+14]..[1,0+15])

  $ echo "let _ : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+11]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+31]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+4]..[1,0+57]) ghost
  ---
  >           core_type (file.ml[1,0+20]..[1,0+31])
  24c24
  <         expression (file.ml[1,0+4]..[1,0+57])
  ---
  >         expression (file.ml[1,0+34]..[1,0+57])
  26c26
  <           expression (file.ml[1,0+4]..[1,0+57])
  ---
  >           expression (file.ml[1,0+34]..[1,0+57])
  28c28
  <             expression (file.ml[1,0+4]..[1,0+57])
  ---
  >             expression (file.ml[1,0+34]..[1,0+57])
  30c30
  <               expression (file.ml[1,0+4]..[1,0+57])
  ---
  >               expression (file.ml[1,0+34]..[1,0+57])

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+23]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+4]..[1,0+36]) ghost
  ---
  >           core_type (file.ml[1,0+17]..[1,0+23])
  19c19
  <         expression (file.ml[1,0+4]..[1,0+36])
  ---
  >         expression (file.ml[1,0+26]..[1,0+36])
  21c21
  <           expression (file.ml[1,0+4]..[1,0+36])
  ---
  >           expression (file.ml[1,0+26]..[1,0+36])

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+24]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+10])

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+28]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+4]..[1,0+43]) ghost
  ---
  >           core_type (file.ml[1,0+15]..[1,0+28])
  23c23
  <         expression (file.ml[1,0+4]..[1,0+43])
  ---
  >         expression (file.ml[1,0+31]..[1,0+43])
  25c25
  <           expression (file.ml[1,0+4]..[1,0+43])
  ---
  >           expression (file.ml[1,0+31]..[1,0+43])

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+18]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+4]..[1,0+33]) ghost
  ---
  >           core_type (file.ml[1,0+17]..[1,0+18])
  14c14
  <         expression (file.ml[1,0+4]..[1,0+33])
  ---
  >         expression (file.ml[1,0+21]..[1,0+33])
  16c16
  <           expression (file.ml[1,0+4]..[1,0+33])
  ---
  >           expression (file.ml[1,0+21]..[1,0+33])

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+18]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+9]..[1,0+18]) ghost
  ---
  >           core_type (file.ml[1,0+9]..[1,0+18])
  21c21
  <         expression (file.ml[1,0+4]..[1,0+23])
  ---
  >         expression (file.ml[1,0+21]..[1,0+23])

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+25]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+16]..[1,0+25]) ghost
  ---
  >           core_type (file.ml[1,0+16]..[1,0+25])
  21c21
  <         expression (file.ml[1,0+4]..[1,0+30])
  ---
  >         expression (file.ml[1,0+28]..[1,0+30])

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+17]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+8]..[1,0+17]) ghost
  ---
  >           core_type (file.ml[1,0+8]..[1,0+17])
  21c21
  <         expression (file.ml[1,0+4]..[1,0+44])
  ---
  >         expression (file.ml[1,0+20]..[1,0+44])

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+33]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+26]..[1,0+33]) ghost
  ---
  >           core_type (file.ml[1,0+26]..[1,0+33])
  18c18
  <         expression (file.ml[1,0+4]..[1,0+72])
  ---
  >         expression (file.ml[1,0+36]..[1,0+72])

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe
  6c6
  <         pattern (file.ml[1,0+4]..[1,0+16]) ghost
  ---
  >         pattern (file.ml[1,0+4]..[1,0+5])
  10c10
  <           core_type (file.ml[1,0+9]..[1,0+16]) ghost
  ---
  >           core_type (file.ml[1,0+9]..[1,0+16])
  18c18
  <         expression (file.ml[1,0+4]..[1,0+55])
  ---
  >         expression (file.ml[1,0+19]..[1,0+55])

The diffs are on locations. If the location modification
at least preserved the location invariants, it might be acceptable.
However, in several cases it doesn't.

  $ echo "let x : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let (x as y) : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
  File "file.ml", line 1, characters 4-12:
  1 | let (x as y) : int = 5
          ^^^^^^^^
  Error: invalid output from ppx:
         this pattern is built from a core type whose location is outside of this node's.
  Child core type found at:
  File "file.ml", line 1, characters 15-18:
  [1]

  $ cat > file.ml << EOF
  > type t = {a : int}
  > let {a} = {a = 5}
  > EOF
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null


  $ echo "let _ : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
  File "file.ml", line 1, characters 4-5:
  1 | let _ : int = 5
          ^
  Error: invalid output from ppx:
         this pattern is built from a core type whose location is outside of this node's.
  Child core type found at:
  File "file.ml", line 1, characters 8-11:
  [1]

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
  File "file.ml", line 1, characters 4-10:
  1 | let (x, y) : (int * int) = assert false
          ^^^^^^
  Error: invalid output from ppx:
         this pattern is built from a core type whose location is outside of this node's.
  Child core type found at:
  File "file.ml", line 1, characters 14-23:
  [1]

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
  File "file.ml", line 1, characters 4-5:
  1 | let x : [`A] :> [`A | `B] = `A
          ^
  Error: invalid output from ppx:
         this pattern is built from a core type whose location is outside of this node's.
  Child core type found at:
  File "file.ml", line 1, characters 16-25:
  [1]

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
  File "file.ml", line 1, characters 4-5:
  1 | let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end
          ^
  Error: invalid output from ppx:
         this pattern is built from a core type whose location is outside of this node's.
  Child core type found at:
  File "file.ml", line 1, characters 26-33:
  [1]

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null
