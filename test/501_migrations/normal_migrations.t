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

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe

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
