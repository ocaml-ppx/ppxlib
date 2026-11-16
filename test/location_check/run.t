The locations check accepts code straight from the parser, including
constructs where the parser's own locations overlap.

Let operators: the parser gives the first binding operator the location of
the whole expression, and the following ones a location starting at the
first binding (#347).

  $ cat > letop.ml << EOF
  > let ( let+ ) x f = f x
  > let ( and+ ) a b = (a, b)
  > let _ = let+ () = () in ()
  > let _ = let+ () = () and+ () = () and+ () = () in ()
  > let _ = let+ x = let+ y = 1 in y and+ z = 2 in x + z
  > EOF
  $ ./identity_driver.exe -check -locations-check letop.ml > /dev/null

Opened patterns: in M.[], the parser gives the opened pattern the location of
the whole pattern (#367).

  $ cat > open_pattern.ml << EOF
  > module M = struct
  >   type t = [] | ( :: ) of int * t
  >   type r = { x : int }
  > end
  > let _ = match ([] : M.t) with M.[] -> () | M.(_ :: _) -> ()
  > let _ = match [||] with M.[||] -> () | _ -> ()
  > let _ = match [||] with M.[|_|] -> () | _ -> ()
  > let _ = match ({ x = 1 } : M.r) with M.{ x } -> x
  > let _ = match 1 with M.(1) -> () | _ -> ()
  > EOF
  $ ./identity_driver.exe -check -locations-check open_pattern.ml > /dev/null
