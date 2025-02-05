Test `expand_inline` for structures.

  $ cat << 'EOF' > program.ml
  > [@@@expand_inline
  >   module T : sig
  >     val foo : [%str]
  >   end = struct
  >     let foo = [%suffix "apples"]
  >   end]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 7, characters 0-1:
   [@@@expand_inline
     module T : sig
       val foo : [%str]
     end = struct
       let foo = [%suffix "apples"]
     end]
  +module T : sig val foo : string end = struct let foo = "apples_suffix" end
   [@@@end]
  [1]

Test `expand_inline` for signatures.

  $ cat << 'EOF' > program.ml
  > module type S = sig
  >   [@@@expand_inline:
  >     val foo : [%str]
  >     include module type of struct
  >       let foo = [%suffix "apples"]
  >     end]
  >   [@@@end]
  > end 
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 7, characters 0-1:
   module type S = sig
     [@@@expand_inline:
       val foo : [%str]
       include module type of struct
         let foo = [%suffix "apples"]
       end]
  -  [@@@end]
  +  
  +val foo : string
  +include module type of struct let foo = "apples_suffix" end
  +[@@@end]
   end 
  [1]
