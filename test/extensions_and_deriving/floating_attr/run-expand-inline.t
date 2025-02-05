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

Test (** ... *) comments get translated using {| |} syntax.

  $ cat << 'EOF' > program.ml
  > [@@@expand_inline 
  > module T : sig
  > (**foo*)
  > val foo : [%str]
  > end = struct
  > (**bar*)
  > let foo = [%suffix "apples"]
  > end
  > 
  > (** baz *)
  > 
  > ]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 13, characters 0-1:
   [@@@expand_inline 
   module T : sig
   (**foo*)
   val foo : [%str]
   end = struct
   (**bar*)
   let foo = [%suffix "apples"]
   end
   
   (** baz *)
   
   ]
  +module T : sig val foo : string[@@ocaml.doc {|foo|}] end =
  +  struct let foo = "apples_suffix"[@@ocaml.doc {|bar|}] end
  +[@@@ocaml.text {| baz |}]
   [@@@end]
  [1]

Test [@@ocaml.doc ...] attributes do not get swapped to using {| |}.

  $ cat << 'EOF' > program.ml
  > [@@@expand_inline 
  > module T : sig
  > val foo : [%str] [@@ocaml.doc "foo"]
  > end = struct
  > let foo = [%suffix "apples"] [@@ocaml.doc "foo"]
  > end
  > ]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 8, characters 0-1:
   [@@@expand_inline 
   module T : sig
   val foo : [%str] [@@ocaml.doc "foo"]
   end = struct
   let foo = [%suffix "apples"] [@@ocaml.doc "foo"]
   end
   ]
  +module T : sig val foo : string[@@ocaml.doc "foo"] end =
  +  struct let foo = "apples_suffix"[@@ocaml.doc "foo"] end
   [@@@end]
  [1]

Test the delim finding behaviour when translating (** ... *) comments to {| |} syntax.

  $ cat << 'EOF' > program.ml
  > [@@@expand_inline 
  > (**blah blah |} blah blah*)
  > let foo = [%suffix "apples"]
  > ]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 5, characters 0-1:
   [@@@expand_inline 
   (**blah blah |} blah blah*)
   let foo = [%suffix "apples"]
   ]
  +let foo = "apples_suffix"[@@ocaml.doc {x|blah blah |} blah blah|x}]
   [@@@end]
  [1]

  $ cat << 'EOF' > program.ml
  > [@@@expand_inline 
  > (**blxxx |} blaxxxxxxxxxh blxxahx*)
  > let foo = [%suffix "apples"]
  > ]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 5, characters 0-1:
   [@@@expand_inline 
   (**blxxx |} blaxxxxxxxxxh blxxahx*)
   let foo = [%suffix "apples"]
   ]
  +let foo = "apples_suffix"[@@ocaml.doc {x|blxxx |} blaxxxxxxxxxh blxxahx|x}]
   [@@@end]
  [1]
