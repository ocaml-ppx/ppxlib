type t =
  { x : int
  ; y : u
  }

and u = A of int | B of t
[@@deriving_inline traverse]
class virtual map =
  object (self)
    method virtual  int : int -> int
    method t : t -> t=
      fun { x; y }  -> let x = self#int x  in let y = self#u y  in { x; y }
    method u : u -> u=
      fun x  ->
        match x with
        | A a -> let a = self#int a  in A a
        | B a -> let a = self#t a  in B a
  end
class virtual iter =
  object (self)
    method virtual  int : int -> unit
    method t : t -> unit= fun { x; y }  -> self#int x; self#u y
    method u : u -> unit=
      fun x  -> match x with | A a -> self#int a | B a -> self#t a
  end
class virtual ['acc] fold =
  object (self)
    method virtual  int : int -> 'acc -> 'acc
    method t : t -> 'acc -> 'acc=
      fun { x; y }  ->
        fun acc  ->
          let acc = self#int x acc  in let acc = self#u y acc  in acc
    method u : u -> 'acc -> 'acc=
      fun x  ->
        fun acc  ->
          match x with | A a -> self#int a acc | B a -> self#t a acc
  end
class virtual ['acc] fold_map =
  object (self)
    method virtual  int : int -> 'acc -> (int * 'acc)
    method t : t -> 'acc -> (t * 'acc)=
      fun { x; y }  ->
        fun acc  ->
          let (x,acc) = self#int x acc  in
          let (y,acc) = self#u y acc  in ({ x; y }, acc)
    method u : u -> 'acc -> (u * 'acc)=
      fun x  ->
        fun acc  ->
          match x with
          | A a -> let (a,acc) = self#int a acc  in ((A a), acc)
          | B a -> let (a,acc) = self#t a acc  in ((B a), acc)
  end
class virtual ['ctx] map_with_context =
  object (self)
    method virtual  int : 'ctx -> int -> int
    method t : 'ctx -> t -> t=
      fun ctx  ->
        fun { x; y }  ->
          let x = self#int ctx x  in let y = self#u ctx y  in { x; y }
    method u : 'ctx -> u -> u=
      fun ctx  ->
        fun x  ->
          match x with
          | A a -> let a = self#int ctx a  in A a
          | B a -> let a = self#t ctx a  in B a
  end
class virtual ['res] lift =
  object (self)
    method virtual  record : (string * 'res) list -> 'res
    method virtual  constr : string -> 'res list -> 'rest
    method virtual  int : int -> 'res
    method t : t -> 'res=
      fun { x; y }  ->
        let x = self#int x  in
        let y = self#u y  in self#record [("x", x); ("y", y)]
    method u : u -> 'res=
      fun x  ->
        match x with
        | A a -> let a = self#int a  in self#constr "A" [a]
        | B a -> let a = self#t a  in self#constr "B" [a]
  end
[@@@end]

let () = print_endline "OK"
