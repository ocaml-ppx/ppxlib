(** Type of expansion contexts *)
type t

(** Build a new expansion context with the given location and code path *)
val make : loc:Location.t -> code_path:Code_path.t -> t

(** Return the location for the given expansion context *)
val loc : t -> Location.t

(** Return the code path for the given expansion context *)
val code_path : t -> Code_path.t

(** Wrap a [fun ~loc ~path] into a [fun ~ctxt] *)
val with_loc_and_path : (loc:Location.t -> path:string -> 'a) -> (ctxt:t -> 'a)
