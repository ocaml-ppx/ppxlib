module Extension : sig
  (** Type of expansion contexts for extensions *)
  type t

  (** Build a new expansion context with the given extension point location and code path *)
  val make : extension_point_loc:Location.t -> code_path:Code_path.t -> t

  (** Return the location of the extension point being expanded *)
  val extension_point_loc : t -> Location.t

  (** Return the code path for the given context *)
  val code_path : t -> Code_path.t

  (** Wrap a [fun ~loc ~path] into a [fun ~ctxt] *)
  val with_loc_and_path : (loc:Location.t -> path:string -> 'a) -> (ctxt:t -> 'a)
end

module Deriver : sig
  (** Type of expansion contexts for derivers *)
  type t

  (** Build a new expansion context with the given item location and code path *)
  val make : derived_item_loc:Location.t -> code_path:Code_path.t -> t

  (** Return the location of the item to which the deriver is being applied *)
  val derived_item_loc : t -> Location.t

  (** Return the code path for the given context *)
  val code_path : t -> Code_path.t

  (** Wrap a [fun ~loc ~path] into a [fun ~ctxt] *)
  val with_loc_and_path : (loc:Location.t -> path:string -> 'a) -> (ctxt:t -> 'a)
end
