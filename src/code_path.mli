open !Import

(** Type for path to AST nodes *)
type t

(** [top_level ~file_path] returns the code path for any toplevel item in the file at [file_path]. *)
val top_level : file_path: string -> t

(** Return the path to the .ml or .mli file for this code path. *)
val file_path : t -> string

(** Return the module name corresponding to the file to which this code path leads to. *)
val main_module_name : t -> string

(** Return the path within the main module this code path represents as a list of module names and
    eventually a value name if within a value binding.
*)
val val_path : t -> string list

(** Return the fully qualified path represented by this code path as a single string, eg
    ["Some_main_module.Some_submodule.some_value"].
*)
val fully_qualified_path : t -> string

(** [enter val_or_module_name t] returns a new code path updated with the given name and location *)
val enter : loc:Location.t -> string -> t -> t

(** Return the string version of this code path as built by [Ast_traverse.map_with_path].
    Used for compatibility with path from version 0.5.0 and lower.
*)
val to_string_path : t -> string

(** Wrap a [fun ~loc ~path] expecting a string path into one expecting a [t]. *)
val with_string_path :
  (loc:Location.t -> path:string -> 'a) ->
  (loc:Location.t -> path:t -> 'a)
