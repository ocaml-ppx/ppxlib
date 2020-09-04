type t = Ocaml_common.Location.t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
  loc_ghost : bool;
}

type 'a loc = 'a Ocaml_common.Location.loc = { txt : 'a; loc : t }

module Error : sig
  type location

  type t

  val is_well_formed : t -> bool

  val main_msg : t -> string loc

  val sub_msgs : t -> string loc list

  val set_main_msg : t -> string -> t

  val make : sub:string loc list -> string loc -> t

  val update_loc : t -> location -> t

  val of_exn : exn -> t option
end
with type location := t

val set_input_name : string -> unit

val none : t

(** {1 Automatically reporting errors for raised exceptions} *)

val register_error_of_exn : (exn -> Error.t option) -> unit
(** Each compiler module which defines a custom type of exception which can
    surface as a user-visible error should register a "printer" for this
    exception using [register_error_of_exn]. The result of the printer is an
    [error] value containing a location, a message, and optionally sub-messages
    (each of them being located as well). *)

val raise_errorf : ?loc:t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val report_exception : Format.formatter -> exn -> unit
(** Reraise the exception if it is unknown. *)

exception Error of Error.t
(** Raising [Error e] signals an error [e]; the exception will be caught and the
    error will be printed. *)
