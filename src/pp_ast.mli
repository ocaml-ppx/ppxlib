open! Import

module Config : sig
  type t
  (** Type for AST pretty-printing config *)

  val make : ?show_attrs:bool -> unit -> t
  (** Create a custom pretty-printing config.

      Default values are the ones that are used when no configuration is passed
      to the pretty-printers defined below.

      @param show_attrs
        controls whether attributes are shown or hidden. It defaults to [false].
        When set to [true], records such as [expression] that have a [desc]
        field will only be printed if the list of attributes is non-empty,
        otherwise their [_desc] field will be printed directly instead, as it is
        the case when [show_attrs] is [false]. *)
end

type 'node pp = ?config:Config.t -> Format.formatter -> 'node -> unit

val structure : structure pp
val signature : signature pp
val expression : expression pp
val pattern : pattern pp
val core_type : core_type pp
