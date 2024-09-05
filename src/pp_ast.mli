open! Import

module Config : sig
  type t
  (** Type for AST pretty-printing config *)

  val make :
    ?show_attrs:bool ->
    ?show_locs:bool ->
    ?loc_mode:[ `Short | `Full ] ->
    unit ->
    t
  (** Create a custom pretty-printing config.

      Default values are the ones that are used when no configuration is passed
      to the pretty-printers defined below.

      @param show_attrs
        controls whether attributes are shown or hidden. It defaults to [false].
        When set to [true], records such as [expression] that have a [desc]
        field will only be printed if the list of attributes is non-empty,
        otherwise their [_desc] field will be printed directly instead, as it is
        the case when [show_attrs] is [false].

      @param show_loc
        controls whether locations are shown or hidden. Defaults to [false].

      @param loc_mode
        controls how locations are shown if they are shown at
        all.
        - When set to [`Short], locations are displayed as ["l1c6..l2c2"] for
          multiline locations and as ["l1c6..12"] for single line locations.
          Ghost locations are suffixed with a ["(g)"].
        - When set to [`Full], locations are displayed as any other record would
          be. Defaults to [`Short]. *)
end

type 'node pp = ?config:Config.t -> Format.formatter -> 'node -> unit

val structure : structure pp
val signature : signature pp
val expression : expression pp
val pattern : pattern pp
val core_type : core_type pp
