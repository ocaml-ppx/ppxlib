(** Match source code against generated code *)

open! Import

val match_structure_res :
  pos:Lexing.position ->
  expected:structure ->
  mismatch_handler:(Location.t -> structure -> unit) ->
  structure ->
  (unit, Location.Error.t NonEmptyList.t) result
(** Checks that the given code starts with [expected] followed by
    [@@@deriving.end] or [@@@end].

    Returns an error if there is no [@@@deriving.end].

    If some items don't match, it calls [mismatch_handler] with the location of
    the source items and the expected code. *)

val match_structure :
  pos:Lexing.position ->
  expected:structure ->
  mismatch_handler:(Location.t -> structure -> unit) ->
  structure ->
  unit
(** See {!match_structure_res}. Raises a located error in case of error. *)

val match_signature_res :
  pos:Lexing.position ->
  expected:signature ->
  mismatch_handler:(Location.t -> signature -> unit) ->
  signature ->
  (unit, Location.Error.t NonEmptyList.t) result
(** Same for signatures *)

val match_signature :
  pos:Lexing.position ->
  expected:signature ->
  mismatch_handler:(Location.t -> signature -> unit) ->
  signature ->
  unit
(** Same for signatures *)

(** The following functions mark [@@@deriving.end] as seen. Useful when
    purposefully ignoring correction based transformations. *)

val see_end_marker_str :
  structure_item -> (unit, Location.Error.t NonEmptyList.t) result

val see_end_marker_sig :
  signature_item -> (unit, Location.Error.t NonEmptyList.t) result
