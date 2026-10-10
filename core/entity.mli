exception Empty of string
(** Raised by {!Name.of_string_exn}, {!Label.of_string_exn} and {!Extended.of_string_exn}, which
    refuse the empty string: an empty name, label or description is a value the formatters write and
    readers drop, so a collection carrying one does not round-trip. The payload names the field.

    Each of the three decodes a YAML string both ways: [t_of_yaml] raises this, [option_of_yaml]
    returns [None]. The set decoders {!Name_set.t_of_yaml}, {!Label_set.t_of_yaml} and
    {!Extended_set.t_of_yaml} go through [option_of_yaml] and drop an empty entry rather than
    raising, as every set drops a null one: both are absent (henrytill/hbt-go#73,
    henrytill/hbt-data#44). So does the value side of a mappings file, where an empty value is a
    deletion (#50). *)

module Uri : sig
  type t

  val empty : t
  val of_string : string -> t
  val to_string : t -> string
  val canonicalize : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val hash : t -> int
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Name : sig
  type t

  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val option_of_yaml : Yaml.value -> t option
  val yaml_of_t : t -> Yaml.value
end

module Name_set : Yaml_set.S with type elt = Name.t

module Label : sig
  type t

  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val option_of_yaml : Yaml.value -> t option
  val yaml_of_t : t -> Yaml.value
end

module Label_set : Yaml_set.S with type elt = Label.t
module Label_map : Map.S with type key = Label.t

module Time : sig
  type t

  exception Invalid_month_name of string

  exception Malformed of string
  (** Raised by {!of_string_exn} for a string that is neither ISO 8601 nor [Month day, year],
      carrying the string. *)

  exception Out_of_range of string
  (** Raised for an instant more than [2^53 - 1] seconds from the epoch, or not a number at all,
      carrying the input as written (by {!t_of_yaml}, the number as YAML would write it). Also
      raised by {!of_string_exn} for a date with any field beyond [2^28], whatever instant it would
      produce. *)

  val of_string_exn : string -> t
  (** Raises {!Invalid_month_name} for an unknown month name, {!Malformed} for any other string that
      does not parse, and {!Out_of_range} for one that parses to an instant out of range, such as a
      year of [99999999999999], or that has any field - year, month, day, hour, minute or second -
      beyond [2^28], such as [1970-01-01T00:00:300000000Z]. The field bound keeps the arithmetic
      exact: fields of opposite sign could otherwise cancel into a wrong instant that is in range.
      Unlike {!Name.of_string} there is no option-returning counterpart: no caller wants to skip a
      bad date quietly, and an option would discard what was wrong with it. *)

  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Time_set : Yaml_set.S with type elt = Time.t
(** Update timestamps are a set so that an instant recorded by two entities with the same URI
    appears once however many times the input carried it, as with {!Extended_set}. A set is also
    sorted by construction, which is what the merge path used to maintain by hand; note that this
    normalizes an input whose [updatedAt] was written out of order, rather than round-tripping it as
    given. *)

module Extended : sig
  type t

  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val option_of_yaml : Yaml.value -> t option
  val yaml_of_t : t -> Yaml.value
end

module Extended_set : Yaml_set.S with type elt = Extended.t
(** Descriptions are a set so that merging entities unions them, as it does {!Name_set} and
    {!Label_set}: a description shared by two entities with the same URI appears once however many
    times the input carried it. *)

module Shared : Flag_intf.S
module To_read : Flag_intf.S
module Is_feed : Flag_intf.S

module Last_visited_at : sig
  type t

  val of_time : Time.t -> t
  val empty : t
  val get : t -> Time.t option
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concat : t -> t -> t
end

type t

val make :
  Uri.t ->
  Time.t ->
  ?updated_at:Time_set.t ->
  ?maybe_name:Name.t option ->
  ?labels:Label_set.t ->
  ?extended:Extended_set.t ->
  ?shared:Shared.t ->
  ?to_read:To_read.t ->
  ?last_visited_at:Last_visited_at.t ->
  ?is_feed:Is_feed.t ->
  unit ->
  t

val empty : t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val absorb : t -> t -> t
val uri : t -> Uri.t

val created_at : t -> Time.t option
(** [None] when the input gave no creation time; see henrytill/hbt-data#37. *)

val updated_at : t -> Time_set.t
val names : t -> Name_set.t
val labels : t -> Label_set.t
val extended : t -> Extended_set.t
val shared : t -> Shared.t
val to_read : t -> To_read.t
val last_visited_at : t -> Last_visited_at.t
val is_feed : t -> Is_feed.t
val map_labels : (Label_set.t -> Label_set.t) -> t -> t
val of_post : Pinboard.Post.t -> t

exception Missing_uri
(** Raised by {!t_of_yaml} for an entity with no [uri], or an empty or null one. *)

val t_of_yaml : Yaml.value -> t
val yaml_of_t : t -> Yaml.value

module Html : sig
  module Attrs = Prelude.Markup_ext.Attrs

  val entity_of_attrs : Attrs.t -> Name_set.t -> Label_set.t -> Extended_set.t -> t
end
