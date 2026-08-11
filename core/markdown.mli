exception Missing_date of string
(** Raised for a link that appears before any date heading, carrying its URI. Every bookmark needs a
    creation time, which comes from the enclosing level-1 heading. *)

val parse : string -> Collection.t
