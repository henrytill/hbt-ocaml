module type SIZE = sig
  val n : int
end

module type S = sig
  type t
  (** A Belnap vector of exactly [n] elements. *)

  type index
  (** A bounds-checked index into [t]. Values are only constructable via [index] or [index_exn],
      guaranteeing validity for any [t] without a runtime check. *)

  (** {2 Construction} *)

  val make : unit -> t
  val all_true : unit -> t
  val all_false : unit -> t

  val of_array : Belnap.t array -> t
  (** [of_array a] raises [Invalid_argument] unless [Array.length a = n]. *)

  val of_list : Belnap.t list -> t
  (** [of_list l] raises [Invalid_argument] unless [List.length l = n]. *)

  val to_array : t -> Belnap.t array
  (** Returns an array of exactly [n] elements. *)

  val to_list : t -> Belnap.t list
  (** Returns a list of exactly [n] elements. *)

  (** {2 Indexing} *)

  val index : int -> index option
  (** [index i] returns [Some i] if [0 <= i < n], [None] otherwise. *)

  val index_exn : int -> index
  (** [index_exn i] returns [i] if [0 <= i < n], raises [Invalid_argument] otherwise. *)

  val get : t -> index -> Belnap.t
  val set : t -> index -> Belnap.t -> unit

  (** {2 Bulk operations} *)

  val not : t -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val merge : t -> t -> t
  val implies : t -> t -> t

  (** {2 Predicates} *)

  val is_consistent : t -> bool
  val is_all_determined : t -> bool
  val is_all_true : t -> bool
  val is_all_false : t -> bool

  (** {2 Counts} *)

  val count_true : t -> int
  val count_false : t -> int
  val count_both : t -> int
  val count_unknown : t -> int

  (** {2 Search} *)

  val find_first : Belnap.t -> t -> int option

  (** {2 Utilities} *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end
