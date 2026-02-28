type t

val create : unit -> t
val make : int -> t
val all_true : int -> t
val all_false : int -> t
val of_list : Belnap.t list -> t
val of_array : Belnap.t array -> t
val to_list : t -> Belnap.t list
val to_array : t -> Belnap.t array
val find_first : Belnap.t -> t -> int option
val width : t -> int
val get : t -> int -> Belnap.t
val set : t -> int -> Belnap.t -> unit
val truncate : t -> int -> unit
val resize : t -> int -> unit
val not : t -> t
val ( && ) : t -> t -> t
val ( || ) : t -> t -> t
val implies : t -> t -> t
val merge : t -> t -> t
val is_consistent : t -> bool
val is_all_determined : t -> bool
val is_all_true : t -> bool
val is_all_false : t -> bool
val count_true : t -> int
val count_false : t -> int
val count_both : t -> int
val count_unknown : t -> int
val equal : t -> t -> bool
