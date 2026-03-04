type t

type view =
  | Unknown
  | True
  | False
  | Both

val of_view : view -> t
val to_view : t -> view
val of_bool : bool -> t
val to_bool : t -> bool option
val of_bits : int -> t
val to_bits : t -> int
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val leq_truth : t -> t -> bool
val leq_knowledge : t -> t -> bool

val not : t -> t
(** Belnap logical NOT. Swaps [True] and [False]; leaves [Unknown] and [Both] fixed. *)

val ( && ) : t -> t -> t
(** Belnap logical AND.

    {math
      \begin{array}{ c|cccc }
      a \backslash b & U & T & F & B \\ \hline
      U & U & U & F & F \\
      T & U & T & F & B \\
      F & F & F & F & F \\
      B & F & B & F & B
      \end{array}
    } *)

val ( || ) : t -> t -> t
(** Belnap logical OR.

    {math
      \begin{array}{ c|cccc }
      a \backslash b & U & T & F & B \\ \hline
      U & U & T & U & T \\
      T & T & T & T & T \\
      F & U & T & F & B \\
      B & T & T & B & B
      \end{array}
    } *)

val implies : t -> t -> t
(** Belnap implication: {m \lnot a \lor b}. *)

val empty : t

val merge : t -> t -> t
(** Knowledge-ordering join: combine observations from independent sources. [Unknown] is the
    identity element; [Both] is absorbing.

    {math
      \begin{array}{ c|cccc }
      a \backslash b & U & T & F & B \\ \hline
      U & U & T & F & B \\
      T & T & T & B & B \\
      F & F & B & F & B \\
      B & B & B & B & B
      \end{array}
    } *)

val consensus : t -> t -> t
(** Knowledge-ordering meet: retain only information both sources agree on. [Both] is the identity
    element; [Unknown] is absorbing.

    {math
      \begin{array}{ c|cccc }
      a \backslash b & U & T & F & B \\ \hline
      U & U & U & U & U \\
      T & U & T & U & T \\
      F & U & U & F & F \\
      B & U & T & F & B
      \end{array}
    } *)

val is_known : t -> bool
val is_determined : t -> bool
val is_contradicted : t -> bool
