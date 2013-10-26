type t = { hi : float; lo : float; }
val t : float -> float -> t

(* Simple conversions *)
val of_float : float -> t
val to_float : t -> float
val of_int : int -> t
val to_int : t -> int

(* String I/O *)
val dump : t -> string
val to_string_std : t -> string
val to_string_sci : t -> string
val to_string : t -> string
val of_string : string -> t

(* Some predicates *)
val is_nan : t -> bool
val is_zero : t -> bool
val is_negative : t -> bool
val is_positive : t -> bool

(* Comparison *)
val eq : t -> t -> bool
val ne : t -> t -> bool
val gt : t -> t -> bool
val ge : t -> t -> bool
val lt : t -> t -> bool
val le : t -> t -> bool
val compare : t -> t -> int
val signum : t -> int

(* Computations *)
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val neg : t -> t
val inv : t -> t
val floor : t -> t
val ceil : t -> t
val abs : t -> t
val trunc : t -> t
val sqr : t -> t
val sqrt : t -> t
val pow : t -> int -> t

(* Constants *)
val pi : t
val two_pi : t
val pi_2 : t
val e : t
val nan : t
val zero : t
val one : t
val ten : t

val k_eps : float

(* Syntax sugar *)
module Infix : sig
  val ( +..  ) : t -> t -> t
  val ( -..  ) : t -> t -> t
  val ( ~-.. ) : t -> t
  val ( *..  ) : t -> t -> t
  val ( /..  ) : t -> t -> t
  val ( **.. ) : t -> int -> t
  val ( <..  ) : t -> t -> bool
  val ( >..  ) : t -> t -> bool
  val ( =..  ) : t -> t -> bool
  val ( <>.. ) : t -> t -> bool
  val ( <=.. ) : t -> t -> bool
  val ( >=.. ) : t -> t -> bool
end
