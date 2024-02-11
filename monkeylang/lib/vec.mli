(** The vector is 1D array-like data-structure. *)
type 'a t

(** [create len dummy_elt] creates a vector of length [len], whose elements are initialized to [dummy_elt]. *)
val create : int -> 'a -> 'a t

(** [length v] returns the number of elements in [v]. *)
val length : 'a t -> int

(** [push v elt] appends [elt] to [v]. *)
val push : 'a t -> 'a -> unit

(** [pop v i] removes the [i]th element. If [v] contains elements after the index [i] then the index of each remaining elements is shifted down by one. *)
val pop : 'a t -> int -> unit

(** [get v i] returns [i]th element of [v] if it exists, otherwise nothing is returned. *)
val get : 'a t -> int -> 'a option

(** [to_list v] converts [v] to a list. *)
val to_list : 'a t -> 'a list

(** [of_list x] converts [x] to a vector. *)
val of_list : 'a list -> 'a t

(** Pretty printing *)
val pp : (Format.formatter -> 'a -> unit) -> 'a t -> unit

(* val show : 'a t -> unit *)
