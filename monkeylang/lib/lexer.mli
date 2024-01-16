type t =
  { input : string
  ; position : int
  ; ch : char option
  ; length : int
  }

(* Initializes the lexer. *)
val init : string -> t

(* Takes in token and returns a token option. We keep lexing until `next_token` returns `None`.*)
val next_token : t -> t * Token.t option

(* Pretty printing *)
(* val pp : Format.formatter -> t -> unit *)
(* val show : t -> string *)
