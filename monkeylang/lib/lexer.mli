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
val string_of_token : Token.t -> string
(* Pretty printing *)
(* val pp : t -> Format.formatter *)
(* val show : Format.formatter -> unit *)
