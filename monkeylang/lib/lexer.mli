(** The Lexer generates tokens from an input stream.*)
type t

(** [init] creates a new lexer from the [input]. *)
val init : string -> t

(** [next_token lexer] returns a new lexer and the next token in the stream if it exists. The lexer state is updated each time a [next_token] is called.*)
val next_token : t -> t * Token.t option

val equal : t -> t -> bool

(* Pretty printing *)
val pp : Format.formatter -> t -> unit
val show : t -> string
