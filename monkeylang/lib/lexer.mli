(** The Lexer generates tokens from an input stream. *)
type t

(** [init] creates a new lexer from the string [input]. *)
val init : string -> t

(** [next_token lexer] returns a new lexer and the next token in the stream if it exists. The lexer state is updated each time [next_token] is called. *)
val next_token : t -> t * Token.t option

(** [equal a b] checks whether two lexers have the same state. *)
val equal : t -> t -> bool

(** Pretty printing *)
val pp : Format.formatter -> t -> unit

val show : t -> string
