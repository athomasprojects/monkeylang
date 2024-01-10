module Token = Token

type t = unit

(* Initializes the lexer. *)
let init lexer : t =
  let _ = lexer in
  ()
;;

(* Takes in token and returns a token option. We keep lexing until `next_token` returns `None`.*)
let next_token (lexer : t) = Some lexer
