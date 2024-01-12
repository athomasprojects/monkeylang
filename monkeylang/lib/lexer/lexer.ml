open Core

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

(* Initializes the lexer. *)
let init input =
  if String.is_empty input
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let advance_lexer lexer =
  let _ = Fmt.pr "%a" pp lexer in
  let { input; position; _ } = lexer in
  if position >= String.length input
  then { input; position = 0; ch = None }
  else (
    let position = succ position in
    let ch = Some (String.get input position) in
    { input; position; ch })
;;

(* Takes in token and returns a token option. We keep lexing until `next_token` returns `None`.*)
let next_token lexer =
  let open Token in
  let tok =
    match lexer.ch with
    | None -> Eof
    | Some ch ->
      (match ch with
       | '=' -> Assign
       | '-' -> Minus
       | '+' -> Plus
       | '(' -> LeftParen
       | ')' -> RightParen
       | '{' -> LeftBrace
       | '}' -> RightBrace
       | ',' -> Comma
       | ';' -> Semicolon
       | _ -> Illegal)
  in
  match tok with
  | Eof -> lexer, None
  | _ -> advance_lexer lexer, Some tok
;;
