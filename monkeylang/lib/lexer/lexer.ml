open Core

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

(* Initializes the lexer. *)
let init input =
  let lex =
    if String.is_empty input
    then { input; position = 0; ch = None }
    else { input; position = 0; ch = Some (String.get input 0) }
  in
  let _ = Fmt.pr "@.Creating lexer => %a@." pp lex in
  lex
;;

let read_char lexer =
  if String.is_empty lexer.input
  then None
  else Some (String.get lexer.input lexer.position)
;;

let peek_char lexer =
  match String.is_empty lexer.input with
  | true -> None
  | false ->
    let position = succ lexer.position in
    if position >= 0 && position <= String.length lexer.input
    then Some (String.get lexer.input lexer.position)
    else None
;;

let advance_lexer lexer =
  let { input; position; _ } = lexer in
  if position >= String.length input
  then { input; position = 0; ch = None }
  else (
    let position = succ position in
    let ch = Some (String.get input position) in
    { input; position; ch })
;;

(* Takes in token and returns a token option. We keep lexing until `next_token` returns `None`.*)
let rec next_token lexer =
  let aux lex =
    let open Token in
    let tok =
      match read_char lex with
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
    | Eof -> lex, None
    | _ ->
      let lex = advance_lexer lex in
      let _ = Fmt.pr "advancing lexer => %a@." pp lex in
      lex, Some tok
  in
  let lexer, tok = aux lexer in
  match tok with
  | None -> lexer, tok
  | Some _ -> next_token lexer
;;
