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
    else { input; position = 0; ch = Some (String.get input 1) }
  in
  let _ = Fmt.pr "@.Creating lexer => %a@." pp lex in
  lex
;;

(* TODO: implement `peek` function to handle checking 'next' character for tokenizing idents, integers, and keywords *)

let advance_lexer lexer =
  let { input; position; _ } = lexer in
  let position = succ position in
  let str_length = String.length input - 1 in
  let remaining_chars = str_length - position in
  if remaining_chars > 0 && remaining_chars < str_length
  then (
    let ch = Some (String.get input (succ position)) in
    { input; position; ch })
  else { input; position; ch = None }
;;

(* Keeps lexing until `next_token` returns `None`. This is when the entire stream of characters has been tokenized and have reached the end of the input stream. *)
let rec next_token lexer =
  let str_length = String.length lexer.input - 1 in
  let aux lex =
    let open Token in
    let remaining_chars = str_length - lex.position in
    let _ = Fmt.pr "Remaining characters: %d@." remaining_chars in
    let ch =
      if remaining_chars >= 0 && remaining_chars <= str_length
      then Some (String.get lex.input lex.position)
      else None
    in
    let _ = Fmt.pr "current lexer: %a; ch => " pp lex in
    let _ =
      match ch with
      | None -> Fmt.pr "NONE@."
      | Some c -> Fmt.pr "'%c'@." c
    in
    let tok =
      match ch with
      | None -> Eof
      | Some c ->
        (match c with
         | '=' -> Assign
         | '-' -> Minus
         | '+' -> Plus
         | '(' -> LeftParen
         | ')' -> RightParen
         | '{' -> LeftBrace
         | '}' -> RightBrace
         | '[' -> LeftBracket
         | '<' -> LeftAngle
         | '>' -> RightAngle
         | ']' -> RightBracket
         | ',' -> Comma
         | ';' -> Semicolon
         | _ -> Illegal)
    in
    let _ = Token.string_of_token tok |> Fmt.pr "TOKEN => %s@.@." in
    match tok with
    | Eof -> lex, None
    | _ ->
      let new_lex = advance_lexer lex in
      let _ = Fmt.pr "==== Advancing lexer@." in
      new_lex, Some tok
  in
  let lex, tok = aux lexer in
  match tok with
  | None -> lex, tok
  | Some _ -> next_token lex
;;
