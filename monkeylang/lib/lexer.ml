open Core

type t =
  { input : string
  ; position : int
  ; ch : char option
  ; length : int
  }
[@@deriving show]

(* Initializes the lexer. *)
let init input =
  let length = String.length input - 1 in
  let lex =
    match length with
    | -1 | 0 -> { input; position = 0; ch = None; length }
    | x when x >= 1 ->
      { input; position = 0; ch = Some (String.get input 1); length }
    | _ -> assert false
  in
  let _ = Fmt.pr "@.Creating lexer => %a@." pp lex in
  lex
;;

let advance_lexer lexer =
  let { input; position; length; _ } = lexer in
  let position = succ position in
  (* let str_length = String.length input - 1 in *)
  let remaining_chars = length - position in
  if remaining_chars > 0 && remaining_chars < length
  then (
    let ch = Some (String.get input (succ position)) in
    { input; position; ch; length })
  else { input; position; ch = None; length }
;;

let lookup_ident ident =
  let open Token in
  match Map.find keywords ident with
  | Some keyword -> keyword
  | None -> Ident ident
;;

let read_ident { input; position; length; _ } =
  let ident =
    String.slice input position (length + 1)
    |> String.take_while ~f:(fun c -> Char.is_alpha c || Char.equal '_' c)
  in
  let tok = lookup_ident ident in
  let offset = String.length ident + (position - 1) in
  (* let _ = Fmt.pr "offset: %d@." offset in *)
  offset, tok
;;

let read_integer { input; position; length; _ } =
  let ident =
    String.slice input position (length + 1)
    |> String.take_while ~f:Char.is_digit
  in
  let tok = Token.Integer ident in
  let offset = String.length ident + (position - 1) in
  (* let _ = Fmt.pr "offset: %d@." offset in *)
  offset, tok
;;

let current_char lex =
  let remaining_chars = lex.length - lex.position in
  if remaining_chars >= 0 && remaining_chars <= lex.length
  then Some (String.get lex.input lex.position)
  else None
;;

let rec skip_whitespace lexer =
  match current_char lexer with
  | Some ch ->
    if Char.is_whitespace ch
    then skip_whitespace (advance_lexer lexer)
    else lexer
  | None -> lexer
;;

let update_lexer_pos lexer position = { lexer with position }

let two_char_token ch lexer =
  let next_ch =
    match lexer.ch with
    | Some x -> x
    | None -> Char.of_string ""
  in
  let tok, pos =
    let open Token in
    match ch with
    | '=' ->
      if Char.equal '=' next_ch
      then Equal, Some (succ lexer.position)
      else Assign, None
    | '!' ->
      if Char.equal '=' next_ch
      then NotEqual, Some (succ lexer.position)
      else Bang, None
    | _ -> assert false
  in
  tok, pos
;;

(* Keeps lexing until `next_token` returns `None`. This is when the entire stream of characters has been tokenized and have reached the end of the input stream. *)
let make_token lex ch =
  let pos = None in
  let tok, position =
    let open Token in
    match ch with
    | None -> Eof, pos
    | Some c ->
      (match c with
       | '=' -> two_char_token c lex
       | '!' -> two_char_token c lex
       | '-' -> Minus, pos
       | '+' -> Plus, pos
       | '*' -> Asterisk, pos
       | '/' -> Slash, pos
       | '(' -> LeftParen, pos
       | ')' -> RightParen, pos
       | '{' -> LeftBrace, pos
       | '}' -> RightBrace, pos
       | '[' -> LeftBracket, pos
       | '<' -> LessThan, pos
       | '>' -> GreaterThan, pos
       | ']' -> RightBracket, pos
       | ',' -> Comma, pos
       | ';' -> Semicolon, pos
       | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
         let pos, t = read_ident lex in
         t, Some pos
       | '0' .. '9' ->
         let pos, t = read_integer lex in
         t, Some pos
       | _ -> Illegal, pos)
  in
  tok, position
;;

let rec next_token lexer =
  let read_char lex =
    let open Token in
    let lex = skip_whitespace lex in
    let ch = current_char lex in
    (* let remaining_chars = lex.length - lex.position in *)
    (* let _ = Fmt.pr "Remaining characters: %d@." remaining_chars in *)
    (* let _ = Fmt.pr "current lexer: %a; current ch => " pp lex in *)
    (* let _ = *)
    (*   match ch with *)
    (*   | None -> Fmt.pr "NONE@." *)
    (*   | Some c -> Fmt.pr "'%c'@." c *)
    (* in *)
    let tok, pos = make_token lex ch in
    let _ = string_of_token tok |> Fmt.pr "TOKEN => %s@." in
    let lex =
      match pos with
      | Some position -> update_lexer_pos lex position
      | None -> lex
    in
    match tok with
    | Eof -> lex, None
    | _ ->
      let new_lex = advance_lexer lex in
      (* let _ = Fmt.pr "==== Advancing lexer@." in *)
      new_lex, Some tok
  in
  let lex, tok = read_char lexer in
  match tok with
  | None -> lex, tok
  | Some _ -> next_token lex
;;

let string_of_token = Token.string_of_token

module Tok = Token
