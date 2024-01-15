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

(* let skip_whitespace lexer = *)
(*   if Char.is_whitespace (String.get lexer.input lexer.position) *)
(*   then advance_lexer lexer *)
(*   else lexer *)
(* ;; *)
(**)
(* let lookup_ident ident = *)
(*   match Map.find Token.keywords ident with *)
(*   | Some keyword -> keyword *)
(*   | None -> Ident ident *)
(* ;; *)
(**)
(* let read_ident lexer = *)
(*   let open Token in *)
(*   let input_length = String.length lexer.input - 1 in *)
(*   let ident = *)
(*     String.slice lexer.input lexer.position input_length *)
(*     |> String.take_while ~f:Char.is_alpha *)
(*   in *)
(*   let _ = String.capitalize ident |> Map.find keywords in *)
(*   match ident with *)
(*   | "let" -> Let *)
(*   | "fn" -> Function *)
(*   | _ -> *)
(*     let value = *)
(*       String.slice *)
(*         lexer.input *)
(*         (lexer.position + (String.length ident - 1)) *)
(*         input_length *)
(*       |> String.take_while ~f:(fun ch -> Char.is_alpha ch || Char.equal '_' ch) *)
(*     in *)
(*     Ident value *)
(* ;; *)

(* let read_integer lexer = *)
(*   let open Token in *)
(*   let ident = *)
(*     String.slice lexer.input lexer.position (String.length lexer.input) *)
(*     |> String.take_while ~f:(fun ch -> Char.is_alpha ch || Char.equal '_' ch) *)
(*   in *)
(*   match String.capitalize ident with *)
(*   | "let" -> Let *)
(*   | "fn" -> Function *)
(*   | _ -> Ident ident *)
(* ;; *)

(* Keeps lexing until `next_token` returns `None`. This is when the entire stream of characters has been tokenized and have reached the end of the input stream. *)
let read_char lex =
  let open Token in
  let str_length = String.length lex.input - 1 in
  let remaining_chars = str_length - lex.position in
  let _ = Fmt.pr "Remaining characters: %d@." remaining_chars in
  let ch =
    if remaining_chars >= 0 && remaining_chars <= str_length
    then Some (String.get lex.input lex.position)
    else None
  in
  let _ = Fmt.pr "current lexer: %a; current ch => " pp lex in
  let _ =
    match ch with
    | None -> Fmt.pr "NONE@."
    | Some c -> Fmt.pr "'%c'@." c
  in
  let tok =
    match ch with
    | None -> Eof
    | Some c ->
      (* if Char.is_whitespace c *)
      (* then skip_whitespace lex *)
      (* else ( *)
      (match c with
       (* | 'a' .. 'z' | 'A' .. 'Z' | '_' -> read_ident lex *)
       (* | 'a' .. 'z' | 'A' .. 'Z' -> read_ident lex *)
       | '=' -> Assign
       | '-' -> Minus
       | '+' -> Plus
       | '!' -> Bang
       | '(' -> LeftParen
       | ')' -> RightParen
       | '{' -> LeftBrace
       | '}' -> RightBrace
       | '[' -> LeftBracket
       | '<' -> LessThan
       | '>' -> GreaterThan
       | ']' -> RightBracket
       | ',' -> Comma
       | ';' -> Semicolon
       | _ -> Illegal)
  in
  let _ = string_of_token tok |> Fmt.pr "TOKEN => %s@.@." in
  match tok with
  | Eof -> lex, None
  | _ ->
    let new_lex = advance_lexer lex in
    let _ = Fmt.pr "==== Advancing lexer@." in
    new_lex, Some tok
;;

let rec next_token lexer =
  let lex, tok = read_char lexer in
  match tok with
  | None -> lex, tok
  | Some _ -> next_token lex
;;
