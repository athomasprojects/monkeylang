open Core
module SMap = Map.Make (String)

type t =
  | Illegal
  | Eof
  (* Integer *)
  | Integer of string
  (* Identifiers *)
  | Ident of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Equal
  | NotEqual
  | Bang
  (* Delimeters *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | LessThan
  | GreaterThan
  | Comma
  | Semicolon
  (* Keywords *)
  | Let
  | Function

let string_of_token = function
  | Illegal -> "Illegal"
  | Eof -> "Eof"
  | Integer integer -> "Integer: " ^ integer
  | Ident ident -> "Ident: " ^ ident
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | Bang -> "Bang"
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | LeftBracket -> "LeftBracket"
  | RightBracket -> "RightBracket"
  | LessThan -> "LessThan"
  | GreaterThan -> "GreaterThan"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | Let -> "Let"
  | Function -> "Function"
;;

let keywords = SMap.of_alist_exn [ "let", Let; "fn", Function ]
