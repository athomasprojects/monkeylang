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
  | Asterisk
  | Slash
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
  | If
  | Else
  | True
  | False
  | Return

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
  | Asterisk -> "Asterisk"
  | Slash -> "Slash"
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
  | If -> "If"
  | Else -> "Else"
  | True -> "True"
  | False -> "False"
  | Return -> "Return"
;;

let keywords =
  SMap.of_alist_exn
    [ "let", Let
    ; "fn", Function
    ; "if", If
    ; "else", Else
    ; "true", True
    ; "false", False
    ; "return", Return
    ]
;;

let pp ppf t = Fmt.pf ppf "Token => %s@." (string_of_token t)
let show t = Fmt.str "Token => %s@." (string_of_token t)
