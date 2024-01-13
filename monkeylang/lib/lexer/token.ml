type t =
  | Illegal
  | Eof
  (* Integer *)
  | Integer of string
  (* Identifiers *)
  | Identifier of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  (* Delimeters *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Semicolon
  (* Keywords *)
  | Let
  | Function

let string_of_token = function
  | Illegal -> "Illegal"
  | Eof -> "Eof"
  | Integer integer -> integer
  | Identifier ident -> ident
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | Let -> "Let"
  | Function -> "Function"
;;
