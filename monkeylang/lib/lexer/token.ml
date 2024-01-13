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
  | LeftBracket
  | RightBracket
  | LeftAngle
  | RightAngle
  | Comma
  | Semicolon
  (* Keywords *)
  | Let
  | Function

let string_of_token = function
  | Illegal -> "Illegal"
  | Eof -> "Eof"
  | Integer integer -> "Integer: " ^ integer
  | Identifier ident -> "Ident: " ^ ident
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | LeftBracket -> "LeftBracket"
  | RightBracket -> "RightBracket"
  | LeftAngle -> "LeftAngle"
  | RightAngle -> "RightAngle"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | Let -> "Let"
  | Function -> "Function"
;;
