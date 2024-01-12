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
