type t =
  | Illegal
  | Eof
  (* Integer *)
  | Integer of int
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
