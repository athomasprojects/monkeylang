type t =
  | Illegal
  | Eof
  (* Integer *)
  | Integer of string
  (* Identifiers *)
  | Ident of string
  | String_ of string
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
[@@deriving show, eq, sexp]

let lookup_ident = function
  | "let" -> Let
  | "fn" -> Function
  | "if" -> If
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | "return" -> Return
  | ident -> Ident ident
;;
