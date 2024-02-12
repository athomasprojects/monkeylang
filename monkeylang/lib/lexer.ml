open Core

type t =
  { input : string
  ; position : int
  ; ch : char option
  ; length : int
  }
[@@deriving show, eq]

let init input =
  let length = String.length input in
  if String.is_empty input
  then { input; position = 0; ch = None; length }
  else { input; position = 0; ch = Some (String.get input 0); length }
;;

let rec next_token lexer =
  let lexer = skip_whitespace lexer in
  let open Token in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, tok =
      match ch with
      | '=' -> two_char_token lexer '=' ~default:Assign ~matched:Equal
      | '!' -> two_char_token lexer '=' ~default:Bang ~matched:NotEqual
      (* | '|' -> two_char_token lexer '|' ~default:Illegal ~matched:Or *)
      (* | '&' -> two_char_token lexer '&' ~default:Illegal ~matched:And *)
      | '-' -> advance lexer, Minus
      | '+' -> advance lexer, Plus
      | '*' -> advance lexer, Asterisk
      | '/' -> advance lexer, Slash
      | '(' -> advance lexer, LeftParen
      | ')' -> advance lexer, RightParen
      | '{' -> advance lexer, LeftBrace
      | '}' -> advance lexer, RightBrace
      | '[' -> advance lexer, LeftBracket
      | '<' -> advance lexer, LessThan
      | '>' -> advance lexer, GreaterThan
      | ']' -> advance lexer, RightBracket
      | ',' -> advance lexer, Comma
      | ';' -> advance lexer, Semicolon
      | ch when is_letter ch -> read_identifier lexer
      | ch when Char.is_digit ch -> read_number lexer
      | ch when is_string ch -> read_string lexer
      | _ -> advance lexer, Illegal
    in
    (* let _ = Fmt.pr "Token => %a@." Token.pp tok in *)
    lexer, Some tok

and skip_whitespace lexer =
  let lexer, _ =
    scan_until lexer (fun ch ->
      match ch with
      | Some ch -> Char.is_whitespace ch
      | None -> false)
  in
  lexer

and scan_until lexer condition =
  let rec loop lexer =
    if condition lexer.ch then loop @@ advance lexer else lexer
  in
  let lexer = loop lexer in
  lexer, lexer.position

and advance lexer =
  let lexer =
    if lexer.position >= lexer.length - 1
    then { lexer with ch = None }
    else (
      let position = succ lexer.position in
      { lexer with position; ch = Some (String.get lexer.input position) })
  in
  lexer

and is_string ch = Char.('"' = ch)
and is_letter ch = Char.is_alpha ch || Char.('_' = ch)

and take_while lexer condition =
  String.slice lexer.input lexer.position lexer.length
  |> String.take_while ~f:condition

and read_identifier lexer =
  let ident = take_while lexer is_letter in
  let tok = Token.lookup_ident ident in
  let position = lexer.position + String.length ident - 1 in
  advance { lexer with position }, tok

and read_number lexer =
  let ident = take_while lexer Char.is_digit in
  let tok = Token.Integer ident in
  let position = lexer.position + String.length ident - 1 in
  advance { lexer with position }, tok

and read_string lexer =
  let lexer = advance lexer in
  let ident = take_while lexer (fun ch -> not (is_string ch)) in
  let position = lexer.position + String.length ident in
  let tok =
    if position >= lexer.length
       (* then Fmt.failwith "Missing matching double qoute@." *)
       (* NOTE: an illegal token should probably be handled in the parser stage. For now we can just mark the token as invalid and continue parsing the rest of the input. *)
    then Token.Illegal
    else Token.Str ident
  in
  (* let _ = Fmt.pr "@.Position: %d, lexer length: %d@." position lexer.length in *)
  advance { lexer with position }, tok

and peek_char lexer =
  let remaining_chars = lexer.length - lexer.position - 1 in
  if remaining_chars >= 1
  then Some (String.get lexer.input (succ lexer.position))
  else None

and two_char_token lexer match_char ~default ~matched =
  let lexer, tok =
    match peek_char lexer with
    | Some peeked ->
      if Char.(match_char = peeked)
      then advance @@ advance lexer, matched
      else advance lexer, default
    | None -> advance lexer, default
  in
  lexer, tok
;;
