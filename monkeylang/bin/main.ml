open Core
open Lexer

let () =
  let test_input = "{};" in
  let lexer = Lexer.init test_input in
  Fmt.pr "@.Lexing...@.";
  let _ =
    let lex, tok = Lexer.next_token lexer in
    lex, tok
  in
  Fmt.pr "@.DONE!@."
;;
