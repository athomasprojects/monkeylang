let () =
  let open Lib in
  let test_input = "[{};<>!?" in
  let lexer = Lexer.init test_input in
  Fmt.pr "@.Lexing...@.";
  let _ = Lexer.next_token lexer in
  Fmt.pr "@.DONE!@."
;;
