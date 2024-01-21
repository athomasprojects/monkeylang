open Monkey
open Core

module Test = struct
  let input_to_token_list input =
    let rec aux acc l =
      let l, token = Lexer.next_token l in
      match token with
      | None -> acc
      | Some t -> aux (t :: acc) l
    in
    List.rev @@ aux [] (Lexer.init input)
  ;;

  let lexer_testable = Alcotest.testable Lexer.pp Lexer.equal
  let token_testable = Alcotest.testable Token.pp Token.equal

  let token_opt_value lexer =
    let _, token = Lexer.next_token lexer in
    Option.value token ~default:Token.Eof
  ;;

  let test_init_lexer_empty () =
    let lexer = Lexer.init "" in
    let expected = Lexer.init {||} in
    Alcotest.(check lexer_testable) "same Lexer.t" expected lexer
  ;;

  let test_next_token_one () =
    let open Core in
    let input =
      [ "let"
      ; "fn"
      ; "if"
      ; "else"
      ; "true"
      ; "false"
      ; "return"
      ; "="
      ; "!"
      ; "=="
      ; "!="
      ; "-"
      ; "+"
      ; "*"
      ; "/"
      ; "("
      ; ")"
      ; "{"
      ; "}"
      ; "["
      ; "<"
      ; ">"
      ; "]"
      ; ","
      ; ";"
      ; "foo"
      ; "69"
      ]
    in
    let expected =
      let open Token in
      [ Let
      ; Function
      ; If
      ; Else
      ; True
      ; False
      ; Return
      ; Assign
      ; Bang
      ; Equal
      ; NotEqual
      ; Minus
      ; Plus
      ; Asterisk
      ; Slash
      ; LeftParen
      ; RightParen
      ; LeftBrace
      ; RightBrace
      ; LeftBracket
      ; LessThan
      ; GreaterThan
      ; RightBracket
      ; Comma
      ; Semicolon
      ; Ident "foo"
      ; Integer "69"
      ]
    in
    List.iter2_exn expected input ~f:(fun ex str ->
      let lexer = Lexer.init str in
      let token = token_opt_value lexer in
      Alcotest.(check token_testable) "same Token.t" ex token)
  ;;

  let test_next_token_full () =
    let input =
      {|  let five = 5;
     let ten = 10;

     !*-/?
     let add = fn(x, y){
       x + y;
     };
     let result = add(five, ten);
     if x != y {
       let foo_bar = y / x * 69;
       return true;
     } else {
       return false;
     }
     10 == 10;
     10 != 9;
     baz =!= 420|}
    in
    let expected =
      let open Token in
      [ Let
      ; Ident "five"
      ; Assign
      ; Integer "5"
      ; Semicolon
      ; Let
      ; Ident "ten"
      ; Assign
      ; Integer "10"
      ; Semicolon
      ; Bang
      ; Asterisk
      ; Minus
      ; Slash
      ; Illegal
      ; Minus
      ; Slash
      ; Illegal
      ; Let
      ; Ident "add"
      ; Assign
      ; Function
      ; LeftParen
      ; Ident "x"
      ; Comma
      ; Ident "y"
      ; RightParen
      ; LeftBrace
      ; Ident "x"
      ; Plus
      ; Ident "y"
      ; Semicolon
      ; RightBrace
      ; Semicolon
      ; Let
      ; Ident "result"
      ; Assign
      ; Ident "add"
      ; LeftParen
      ; Ident "five"
      ; Comma
      ; Ident "ten"
      ; RightParen
      ; Semicolon
      ; If
      ; Ident "x"
      ; NotEqual
      ; Ident "y"
      ; LeftBrace
      ; Let
      ; Ident "foo_bar"
      ; Assign
      ; Ident "y"
      ; Slash
      ; Ident "x"
      ; Asterisk
      ; Integer "69"
      ; Semicolon
      ; Return
      ; True
      ; Semicolon
      ; RightBrace
      ; Else
      ; LeftBrace
      ; Return
      ; False
      ; Semicolon
      ; RightBrace
      ; Integer "10"
      ; Equal
      ; Integer "10"
      ; Semicolon
      ; Integer "10"
      ; NotEqual
      ; Integer "9"
      ; Semicolon
      ; Ident "baz"
      ; Assign
      ; NotEqual
      ; Integer "420"
      ]
    in
    let tokens = input_to_token_list input in
    List.iter2_exn expected tokens ~f:(fun ex tok ->
      Alcotest.(check token_testable) "same Token.t" ex tok)
  ;;
end

let () =
  Alcotest.run
    "Monkeylang Lexer Test Suite"
    [ ( "init-lexer"
      , [ Alcotest.test_case "Empty string" `Quick Test.test_init_lexer_empty ]
      )
    ; ( "next-token"
      , [ Alcotest.test_case "Individual tokens" `Quick Test.test_next_token_one
        ; Alcotest.test_case
            "Monkey source code"
            `Quick
            Test.test_next_token_full
        ] )
    ]
;;
