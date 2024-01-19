open Lib

(* NOTE: We don't actually want to expose the lexer data structure
   implemenation in our `production` code. Will remove the `init-lexer` tests
   in the future. *)

module Lexer_test = struct
  let tuple_of_lexer (l : Lexer.t) = l.input, l.position, l.ch
  let create_lexer input = Lexer.init input |> tuple_of_lexer

  let next_token (l : Lexer.t) =
    let _, tok = Lexer.next_token l in
    match tok with
    | None -> Token.Eof
    | Some t -> t
  ;;
end

let test_init_lexer_aux s =
  let open Core in
  let expected =
    match String.is_empty s with
    | true -> s, 0, None
    | false -> s, 0, Some (String.get s 1)
  in
  Alcotest.(check (triple string int (option char)))
    "same"
    expected
    (Lexer_test.create_lexer s)
;;

let token_testable =
  let open Core in
  Alcotest.testable Token.pp (fun a b -> Poly.equal a b)
;;

let test_init_lexer_empty () = test_init_lexer_aux ""
let test_init_lexer_nonempty () = test_init_lexer_aux {|let five = 5;|}

let test_next_token_single () =
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
    Alcotest.(check token_testable)
      "same Token.t"
      ex
      (Lexer.init str |> Lexer_test.next_token))
;;

let test_next_token_all () =
  let input =
    {|  let five = 5;
 let ten = 10;

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
  let lex = Lexer.init input in
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
  let tokens = Lexer.list_of_tokens lex in
  Alcotest.(check (list token_testable)) "same lists" expected tokens
;;

let () =
  let open Alcotest in
  run
    "Utils"
    [ ( "init-lexer"
      , [ test_case "Empty string" `Quick test_init_lexer_empty
        ; test_case "Non-empty string" `Quick test_init_lexer_nonempty
        ] )
    ; ( "next-token"
      , [ test_case "Set of all possible tokens" `Quick test_next_token_single
        ; test_case
            "Monkey source code (w/all tokens)"
            `Quick
            test_next_token_all
        ] )
    ]
;;
