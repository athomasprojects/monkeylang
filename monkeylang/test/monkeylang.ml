open Lexer
open Core

module Lexer_test = struct
  let create_lexer input =
    let l = Lexer.init input in
    l.input, l.position, l.ch
  ;;
end

let test_init_lexer_aux s =
  let expected =
    match String.is_empty s with
    | true -> s, 0, None
    | false -> s, 0, Some (String.get s 0)
  in
  Alcotest.(check (triple string int (option char)))
    "same"
    expected
    (Lexer_test.create_lexer s)
;;

let test_init_lexer_empty () = test_init_lexer_aux ""

let test_init_lexer_nonempty () =
  test_init_lexer_aux
    {|let five = 5;
 let ten = 10;

 let add = fn(x, y){
   x + y;
 };
 let result = add(five, ten);|}
;;

let () =
  let open Alcotest in
  run
    "Utils"
    [ ( "init-lexer"
      , [ test_case "Empty string" `Quick test_init_lexer_empty
        ; test_case "Non-empty string" `Quick test_init_lexer_nonempty
          (* ; test_case "non-empty string" string `Quick test_init_lexer *)
        ] )
    ]
;;
