open Lib

(* NOTE: We don't actually want to expose the lexer data structure
   implemenation in our `production` code. Will remove the `init-lexer` tests
   in the future. *)

module Lexer_test = struct
  let tuple_of_lexer (l : Lexer.t) = l.input, l.position, l.ch
  let create_lexer input = Lexer.init input |> tuple_of_lexer
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

let test_init_lexer_empty () = test_init_lexer_aux ""
let test_init_lexer_nonempty () = test_init_lexer_aux {|let five = 5;|}

let () =
  let open Alcotest in
  run
    "Utils"
    [ ( "init-lexer"
      , [ test_case "Empty string" `Quick test_init_lexer_empty
        ; test_case "Non-empty string" `Quick test_init_lexer_nonempty
        ] )
    ]
;;
