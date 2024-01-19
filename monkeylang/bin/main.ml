open Lib

let () =
  let test_input =
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
  let lexer = Lexer.init test_input in
  Fmt.pr "@.Lexing...@.";
  (* let _ = Lexer.next_token lexer in *)
  let _ =
    let tokens = Lexer.list_of_tokens lexer in
    List.iter (fun t -> Token.pp Fmt.stdout t) tokens
  in
  Fmt.pr "@.DONE!@."
;;
