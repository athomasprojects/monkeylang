open Monkey

let () = Fmt.pr "@.>> Welcome to Monkey!@."

let () =
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
 baz =!= 420
 "string"|}
  in
  let _ = Fmt.pr "%s@." input in
  let _ = Fmt.pr "==== Lexing ====@.@." in
  let tokens = Util.input_to_token_list input in
  let _ =
    List.iter (fun token -> Fmt.pr "Token => %a@." Token.pp token) tokens
  in
  Fmt.pr "@.==== END OF STREAM =====@."
;;
