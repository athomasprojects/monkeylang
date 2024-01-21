open Monkey
open Core

let () = Fmt.pr "@.>> Welcome to Monkey!@."

let input_to_token_list input =
  let rec aux acc l =
    let l, token = Lexer.next_token l in
    match token with
    | None -> acc
    | Some t -> aux (t :: acc) l
  in
  List.rev @@ aux [] (Lexer.init input)
;;

let _print_token token =
  match token with
  | Some t -> Fmt.pr "Token => %a@." Token.pp t
  | None -> Fmt.pr "==== END OF STREAM =====@."
;;

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
 baz =!= 420|}
  in
  let _ = Fmt.pr "==== Lexing ====@.@." in
  let tokens = input_to_token_list input in
  let _ =
    List.iter tokens ~f:(fun token -> Fmt.pr "Token => %a@." Token.pp token)
  in
  Fmt.pr "@.==== END OF STREAM =====@."
;;
