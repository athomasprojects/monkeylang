let input_to_vec input =
  let rec aux acc lexer =
    match Lexer.next_token lexer with
    | _, None -> ()
    | lexer, Some t ->
      Vec.push acc t;
      aux acc lexer
  in
  let lexer = Lexer.init input in
  let vec = Vec.create 0 Token.Eof in
  aux vec lexer;
  vec
;;

let input_to_token_list input = input_to_vec input |> Vec.to_list

let print_listof_tokens tokens =
  List.iter (fun token -> Fmt.pr "%a@." Token.pp token) tokens
;;
