let rec input_to_vec input =
  let open Lexer in
  let lexer = init input in
  let vec = Vec.create 0 Token.Eof in
  let rec aux acc lexer =
    match next_token lexer with
    | _, None -> ()
    | lexer, Some t ->
      Vec.push acc t;
      aux acc lexer
  in
  aux vec lexer;
  vec

and input_to_token_list input = Vec.to_list (input_to_vec input)

and print_listof_tokens tokens =
  List.iter (fun token -> Fmt.pr "%a@." Token.pp token) tokens
;;
