let input_to_token_list input =
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
  Vec.to_list vec
;;
