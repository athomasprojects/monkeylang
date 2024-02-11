open Core

let print_prompt () = printf ">> %!"

let read_in () =
  print_prompt ();
  In_channel.input_line In_channel.stdin
;;

let condition str =
  match str with
  | None -> false
  | Some str ->
    (match str with
     | "quit" -> false
     | "" | _ -> true)
;;

let rec loop input =
  if condition input
  then (
    let str = Option.value input ~default:"" in
    let _ = Util.input_to_token_list str |> Util.print_listof_tokens in
    let input = read_in () in
    loop input)
  else ()
;;

let start () =
  Fmt.pr
    "Hello! This is the monkey programming language!@.Feel free to type in \
     commands@.";
  loop (read_in ())
;;
