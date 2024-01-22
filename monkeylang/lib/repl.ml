open Core

let print_prompt () = printf ">> %!"

let rec start () =
  Fmt.pr
    "Hello! This is the monkey programming language!@.Feel free to type in \
     commands@.";
  loop ()

and read_in () =
  print_prompt ();
  In_channel.input_line In_channel.stdin

and condition str =
  match str with
  | None -> false
  | Some str ->
    (match str with
     | "quit" -> false
     | "" | _ -> true)

and loop () =
  let input = ref (read_in ()) in
  let cond = ref (condition !input) in
  while !cond do
    let str = Option.value !input ~default:"" in
    let _ = Util.input_to_token_list str |> Util.print_listof_tokens in
    input := read_in ();
    cond := condition !input
  done
;;
