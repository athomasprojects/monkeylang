open Core

(* [array] is the underlying array used to store the vector elements. [len] is the number of elements in the vector. [len] is not necessarily equal to the the length of [array]. *)
type 'a t =
  { mutable array : 'a array
  ; mutable len : int
  }

let length v = v.len
let create len dummy_elt = { array = Array.create ~len dummy_elt; len = 0 }

let grow v dummy_elt =
  let len = (2 * v.len) + 1 in
  v.array
  <- Array.init len ~f:(fun i -> if i < v.len then v.array.(i) else dummy_elt)
;;

let push v elt =
  if Array.length v.array = v.len then grow v elt;
  v.array.(v.len) <- elt;
  v.len <- succ v.len
;;

let pop v i =
  if i >= 0 && i < Array.length v.array
  then
    Array.iteri v.array ~f:(fun n elt -> if n > i then v.array.(n - 1) <- elt);
  v.len <- pred v.len
;;

let get v i = if i >= v.len || i < 0 then None else Some v.array.(i)
let to_list v = Array.init v.len ~f:(fun i -> v.array.(i)) |> Array.to_list
let of_list x = { array = Array.of_list x; len = List.length x }

let pp ppf v =
  Format.printf
    "%a"
    (Format.pp_print_array ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.") ppf)
    (Array.init v.len ~f:(fun i -> v.array.(i)))
;;
