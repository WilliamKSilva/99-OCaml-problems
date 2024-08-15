(* Find the number of elements of a list. *)

let length l =
  let rec loop l len =
    match l with [] -> len | _ :: tl -> loop tl (len + 1)
  in

  loop l 0

(* res = 3 *)
let res = length [ "a"; "b"; "c" ]
