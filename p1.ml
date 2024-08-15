(*
   Write a function that returns the last element of a list
*)

let rec last l =
  match l with
  | [] -> None
  | hd :: tl -> if List.is_empty tl then Some hd else last tl

(* res = "d" *)
let res = last [ "a"; "b"; "c"; "d" ]
