(* Reverse a list. *)

let rec rev l =
  match l with
  | [] -> []
  | hd :: tl ->
      let l = rev tl in
      l @ [ hd ]

(* res = ["c", "b", "a"] *)
let res = rev [ "a"; "b"; "c" ]
