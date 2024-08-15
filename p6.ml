(*
  Find out whether a list is a palindrome.
  Hint: A palindrome is its own reverse.
*)

let is_palindrome l =
  let rec rev l =
    match l with
    | [] -> []
    | hd :: tl ->
        let l = rev tl in
        l @ [ hd ]
  in

  let rec compare l rev =
    match l with
    | [] -> if List.is_empty rev then true else false
    | hd :: tl -> (
        match rev with
        | [] -> false
        | hd_rev :: tl_rev -> if hd != hd_rev then false else compare tl tl_rev)
  in

  let reversed = rev l in
  compare l reversed

(* res = true *)
let res = is_palindrome [ "x"; "a"; "m"; "a"; "x" ]
