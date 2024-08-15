(* Find the N'th element of a list. *)

let nth l n =
  let rec loop l n len =
    let get_index len len_tl = len - len_tl - 1 in
    match l with
    | [] -> None
    | hd :: tl ->
        let idx = get_index len (List.length tl) in
        if idx == n then Some hd else loop tl n len
  in

  loop l n (List.length l)

(* res = "c" *)
let res = nth [ "a"; "b"; "c"; "d"; "e" ] 2
