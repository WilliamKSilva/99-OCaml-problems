(* Duplicate the elements of a list. *)

let duplicate l =
  let rec aux l duplicated =
    match l with
    | [] -> duplicated
    | hd :: tl ->
      let duplicated = List.append duplicated [hd] in
      let duplicated = List.append duplicated [hd] in
      aux tl duplicated

  in

  aux l []

(* res = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
let res = duplicate ["a"; "b"; "c"; "c"; "d"]
