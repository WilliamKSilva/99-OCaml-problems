(* Replicate the elements of a list a given number of times. *)

let replicate l n =
  let rec aux l replicated =
    match l with
    | [] -> replicated
    | hd :: tl ->
        let rec append_rec l a n =
          if n == 0 then l
          else
            let replicated = List.append l [ a ] in
            append_rec replicated a (n - 1)
        in

        let replicated = append_rec replicated hd n in
        aux tl replicated
  in

  aux l []

(* res = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
let res = replicate [ "a"; "b"; "c" ] 3
