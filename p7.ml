(* Flatten a nested list structure. *)

type 'a node = One of 'a | Many of 'a node list

let flatten (l : 'a node list) =
  let rec loop l flatten =
    match l with
    | [] -> flatten
    | hd :: tl -> (
        match hd with
        | One one ->
            let f = List.append flatten [ one ] in
            loop tl f
        | Many many ->
            let f = loop many flatten in
            if List.length tl >= 1 then loop tl f else f)
  in

  loop l []

(* res = ["a"; "b"; "c"; "d"; "e"] *)
let res =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
