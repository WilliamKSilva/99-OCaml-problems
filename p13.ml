(*
  Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly
  create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list
  elements into sublists", but only count them. As in problem "Modified run-length encoding",
  simplify the result list by replacing the singleton lists (1 X) by X.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  let rec aux l encoded =
    match l with
    | [] -> encoded
    | hd :: tl ->
      match encoded with
      | [] ->
        let encoded = List.append encoded [One hd] in
        aux tl encoded
      | last :: encoded_tl ->
        match last with
        | One a ->
          if a = hd then
            let many = Many (2, a) in
            let encoded = many :: encoded_tl in
            aux tl encoded
          else
            let one = One hd in
            (* We add to the unmodified encoded List *)
            let encoded = one :: encoded in
            aux tl encoded
        | Many (fst, sec) ->
          if sec = hd then
            let many = Many (fst + 1, sec) in
            let encoded = many :: encoded_tl in
            aux tl encoded
          else
            let one = One hd in
            (* We add to the unmodified encoded List *)
            let encoded = one :: encoded in
            aux tl encoded
  in

  let encoded = aux l [] in
  List.rev encoded

(* res = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]  *)
let res = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
