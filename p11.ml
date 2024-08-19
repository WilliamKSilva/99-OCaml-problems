(*
  Modify the result of the previous problem in such a way that if an element has
  no duplicates it is simply copied into the result list. Only elements with
  duplicates are transferred as (N E) lists.

  Since OCaml lists are homogeneous, one needs to define a type to hold both
  single elements and sub-lists.
*)

type 'a rle = One of 'a | Many of int * 'a

let encode l =
  let rec aux l (encoded : 'a rle list) =
    (* encoded is reversed *)
    match l with
    | [] -> encoded
    | hd :: tl -> (
        match encoded with
        | [] -> aux tl [ One hd ]
        | enc_hd :: enc_tl -> (
            match enc_hd with
            | One e ->
                if e = hd then
                  let updated = Many (2, e) in
                  aux tl (updated :: enc_tl)
                else aux tl (One hd :: encoded)
            | Many (e_count, e_value) ->
                if e_value = hd then
                  let updated = Many (e_count + 1, e_value) in
                  aux tl (updated :: enc_tl)
                else aux tl (One hd :: encoded)))
  in

  List.rev (aux l [])
