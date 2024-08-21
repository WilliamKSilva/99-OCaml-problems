(*
    Given a run-length code list generated as specified in the previous problem,
    construct its uncompressed version.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode l =
  let rec aux l decoded =
    match l with
    | [] -> decoded
    | hd :: tl ->
      match hd with
      | One a ->
        let decoded = List.append decoded [a] in
        aux tl decoded
      | Many (fst, sec) ->
        let rec multiple a decoded count =
          if count == 0 then
            decoded
          else
            let decoded = List.append decoded [a] in
            multiple a decoded (count - 1)
        in

        let decoded = multiple sec decoded fst in
        aux tl decoded
  in

  aux l []
