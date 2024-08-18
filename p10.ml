(*
  Run-Length Encoding

  Run-length encoding (RLE) is a form of lossless data compression in which runs
  of data (consecutive occurrences of the same data value) are stored as a single
  occurrence of that data value and a count of its consecutive occurrences,
  rather than as the original run.
*)

let encode l =
  let rec last l =
    match l with [] -> None | [ hd ] -> Some hd | _ :: tl -> last tl
  in

  let rec update_last l n v =
    match l with
    | [] -> n
    | [ _ ] ->
        let n = List.append n [ v ] in
        update_last [] n v
    | hd :: tl ->
        let n = List.append n [ hd ] in
        update_last tl n v
  in

  let rec loop l mem encoded =
    match l with
    | [] -> encoded
    | hd :: tl -> (
        match mem with
        | None -> loop tl (Some hd) [ (1, hd) ]
        | Some m -> (
            let last_encoded = last encoded in
            match last_encoded with
            | None -> loop tl (Some hd) [ (1, hd) ]
            | Some l ->
                if m = hd then
                  let encoded = update_last encoded [] (fst l + 1, hd) in
                  loop tl (Some hd) encoded
                else
                  let encoded = List.append encoded [ (1, hd) ] in
                  loop tl (Some hd) encoded))
  in

  loop l None []
