(* Eliminate consecutive duplicates of list elements. *)

let compress l =
  let rec loop l mem compressed =
    match l with
    | [] -> compressed
    | hd :: tl -> (
        match mem with
        | None ->
            (* None = The first element of the list, so it will always be appended *)
            loop tl (Some hd) [ hd ]
        | Some m ->
            let exists = List.exists (fun s -> s = m) l in

            if (hd = m && exists == false) || not (hd = m) then
              let compressed = List.append compressed [ hd ] in
              loop tl (Some hd) compressed
            else loop tl (Some hd) compressed)
  in

  loop l None []

(* res = ["a"; "b"; "c"; "a"; "d"; "e"] *)
let res =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
