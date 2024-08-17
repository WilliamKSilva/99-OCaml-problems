(* Eliminate consecutive duplicates of list elements. *)

let compress l =
  let rec loop l mem compressed =
    match l with
    | [] -> compressed
    | hd :: tl ->
        let exists = List.exists (fun s -> s == mem) l in

        if (hd == mem && exists == false) || hd != mem then
          let compressed = List.append compressed [ hd ] in
          loop tl hd compressed
        else loop tl hd compressed
  in

  loop l "" []

(* res = ["a"; "b"; "c"; "a"; "d"; "e"] *)
let res =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
