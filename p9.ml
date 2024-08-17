(*
   TODO: add generic implementation, right now the pack method only works for string
   values, but the problem solution needs to be generic
*)

(* Eliminate consecutive duplicates of list elements. *)

let pack l =
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

  let rec loop l mem (packed : 'a list list) =
    match l with
    | [] -> packed
    | hd :: tl -> (
        let last_packed = last packed in
        match last_packed with
        | None ->
            let packed = [ [ hd ] ] in
            loop tl hd packed
        | Some last_packed ->
            if hd = mem then
              let p = List.append last_packed [ hd ] in
              let packed = update_last packed [] p in
              loop tl hd packed
            else if not (hd = mem) then
              let packed = List.append packed [ [ hd ] ] in
              loop tl hd packed
            else loop tl hd packed)
  in

  loop l "" []

(* res = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
   ["e"; "e"; "e"; "e"]] *)
let res =
  pack
    [
      "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e";
    ]
