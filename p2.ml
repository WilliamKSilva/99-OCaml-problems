(* Find the last but one (last and penultimate) elements of a list *)

let last_two l =
  let rec loop l penultimate =
    match l with
    | [] -> None
    | hd :: tl ->
        if List.length tl == 0 then Some (Option.get penultimate, hd)
        else if List.length tl == 1 then loop tl (Some hd)
        else loop tl None
  in

  loop l None

(* res = Some ("c", "d") *)
let res = last_two [ "a"; "b"; "c"; "d" ]
