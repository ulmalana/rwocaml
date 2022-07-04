(* this function drop a value from a list *)
let rec drop_value l to_drop = 
    match l with
    | [] -> []
    | hd :: tl ->
        let new_tl = drop_value tl to_drop in
        if hd = to_drop then new_tl else hd :: new_tl


(* for comparing the performance of match and if *)
let plus_one_match x =
    match x with
    | 0 -> 1
    | 1 -> 2
    | 2 -> 3
    | 3 -> 4
    | 4 -> 5
    | 5 -> 6
    | _ -> x + 1

let plus_one_if x = 
    if x = 0 then 1
    else if x = 1 then 2
    else if x = 2 then 3
    else if x = 3 then 4
    else if x = 4 then 5
    else if x = 5 then 6
    else x + 1

let rec sum l = 
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl

let rec sum_if l = 
    if List.is_empty l then 0
    else List.hd_exn l + sum_if (List.tl_exn l)
