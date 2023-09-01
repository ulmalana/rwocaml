open Base
open Ppx_jane
open Ppx_let

let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in 
  match List.hd sorted, List.last sorted with 
  | None, _ | _, None -> None 
  | Some x, Some y -> Some (x,y)

let find_mismatches table1 table2 = 
  Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data mismatches -> 
    match Hashtbl.find table2 key with 
    | Some data' when data' <> data -> key :: mismatches
    | _ -> mismatches)

let float_of_string s = 
  Or_error.try_with (fun () -> Float.of_string s)

(* let a = "foo" and b = ("foo", [3;4]) *)

(* Or_error.error_s *)
  (* [%message "Something went wrong" (a:string) (b:string * int list)] *)


let compute_bounds' ~compare list = 
  let sorted = List.sort ~compare list in 
  Option.bind (List.hd sorted) ~f:(fun first -> 
    Option.bind (List.last sorted) ~f:(fun last -> 
      Some (first, last)))

let compute_bounds' ~compare list = 
      let open Option.Monad_infix in 
      let sorted = List.sort ~compare list in 
      List.hd sorted >>= fun first -> 
        List.last sorted >>= fun last -> 
          Some (first, last)

(* let compute_bounds'' ~compare list = 
    let open Option.Let_syntax in 
    let sorted = List.sort ~compare list in 
    let%bind first = List.hd sorted in 
    let%bind last = List.last sorted in 
    Some (first, last) *)

let compute_bounds''' ~compare list = 
          let sorted = List.sort ~compare list in 
          Option.both (List.hd sorted) (List.last sorted)

