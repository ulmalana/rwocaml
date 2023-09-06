open Base
open Ppx_jane
open Ppx_let
open Stdio

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

(* Exception           *)
exception Key_not_found of string 

let exceptions = [Division_by_zero; Key_not_found "b"]

let rec find_exn alist key = match alist with
    | [] -> raise (Key_not_found key)
    | (key', data) :: tl -> if String.(=) key key' 
                            then data 
                            else find_exn tl key 

let alist = [("a", 1); ("b", 2)]

(* type 'a bounds = {lower: 'a; upper : 'a} *)
type 'a bounds = {lower: 'a; upper : 'a} [@@deriving sexp]

(* exception Crossed_bounds of int bounds *)
exception Crossed_bounds of int bounds [@@deriving sexp]

let merge_lists xs ys ~f = 
  if List.length xs <> List.length ys then None 
  else 
    let rec loop xs ys =
      match xs,ys with
      | [],[] -> []
      | x::xs, y::ys -> f x y :: loop xs ys 
      | _ -> assert false 
    in 
    Some (loop xs ys)

let merge_lists' xs ys ~f = 
    let rec loop xs ys =
      match xs,ys with
      | [],[] -> []
      | x::xs, y::ys -> f x y :: loop xs ys 
      | _ -> assert false 
    in 
    loop xs ys

let parse_line line = 
  String.split_on_chars ~on:[','] line 
  |> List.map ~f:Float.of_string

let load filename = 
  let inc = In_channel.create filename in 
  let data = 
    In_channel.input_lines inc 
    |> List.map ~f:parse_line 
  in 
  In_channel.close inc;
  data 
  
let load' filename = 
  let inc = In_channel.create filename in 
  Exn.protect 
  ~f:(fun () -> In_channel.input_lines inc |> List.map ~f:parse_line)
  ~finally:(fun () -> In_channel.close inc)

let load'' filename = 
  In_channel.with_file filename ~f:(fun inc -> 
    In_channel.input_lines inc |> List.map ~f:parse_line) 

let lookup_weight ~compute_weight alist key = 
  try 
    let data = find_exn alist key in 
    compute_weight data 
  with 
    Key_not_found _ -> 0. 

let lookup_weight' ~compute_weight alist key =
  match 
    try Some (find_exn alist key)
    with _ -> None 
  with 
  | None -> 0. 
  | Some data -> compute_weight data 

let lookup_weight'' ~compute_weight alist key = 
  match find_exn alist key with 
  | exception _ -> 0. 
  | data -> compute_weight data 

let lookup_weight''' ~compute_weight alist key = 
  match List.Assoc.find ~equal:String.equal alist key with 
  | None -> 0. 
  | Some data -> compute_weight data 


