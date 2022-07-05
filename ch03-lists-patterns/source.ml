(* module Sys = Sys_unix
module Filename = Core.Filename
*)

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

(* rendering a table from a list *)
let max_widths header rows =
    let lengths l = List.map ~f:String.length l in
    List.fold rows
      ~init:(lengths header)
      ~f:(fun acc row ->
            List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
    let pieces = List.map widths
        ~f:(fun w -> String.make w '-')
    in
    "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

let pad s length =
    s ^ String.make (length - String.length s) ' ' 

let render_row row widths = 
    let padded = List.map2_exn row widths ~f:pad in
    "| " ^ String.concat ~sep:" | " padded ^ " |"

(* concat and ^ does the same job, but for large data concat is better 
 * because ^ will create new string for every concatenation *)
let render_table header rows =
    let widths = max_widths header rows in
    String.concat ~sep:"\n"
      (render_row header widths
        :: render_separator widths
        :: List.map rows ~f:(fun row -> render_row row widths)
      )

(* extract file extensions from a list of files *)
let extensions filenames =
    List.filter_map filenames ~f:(fun fname ->
      match String.rsplit2 ~on:'.' fname with
      | None | Some ("",_) -> None
      | Some (_, ext) -> Some ext)
    |> List.dedup_and_sort ~compare:String.compare

(* Splitting a list into ml files and other files *)
let is_ocaml_source s =
    match String.rsplit2 s ~on:'.' with
    | Some (_, ("ml"|"mli")) -> true
    | _ -> false

let (ml_files, other_files) = 
  List.partition_tf ["foo.c"; "foo.ml"; "bar.ml"; "haha.hs"] ~f:is_ocaml_source

(* listing a directory
 * currently doesnt works since because of Core.Sys deprecation
let rec ls_rec s =
    if Sys.is_file_exn ~follow_symlinks:true s
    then [s]    
    else
        Sys.ls_dir s
        |> List.map ~f:(fun sub -> ls_rec (Filename.concat s sub))
        |> List.concat
        (* or like this 
         * Sys.ls_dir s
         * |> List.concat_map ~f:(fun sub -> ls_rec (Filename.concat s sub)) 
         * *)
*)

(* computing the length of list: unoptimized and tail-call optimized *)

let make_list n = List.init n ~f:(fun x -> x)

let rec length = function
    | [] -> 0
    | _ :: tl -> 1 + length tl

(* optimized *)
let rec length_plus_n l n =
    match l with
    | [] -> n
    | _ :: tl -> length_plus_n tl (n+1)

let length' l = length_plus_n l 0

(* recreating terser version remove_sequential_duplicates function
 * from chapter 1.
 * this only works with integer *)
let rec remove_sequential_duplicates list =
    match list with
    | [] | [_] as l -> l
    | first :: (second :: _ as tl) when first = second ->
        remove_sequential_duplicates tl
    | first :: tl -> first :: remove_sequential_duplicates tl
