open Base

let square x = x * x

let ratio x y = Float.of_int x /. Float.of_int y

let ratio' x y = 
    let open Float.O in
    of_int x / of_int y

let ratio'' x y =
    Float.O.(of_int x / of_int y)

let sum_if_true test first second = 
    (if test first then first else 0)
    + (if test second then second else 0)

(* sum_if_true with explicit type annotation*)
let sum_if_true' (test: int -> bool) (x: int) (y: int) : int =
    (if test x then x else 0) 
    + (if test y then y else 0)

let even x = x % 2 = 0

(* generic type 
* the functon below has type:
* ('a -> bool) -> 'a -> 'a -> 'a = <fun>
* 'a is type variable
* *)
let first_if_true test x y =
    if test x then x else y

let long_string s = String.length s > 6
let big_number x =  x > 3

(* Tuple *)
let a_tuple = (3, "three") (* this has type int * string *)
let another_tuple = (3, "three", 5.0) (* this has type int * string * float *)

let distance (x1,y1) (x2,y2) =
    Float.sqrt ((x1 -. x2) **. 2. +. (y1 -. y2) **. 2.)

(* List *)
(* let languages = ["OCaml"; "Perl"; "C"]
List.length languages

List.map languages ~f:String.length
*)

(* Pattern matching *)
(* non-exhaustive *)
let my_fav_lang (my_fav :: rest) = my_fav

(* exhaustive pattern matching *)
let my_fav_lang' languages =
    match languages with
    | first :: rest -> first
    | [] -> "Ocaml" (* default *)


(* recursive function *)
let rec sum l =
    match l with
    | [] -> 0                   (* base case *)
    | hd :: tl -> hd + sum tl   (* inductive case *)

let rec remove_sequential_duplicates list =
    match list with
    | [] -> []
    | [x] -> [x]
    | first :: second :: tl ->
        if first = second then
            remove_sequential_duplicates (second :: tl)
        else
            first :: remove_sequential_duplicates (second :: tl)

(* Option datatype 
* equivalent to Haskell Maybe *)
let divide x y =
    if y = 0 then None else Some (x / y)

(* this function will downcase the format of a file
* ex: TXT to txt *)
let downcase_extension filename =
    match String.rsplit2 filename ~on:'.' with
    | None -> filename
    | Some (base, ext) -> 
        base ^ "." ^ String.lowercase ext
