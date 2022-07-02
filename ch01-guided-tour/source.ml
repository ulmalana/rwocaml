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

(* Record type: a tuple where individual fields are named *)
type point2d = {x: float; y: float}

let p = {x = 3.; y = 4.}

let magnitude {x = x_pos; y = y_pos} = 
    Float.sqrt(x_pos **. 2. +. y_pos **. 2.)

(* more terse version of magnitude with field punning 
 * since x and y are known *)
let magnitude' {x; y} = Float.sqrt(x **. 2. +. y **. 2.)

(* use dot notation to access fields *)
let distance' v1 v2 = 
    magnitude' {x = v1.x -. v2.x; y = v1.y -. v2.y}

(* defining geometric objects with point2d type *)
type circle_desc = { center: point2d; radius: float }
type rect_desc = { lower_left: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d }

(* combining multiple record type in a variant type *)
type scene_element = 
    | Circle of circle_desc
    | Rect of rect_desc
    | Segment of segment_desc

(* checking whether a point is in the interior of some element
 * of scene_element object *)
let is_inside_scene_element point scene_element = 
    let open Float.O in
    match scene_element with
    | Circle {center; radius} ->
      distance' center point < radius
    | Rect {lower_left; width; height} ->
      point.x > lower_left.x && point.x < lower_left.x + width
      && point.y > lower_left.y && point.y < lower_left.y + height
    | Segment _ -> false

let is_inside_scene point scene = 
    List.exists scene
      ~f:(fun el -> is_inside_scene_element point el)

(* Data in OCaml is immutable by default 
 * There are some data structure that are mutable such as Array.
 * Array is more compact in terms of memory utilization than most 
 * data structure in Ocaml *)
let numbers = [| 1; 2; 3; 4 |]

(* accessing and changing the third element of numbers
 *  numbers.(2) <- 4;; *)

(* Record type is immutable by default. To make its fields mutable,
 * we need to declare it explicitly *)
type running_sum = 
  { mutable sum: float;
    mutable sum_sq: float;
    mutable samples: int;
  }

let mean rsum = rsum.sum /. Float.of_int rsum.samples
let stdev rsum = 
  Float.sqrt
    (rsum.sum_sq /. Float.of_int rsum.samples -. mean rsum **. 2.)

let create () = {sum = 0.; sum_sq = 0.; samples = 0}
let update rsum x =
  rsum.samples <- rsum.samples + 1;
  rsum.sum <- rsum.sum +. x;
  rsum.sum_sq <- rsum.sum_sq +. x *. x

(* we can create a single mutable value with ref.
 * ref is a record type with a single mutable field called contents. *)
let x1 = { contents = 0}

let x2 = ref 0
(* change the value of x
 * x1.contents <- x1.contents + 5
 * or 
 * x1! *)

(* example: summing a list and storing the result in a ref type *)
let sum_ref list = 
  let sum = ref 0 in
  List.iter list ~f:(fun x -> sum := !sum + x);
  !sum

(* for loop *)
let permute array = 
  let length = Array.length array in
  for i = 0 to length - 2 do
    let j = i + Random.int (length - i) in
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done

(* while loop *)
let find_first_negative_entry array = 
    let pos = ref 0 in
    while !pos < Array.length array && array.(!pos) >= 0 do
      pos := !pos + 1
    done;
    if !pos = Array.length array then None else Some !pos
