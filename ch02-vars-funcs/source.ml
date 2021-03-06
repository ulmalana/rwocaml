(* Chapter 02 - Variables and Functions *)

open Base

(* Variables *)
(* variable binding syntax 
 * let <variable> = <expr> *)
let x = 3
let y = 4
let z = x + y

(* introducing new binding for limited scope 
 * let <variable> = <expr1> in <expr2> *)
let languages = "Ocaml,Perl,C++,Haskell"
let dashed_languages = 
  let language_list = String.split languages ~on:',' in
  String.concat ~sep:"-" language_list (* language_list only available here*)

(* nesting let/in *)
let area_of_ring inner_radius outer_radius = 
    let pi = Float.pi in
    let area_of_circle r = pi *. r *. r in
    area_of_circle outer_radius -. area_of_circle inner_radius


(* pattern matching with let *)
let (ints, strings) = List.unzip [(1, "one"); (2, "two"); (3, "three")]

(* some type like list is not irrefutable, so sometimes pattern matching 
 * using let does not really work as intended. 
 * consider using match expression in that case. *)
let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false (*String.split always returns at least one element *)
  | first :: rest -> 
    String.concat ~sep:"," (String.uppercase first :: rest);


(* Functions *)

(* anonymous function *)
(fun x -> x + 1) 8;;
List.map ~f:(fun x -> x + 1) [1;2;3];;

(* insert functions into a data structure *)
let transforms = [String.uppercase; String.lowercase];;
List.map ~f:(fun g -> g "Halo Riz") transforms;;

(* naming anomymous function *)
let plusone = (fun x -> x + 1);;
let plusone' x = x + 1;; (* the syntactic sugar *)

(* Multiargument functions *)
let abs_diff x y = abs (x - y);;
let abs_diff' =         
        (fun x -> (fun y -> abs (x - y)));;

(* recursive functions *)
let rec find_first_repeat list =
    match list with
    | [] | [_] -> None
    | x :: y :: tl -> if x = y then Some x else find_first_repeat (y::tl)

(* defining multiple mutually recursive function with let rec combined
 * with and keyword *)
let rec is_even x = 
  if x = 0 then true else is_odd (x - 1)
and is_odd x =
  if x = 0 then false else is_even (x - 1)

(* we can define infix operator using specialized char, as long as not
 * started with ~, !, or $.
 * Example: simple vector addition *)
let (+!) (x1,y1) (x2,y2) = (x1+x2, y1+y2)
let ( *** ) x y = (x **. y) **. y

(* |> is reverse application operator, while @@ is application operator
 * let (|>) x f =  f x  
 * f (g (h x)) ==  f @@ g @@ h x *)

(* Declaring functions with function keyword *)
let some_or_zero = function
    | Some x -> x
    | None -> 0

(* the equivalent*)
let some_or_zero' num_opt =
    match num_opt with
      | Some x -> x
      | None -> 0

(* with multiple arguments *)
let some_or_default default = function
    | Some x -> x
    | None -> default

(* Labeled arguments *)
let ratio ~num ~denom = Float.of_int num /. Float.of_int denom

(* the order of labeled arguments in higher order function matters *)
let apply_to_tupple f (first,second) = f ~first ~second
let apply_to_tupple' f (first,second) = f ~second ~first

let divide ~first ~second = first / second

(* we can run this:
    apply_to_tuple divide (3,4)

   but not this since it has different ordering:
    apply_to_tuple' divide (3,4) 
*)

(* Optional arguments
 
    Use ? to indicate optional arguments
*)
let concat ?sep x y = (* sep is optional for separating values *)
    let sep = match sep with None -> "" | Some s -> s in
    x ^ sep ^ y

(* optional argument with default value *)
let concat' ?(sep="") x y = x ^ sep ^ y

(* when to use optional arguments?
    - rarely used functions should not have optional arguments 
    - avoid optional arguments for functions internal to a module
*)

(* Optional argument is erased as soon as the first positional
    (neither labeled nor optional) argument defined after the optional 
    argument is passed in. 
*)

let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b

(* this function computes the derivative of a function.
    the types in this function are inferred 
*)
let numeric_deriv ~delta ~x ~y ~f =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~x:x' ~y -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx,dy)

