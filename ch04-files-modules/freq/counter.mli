open Base

(* a collection of string freq counts *)
type t

type median =
  | Median of string
  | Before_and_after of string * string

(* empty set of freq counts *)
val empty : t

val touch : t -> string -> t

val to_list : t -> (string * int) list

val median : t -> median
