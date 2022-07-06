open Base

(* Counter module v0 using assoc list
type t = (string * int) list

let empty = []

let to_list x = x

let touch counts line = 
    let count =
      match List.Assoc.find ~equal:String.equal counts line with
      | None -> 0
      | Some x -> x
    in
    List.Assoc.add ~equal:String.equal counts line (count + 1)

*)

(* Counter module v1 using Map which is more efficient than assoc list. 
 * The older module and this newer one are shown for comparison. In reality,
 * we just change each definition to something else. 
 *)
    
type t = int Map.M(String).t

let empty = Map.empty (module String)

let to_list t = Map.to_alist t

let touch t s =
    let count =
      match Map.find t s with
      | None -> 0
      | Some x -> x
    in
    Map.set t ~key:s ~data:(count + 1)
