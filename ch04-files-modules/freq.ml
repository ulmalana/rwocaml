open Base
open Stdio
(* build_counts v0
 *
let build_counts () =
    In_channel.fold_lines In_channel.stdin ~init:[] ~f:(fun counts line ->
        let count =
          match List.Assoc.find ~equal:String.equal counts line with
          | None -> 0
          | Some x -> x
        in
        List.Assoc.add ~equal:String.equal counts line (count + 1))
*)

(* build_counts v1 with Counter module
let build_counts () =
    In_channel.fold_lines In_channel.stdin ~init:[] ~f:Counter.touch
*)

(* build_counts v2 with mli interface
 * with interface, we can just modify the counter module if we want
 * to change the data structure, for example *)
let build_counts () =
    In_channel.fold_lines In_channel.stdin
      ~init:Counter.empty ~f:Counter.touch

let () =
    build_counts()
    |> Counter.to_list  (* add this after implementing interface *)
    |> List.sort ~compare:(fun (_,x) (_,y) -> Int.descending x y)
    |> (fun l -> List.take l 10)
    |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
