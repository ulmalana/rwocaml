open Base
open Stdio

type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type weight = Regular | Bold

type color =
  | Basic of basic_color * weight
  | RGB of int * int * int (* 6x6x6 color cube *)
  | Gray of int (* 24 grayscale level *)

type color' =
  | Basic' of basic_color
  | Bold' of basic_color
  | RGB' of int * int * int
  | Gray' of int 

let basic_color_to_int = function
  | Black -> 0 | Red -> 1     | Green -> 2 | Yellow -> 3
  | Blue -> 4  | Magenta -> 5 | Cyan -> 6  | White -> 7

let color_by_number number text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text

let blue = color_by_number (basic_color_to_int Blue) "Blue"

(* printf "halo %s world\n" blue;; *)

let color_to_int = function
  | Basic (basic_color, weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basic_color_to_int basic_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

let color_to_int' = function
  | Basic' basic_color -> basic_color_to_int basic_color
  | Bold' basic_color -> 8 + basic_color_to_int basic_color 
  | RGB' (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray' i -> 232 + i

let oldschool_color_to_int = function
  | Basic (basic_color, weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basic_color_to_int basic_color
  | _ -> basic_color_to_int White

let color_print color s =
  printf "%s\n" (color_by_number (color_to_int color) s)
