open Base

module Interval = struct
  type t = | Interval of int * int
           | Empty

  let create low high =
    if high < low then Empty else Interval (low, high)
end
