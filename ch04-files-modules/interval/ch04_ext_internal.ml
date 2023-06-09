open Base

module Extended_interval = struct

  include Interval

  let contains t x =
    match t with
    | Empty -> false
    | Interval (low, high) -> x >= low && x <= high

end
