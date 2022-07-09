open Base

module Username : sig
    (* the signatures of each entity contained in Username *)
    type t

    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
end = struct
    (* the real implementation of each entity with signature defined above *)
    type t = string

    let of_string x = x
    let to_string x = x
    let ( = ) = String.( = )

end
