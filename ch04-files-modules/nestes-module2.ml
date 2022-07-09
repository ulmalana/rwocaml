open Base
module Time = Core.Time

(* this signature module can be used for multiple entities *)
module type ID = sig
    type t

    val of_string = string -> t
    val to_string = t -> string
    val ( = ) = t -> t -> string
end

module String_id = struct
    type t = string

    let of_string x = x
    let to_string x = x
    let ( = ) = String.( = )
end

(* both modules below use the same signature but are two different entity *)
module Username : ID = String_id
module Hostname : ID = String_id

type session_info =
    { user : Username.t;
      host : Hostname.t;
      when_started : Time.t
    }

(* the function below wont compiled since it compares different entity
 * (username vs. hostname ) *)
let sessions_have_same_name s1 s2 = String.( = ) s1.user s2.host
