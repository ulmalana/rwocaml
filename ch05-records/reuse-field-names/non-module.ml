open Core

type log_entry = {
  session_id : string;
  time : Time_ns.t;
  important : bool;
  message : string;
}

type heartbeat = {
  session_id : string;
  time : Time_ns.t;
  status_message : string;
}

type logon = {
  session_id : string;
  time : Time_ns.t;
  user: string;
  credentials: string;
}

(* confusing field name *)
let get_session_id t = t.session_id

(* using type annotation *)
let get_heartbeat_session_id (t : heartbeat) = t.session_id


