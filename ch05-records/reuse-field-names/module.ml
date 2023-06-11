open Core

#require "core_unix"
#require "ppx_jane"

module Log_entry = struct
  type t =
    {
      session_id: string;
      time: Time_ns.t;
      important: bool;
      message: string;
    }
end

module Heartbeat = struct
  type t =
    {
      session_id: string;
      time: Time_ns.t;
      status_message: string;
    }
end

module Logon = struct
  type t =
    {
      session_id: string;
      time: Time_ns.t;
      user: string;
      credentials: string;
    }
    [@@deriving fields]
end

type client_info =
  {
    addr: Core_unix.Inet_addr.t;
    port: int;
    user: string;
    credentials: string;
    last_heartbeat_time: Time_ns.t;
    last_heartbeat_status: string; (* new field *)
  }

type client_info_mut =
  {
    addr: Core_unix.Inet_addr.t;
    port: int;
    user: string;
    credentials: string;
    mutable last_heartbeat_time: Time_ns.t; (* set mutable *)
    mutable last_heartbeat_status: string; (* set mutable *)
  }

let create_log_entry ~session_id ~important message =
  {
    Log_entry.time = Time_ns.now ();
    Log_entry.session_id;
    Log_entry.important;
    Log_entry.message
  }

let create_log_entry' ~session_id ~important message =
  { Log_entry.
    time = Time_ns.now (); session_id; important; message
  }

let create_log_entry'' ~session_id ~important message : Log_entry.t =
  { time = Time_ns.now (); session_id; important; message}

let message_to_string { Log_entry.important; message; _} =
  if important then String.uppercase message else message

let is_important t = t.Log_entry.important

(* or *)
let message_to_string' ({important; message; _} : Log_entry.t) =
  if important then String.uppercase message else message

let is_important' (t : Log_entry.t) = t.important

let register_heartbeat (t: client_info) hb =
  {
    addr = t.addr;
    port = t.port;
    user = t.user;
    credentials = t.credentials;
    last_heartbeat_time = hb.Heartbeat.time;
  }

(* with functional update syntax *)
let register_heartbeat' (t : client_info ) hb =
  { t with last_heartbeat_time = hb.Heartbeat.time }

let register_heartbeat'' (t: client_info) hb =
  { t with last_heartbeat_time = hb.Heartbeat.time ;
           last_heartbeat_status = hb.Heartbeat.status_message;
  }

let register_heartbeat_mut t (hb:Heartbeat.t) =
    t.last_heartbeat_time <- hb.time;
    t.last_heartbeat_status <- hb.status_message


let get_users logons =
  List.dedup_and_sort ~compare:String.compare
    (List.map logons ~f:(fun x -> x.Logon.user))

let get_users' logons =
  List.dedup_and_sort ~compare:String.compare
    (List.map logons ~f:Logon.user)

let show_field field to_string record =
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
  name ^ ": " ^ field_string

let logon =
  { Logon.
    session_id = "1234";
    time = Time_ns.of_string_with_utc_offset "2023-06-11 11:04:22Z";
    user = "riz";
    credentials = "987654321";
  }

let print_logon logon =
  let print to_string field =
    printf "%s\n" (show_field field to_string logon)
  in
  Logon.Fields.iter
    ~session_id:(print Fn.id)
    ~time:(print Time_ns.to_string_utc)
    ~user:(print Fn.id)
    ~credentials:(print Fn.id)
