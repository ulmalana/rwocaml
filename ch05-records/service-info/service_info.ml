open Core

#require "re"

type 'a with_line_num = { item: 'a; line_num: int }

type service_info =
{
  service_name : string;
  port : int;
  protocol : string;
  comment : string option;
}

let service_info_of_string line =
  let matches =
    let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
    Re.exec (Re.Posix.compile_pat pat) line
  in
  {
    service_name = Re.Group.get matches 1;
    port = Int.of_string (Re.Group.get matches 2);
    protocol = Re.Group.get matches 3;
    comment = Some "some comment";
  }

let service_info_of_string' line =
  let (line, comment) =
    match String.rsplit2 line ~on:'#' with
    | None -> (line, None)
    | Some (ordinary, comment) -> (ordinary, Some comment)
  in
  let matches =
    Re.exec (Re.Posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  let service_name = Re.Group.get matches 1 in
  let port = Int.of_string (Re.Group.get matches 2) in
  let protocol = Re.Group.get matches 3 in
  { service_name; port; protocol; comment}

let ssh = service_info_of_string "ssh 22/udp # ssh remote login protocol"

let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
    {
      item = parse line;
      line_num = line_num + 1;
    })

let parsed_file = parse_lines service_info_of_string
  "rtmp  1/ddp # routing table maintenance protocol
   tcpmux 1/udp # tcp port service muxer
   tcpmux 1/tcp # tcp port service muxer"

let parsed_number = parse_lines Int.of_string "1\n10\n100\n1000"

let service_info_to_string
    { service_name = name; port = port; protocol = prot } =
  sprintf "%s %i/%s" name port prot

let service_info_to_string' { service_name; port; protocol; comment } =
  let base = sprintf "%s %i/%s" service_name port protocol in
  match comment with
  | None -> base
  | Some text -> base ^ " # " ^ text

let create_service_info ~service_name ~port ~protocol ~comment =
  { service_name; port; protocol; comment}
