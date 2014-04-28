{
(* Module used for url file parsing only. *)
(* TODO: use custom type instead of string as list key? *)
}

let layout = [' ' '\t' '\n']

let field = "src" | "archive" | "http" | "local" | "git" | "darcs" | "hg"
            | "checksum" | "mirrors"


rule main acc = parse

| eof { List.rev acc }

| layout { main acc lexbuf }

| (field as f) layout* ":" layout*
  { let s = value lexbuf in main ((f, s) :: acc) lexbuf }

| _ as c { failwith ("unexpected char" ^ String.make 1 c) }

and value = parse

| "\"" { string (Buffer.create 16) lexbuf }

| "[" { list (Buffer.create 16) lexbuf }

and list buff = parse

(* TODO: do something useful (such as parse list) *)

| "]" { Buffer.add_char buff ']'; Buffer.contents buff }

| "\"" { let s = string (Buffer.create 16) lexbuf in
         Buffer.add_string buff s; list buff lexbuf }

| _ as c { Buffer.add_char buff c; list buff lexbuf }

and string buff = parse

| "\\\"" { Buffer.add_char buff '"'; string buff lexbuf }

| "\"" { Buffer.contents buff }

| _ as c { Buffer.add_char buff c; string buff lexbuf }

{ 
  let parse lexbuf = main [] lexbuf
  let from_string s = parse (Lexing.from_string s)
  let from_channel c = parse (Lexing.from_channel c)
  let from_filename f = from_channel (open_in f)
}
