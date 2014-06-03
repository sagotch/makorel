open Digest
open Str
open Sys
open Unix

open Curl
open Urlex
open Unixx

(* Strip the path and a trailing '/' if present *)
let package_name dir =
  Str.global_replace (Str.regexp "\\(^.*/\\|^\\)\\([^/]+\\)/?$") "\\2" dir

(* Strip everything before the first dot, the dot, and a trailing '/'
 * if present *)
let prev_version dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort Pervasives.compare
  |> List.rev
  |> List.hd
  |> Str.global_replace (Str.regexp "^[^\\.]*.\\([^/]+\\)/?$") "\\1"
  
(* replace string part matching  x with y *)
let replace str x y =
  Str.global_replace (Str.regexp (Str.quote x)) y str

(* FIXME: May avoid hardcoded file content?
 * or at least avoid global_replace. *)
let init root_dir version =
  let package_name = package_name root_dir in
  let opam_items =
    "opam-version: STRING\n\
     # name: STRING\n\
     # version: STRING\n\
     maintainer: STRING\n\
     # homepage: STRING\n\
     # authors: [ STRING+ ]\n\
     # doc: STRING\n\
     # license: STRING\n\
     # tags: [ STRING+ ]\n\
     # subst: [ STRING+ ]\n\
     # patches: [ (STRING { <filter> } )+ ]\n\
     # build: commands\n\
     # build-doc: commands\n\
     # build-test: commands\n\
     # remove: commands\n\
     # depends: [ <and-formula(package)>+ ]\n\
     # depopts: [ <or-formula(package)>+ ]\n\
     # depexts: [ [[STRING+] [STRING+]]+ ]\n\
     # conflicts: [ <package>+ ]\n\
     # messages: [ (STRING { <filter> } )+ ]\n\
     # post-messages: [ (STRING { <filter> } )+ ]\n\
     # available: [ <filter> ]\n\
     # os: [ <formula(os)>+ ]\n\
     # ocaml-version: [ <and-formula(constraint)>+ ]\n\
     # libraries: [ STRING+ ]\n\
     # syntax: [ STRING+ ]\n" in
  let url_items =
    "# src: STRING\n\
     # archive: STRING\n\
     # http: STRING\n\
     # local: STRING\n\
     # git: STRING\n\
     # darcs: STRING\n\
     # hg: STRING\n\
     # mirrors: [ STRING+ ]\n\
     # checksum: STRING\n" in
  let write_file path content =
    let out = Pervasives.open_out path in
    Pervasives.output_string out content;
    Pervasives.close_out out in
  let dir = root_dir ^ "/" ^ package_name ^ "." ^ version in
  mkdir_p dir;
  write_file (dir ^ "/descr") (package_name ^ " - No description yet.\n");
  write_file (dir ^ "/opam") opam_items;
  write_file (dir ^ "/url") url_items

let upgrade root_dir new_ver =

  let package_name = package_name root_dir in
  let prev_ver = prev_version root_dir in

  let src_dir = (root_dir ^ "/" ^ package_name ^ "." ^ prev_ver) in
  let dst_dir = (root_dir ^ "/" ^ package_name ^ "." ^ new_ver) in

  (* Note: copying url file is useless since we will rewrite it. *)  
  cp_r src_dir dst_dir;

  (* Get the value of the first (and supposed only) key matching 
   * a location keyword  *)
  let main_url fields =
    let keys = ["src"; "archive"; "http"; "local"; "git"; "hg"; "darcs"] in
    snd (List.find (fun (k, _) -> List.mem k keys) fields) in

  (* parse original url file *)
  let url_fields =
    Urlex.from_filename
      (root_dir ^ "/" ^ package_name ^ "." ^ prev_ver ^ "/url") in

  (* find url for the new version replacing the version number 
   * in the previous url *)
  let new_url = replace (main_url url_fields) prev_ver new_ver in
  
  (* update every fields according to this new url *)
  let processed_fields = 
    let process_field (k, v) = match k with

      | "src" | "archive"| "http" | "local" | "git" | "darcs" | "hg" ->
         (k, replace v prev_ver new_ver)

      | "checksum" ->
         (* may need to handle any curl failure? *)
         let buf = Buffer.create 256 in
         let h = new Curl.handle in
         h#set_post false;
         h#set_url new_url;
         h#set_followlocation true;
         h#set_failonerror true;
         h#set_writefunction (fun s -> Buffer.add_string buf s;
                                       String.length s);
         h#perform;
         h#cleanup;
         (k, Buffer.contents buf
             |> Digest.string
             |> Digest.to_hex)

      | "mirrors" -> (k, v) (* TODO: something interesting *)

      | _ -> failwith ("unknown field " ^ k)

    in List.map process_field url_fields in

  (* write to url file *)
  let url_file =
    open_out (root_dir ^ "/" ^ package_name ^ "." ^ new_ver ^ "/url") in
  List.iter (fun (k, v) -> output_string url_file k;
                           output_string url_file ": \"";
                           output_string url_file v;
                           output_string url_file "\"\n") processed_fields;
  close_out url_file;
  print_endline ("Version " ^ new_ver ^ " files (based on " ^ prev_ver
                 ^ ") of package " ^ package_name ^  " succesfully created.")


(* main function *)
let _ =

  let makorel_version = "0.2.1" in

  let create = ref false in
  let version = ref "" in
  let root_dir = ref "" in

  let usage = "usage: makorel [options]" in

  let options =
    Arg.align [
        "-i", Arg.Set create,
        " initialize a new package release (not based on a previous one).";

        "-v", Arg.Set_string version,
        "version Package version to create. MANDATORY.";

        "-p", Arg.Set_string root_dir,
        "package Package root directory. Default is current directory.";

        "--about", Arg.Unit (fun () ->
                             print_endline ("makorel version: "
                                            ^ makorel_version);
                             print_endline usage;
                             exit 0),
        " Print makorel version and usage and exit.";
      ] in

  Arg.parse options (fun s -> raise (Arg.Bad ("Unknown option: " ^ s))) usage;

  let version = if !version <> "" then !version
                else failwith "missing parameter: version." in
  let root_dir = if !root_dir <> "" then !root_dir else Sys.getcwd () in
  let root_dir = Str.global_replace (Str.regexp "/$") "" root_dir in

  if !create then init root_dir version
  else upgrade root_dir version
