open Digest
open Str
open Sys
open Unix

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

(* replace string part matching x with y *)
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

  let update_url old_file new_file =

    (* find url for the new version replacing the version number
     * in the previous url *)
    let rep_url url = replace url prev_ver new_ver in

    let whitespace = "[ \t]*" in
    let line_regex =
      Str.regexp
        (whitespace
         ^ "\\(archive\\|checksum\\|darcs\\|git\\|hg\\|local\\|http\\|src\\)"
         ^ whitespace ^ ":" ^ whitespace
         ^ "\\\"\\([^\\\"]*\\)\\\"" ^ whitespace) in
    (* \1 is field name, \2 is its value *)

    let checksum url =
         let curl_cmd = "curl -Lsf " ^ url in
         let curl_out = open_process_in curl_cmd in
         let checksum = Digest.channel curl_out (-1) |> Digest.to_hex in
         if Unix.close_process_in curl_out <> Unix.WEXITED 0
         then failwith curl_cmd
         else checksum in

    let in_ch = Pervasives.open_in old_file in
    let out_ch = Pervasives.open_out new_file in
    let output line = Pervasives.output_string out_ch line;
                      Pervasives.output_char out_ch '\n' in
    let main_url = ref "" in

    let rec update_line () =
      try let line = Pervasives.input_line in_ch in
          let line =
            if Str.string_match line_regex line 0
            then match Str.matched_group 1 line with
                 | "src" | "archive" | "http" | "local" ->
                   main_url := rep_url (Str.matched_group 2 line);
                   rep_url line
                 | "checksum" ->
                    if !main_url = ""
                    then "#" ^ line
                    else Str.string_before line (Str.group_beginning 2)
                         ^ checksum !main_url
                         ^ Str.string_after line (Str.group_end 2)
                 | _ -> rep_url line
            else rep_url line in
          output line;
          update_line ()
      with End_of_file ->
        Pervasives.close_in in_ch;
        Pervasives.close_out out_ch;
    in
    update_line ();
    print_endline @@ "Version " ^ new_ver ^ " files (based on " ^ prev_ver
                     ^ ") of package " ^ package_name
                     ^ " succesfully created."
  in
  update_url
    (src_dir ^ "/url")
    (dst_dir ^ "/url")

(* main function *)
let _ =

  let makorel_version = "0.2.2" in

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
                             print_string "makorel version: " ;
                             print_endline makorel_version ;
                             print_endline usage;
                             exit 0),
        " Print makorel version and usage and exit.";

        "--version", Arg.Unit (fun () -> print_endline makorel_version ;
                                         exit 0),
        " Print makorel version and exit";

      ] in

  Arg.parse options (fun s -> raise (Arg.Bad ("Unknown option: " ^ s))) usage;

  let version = if !version <> "" then !version
                else failwith "missing parameter: version." in
  let root_dir = if !root_dir <> "" then !root_dir else Sys.getcwd () in
  let root_dir = Str.global_replace (Str.regexp "/$") "" root_dir in

  if !create then init root_dir version
  else upgrade root_dir version
