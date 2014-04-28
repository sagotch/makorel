open Digest
open Str
open Sys
open Unix

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

let version = "0.1.1"

let new_ver = ref ""
let root_dir = ref ""

let usage = "usage: makorel [options] -v package_version [options]"

let options =
  Arg.align [
      "-v", Arg.Set_string new_ver,
      "version Package version to create.";

      "-p", Arg.Set_string root_dir,
      "package Package root directory.";

      "--about", Arg.Unit (fun () ->
                           print_endline ("makorel version: " ^ version);
                           print_endline usage;
                           exit 0),
      " Print makorel version and usage and exit.";
    ]

(* main function *)
let _ =

  Arg.parse options (fun s -> raise (Arg.Bad ("Unknown option: " ^ s))) usage;
  
  let new_ver = if !new_ver <> "" then !new_ver
                else failwith "missing parameter: version." in
  let root_dir = if !root_dir <> "" then !root_dir else Sys.getcwd () in
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
         let curl_cmd = "curl -Lsf " ^ new_url in
         let curl_out = open_process_in curl_cmd in
         let checksum = Digest.channel curl_out (-1) |> Digest.to_hex in
         if Unix.close_process_in curl_out <> Unix.WEXITED 0
         then failwith curl_cmd
         else (k, checksum)

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
