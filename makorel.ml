open Sys
open Unix
open Str
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

(* main function *)
let _ =
  (* TODO: better argument support *)
  assert (Array.length Sys.argv = 2 || Array.length Sys.argv = 3);
  
  let root_dir = if Array.length Sys.argv = 3
                 then Sys.argv.(2)
                 else Sys.getcwd () in
  let new_ver = Sys.argv.(1) in
  let package_name = package_name root_dir in
  let prev_ver = prev_version root_dir in

  let src_dir = (root_dir ^ "/" ^ package_name ^ "." ^ prev_ver) in
  let dst_dir = (root_dir ^ "/" ^ package_name ^ "." ^ new_ver) in

  (* Note: copying url file is useless since we will rewrite it. *)  
  cp_r src_dir dst_dir;

  print_endline ("Copied " ^ src_dir ^ " to " ^ dst_dir);

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
         let checksum = Unixx.pipes [[|"curl"; "-s"; new_url|];
                                     [|"md5sum"|];
                                     [|"cut"; "-d\\ "; "-f"; "1"|]]
                                    Unix.stdin in
         (* assumes that everything goes well *)
         let in_channel = Unix.in_channel_of_descr checksum in
         let checksum = input_line in_channel in
         close_in in_channel;
         print_endline (checksum);
         (k, checksum)
           
      | "mirrors" -> (k, v) (* TODO: something interesting *)
      | _ -> assert false (* TODO: replace string with token when parsing
                           * in order to be pattern-matching exhaustive 
                           * and avoiding this useless case *)
    in List.map process_field url_fields in

  (* write to url file *)
  let url_file =
    open_out (root_dir ^ "/" ^ package_name ^ "." ^ new_ver ^ "/url") in
  List.iter (fun (k, v) -> output_string url_file k;
                           output_char url_file ':';
                           output_string url_file v;
                           output_char url_file '\n') processed_fields;
  close_out url_file;
  print_endline ("Version " ^ new_ver ^ " files (based on " ^ prev_ver
                 ^ ") of package " ^ package_name ^  " succesfully created.")
