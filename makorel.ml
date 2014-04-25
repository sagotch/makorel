open Sys
open Unix
open Str
(* Poor man's cp -R function.
 * NB: Files we are copying are very small *)

let rec cp src dst =

  let cp_file src dst = 
    let src = open_in src in
    let dst = open_out dst in
    try while true do
          output_string dst (input_line src);
          output_char dst '\n';
        done
    with End_of_file -> close_out dst;
                        close_in src in

  if is_directory src then
    (* FIXME: Assumes that there is no trailing '/' 
     * at the end of dst / src
     * FIXME: permissions? *)
    let files = Sys.readdir src |> Array.to_list in
    Unix.mkdir dst 0o775;
    List.map (fun x -> src ^ "/" ^ x, dst ^ "/" ^ x) files
    |> List.iter (fun (s, d) -> cp s d)
              
  else
    cp_file src dst 

(* Strip the path and a trailing '/' if present *)
let package_name dir =
  Str.global_replace (Str.regexp "\\(^.*/\\|^\\)\\([^/]+\\)/$") "\\2" dir

(* Strip everything before the first dot, the dot, and a trailing '/'
 * if present *)
let prev_version dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort Pervasives.compare
  |> List.rev
  |> List.hd
  |> Str.global_replace (Str.regexp "^[^\\.]*.\\([^/]+\\)/?$") "\\1"
  
let replace str x y =
  Str.global_replace (Str.regexp (Str.quote x)) y str


let _ =


  assert Array.length Sys.argv = 2 || Array.length Sys.argv =3;
  
  let root_dir = if Array.length Sys.argv = 3
                 then Sys.argv.(2)
                 else Sys.getcwd () in
  let new_ver = Sys.argv.(1) in
  let package_name = package_name root_dir in
  let prev_ver = prev_version root_dir in

(*
  print_endline (package_name);
  print_endline (prev_ver);
  print_endline (new_ver)
 *)

  let src_dir = (root_dir ^ "/" ^ package_name ^ "." ^ prev_ver) in
  let dst_dir = (root_dir ^ "/" ^ package_name ^ "." ^ new_ver) in
  
  cp src_dir dst_dir;
