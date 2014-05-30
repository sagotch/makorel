(* May be separated as a standalone library later *)

open Sys
open Unix

(* FIXME: permissions? *)
(* file copy
 * from http://ocamlunix.forge.ocamlcore.org/ocamlunix.html#sec33 *)
let rec cp_file src dst =
  let buffer_size = 8192 in
  let buffer = String.create buffer_size in
  let src = openfile src [O_RDONLY] 0 in
  let dst = openfile dst [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () = match read src buffer 0 buffer_size with
    | 0 -> ()
    | r -> ignore (write dst buffer 0 r); copy_loop ()
  in
  copy_loop ();
  close src;
  close dst

(* recursive copy *)
let rec cp_r src dst =  
  let cp_dir src dst =
    (* FIXME: Assumes that there is no trailing '/' 
     * at the end of dst / src *)
    let files = Sys.readdir src |> Array.to_list in
    Unix.mkdir dst 0o775;
    List.map (fun x -> src ^ "/" ^ x, dst ^ "/" ^ x) files
    |> List.iter (fun (s, d) -> cp_r s d)
  in
  if is_directory src
  then cp_dir src dst
  else cp_file src dst 

(* http://pleac.sourceforge.net/pleac_ocaml/processmanagementetc.html *)
let pipes (cmds : (string array) list) init_in : Unix.file_descr =

  let pipe cmd input =
    let out_reader, out_writer = Unix.pipe () in
    let pid = Unix.create_process cmd.(0) cmd input out_writer Unix.stderr in
    Unix.close out_writer;
    let _, status = Unix.waitpid [] pid in
    match status with
    | Unix.WEXITED 0 -> out_reader
    | _ -> failwith (String.concat " " (Array.to_list cmd))

  in
  (* should check for error before returning*)  
  List.fold_left (fun input cmd -> pipe cmd input) init_in cmds 

let mkdir_p path  =
  (* FIXME: absolute are parsed as relatives (first / is removed by splitting) *)
  (* FIXME: "my\/bad\/path/" will be splitted as ["my"; "bad"; "path"]
   * instead of ["my\/bad\/path"]*)
  let initial_dir = Sys.getcwd () in
  path
  |> Str.split (Str.regexp "/")
  |> List.iter
       begin
         fun dst ->
         if not (Sys.file_exists dst)
         then Unix.mkdir dst 0o775
         else if not (Sys.is_directory dst)
         then failwith (dst ^ " already exists and is a file");
         Sys.chdir dst
       end;
  Sys.chdir initial_dir
