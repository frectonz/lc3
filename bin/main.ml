open Program
open Array_ext

let original_tio = Unix.tcgetattr Unix.stdin

let disable_input_buffering () =
  let open Unix in
  let new_tio = { original_tio with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSANOW new_tio
;;

let restore_input_buffering () =
  let open Unix in
  tcsetattr stdin TCSANOW original_tio
;;

let handle_interrupt _ =
  restore_input_buffering ();
  print_newline ();
  exit (-2)
;;

let setup () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt);
  disable_input_buffering ()
;;

let run_program path =
  match Program.read path with
  | Ok image ->
    setup ();
    Program.run image
  | Error `FailedToReadImage -> print_endline "failed to load image"
;;

let run_programs paths = Array.iter run_program paths

let () =
  if Array.length Sys.argv >= 2
  then Sys.argv |> Array.remove_first |> run_programs
  else print_endline "lc3 [image-file1] ..."
;;
