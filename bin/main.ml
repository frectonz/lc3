open Program

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

let action_handler app = function
  | `Arrow `Down, _ ->
    app |> Lwd.peek |> Program.step |> Lwd.set app;
    `Handled
  | `ASCII ch, _ ->
    app |> Lwd.peek |> Program.add_key ch |> Lwd.set app;
    `Handled
  | _ -> `Unhandled
;;

let explore_program path =
  match Program.read path with
  | Ok image ->
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let image = Lwd.var image in
    let ui =
      image
      |> Lwd.get
      |> Lwd.map ~f:Program.render
      |> Lwd.join
      |> Lwd.map ~f:(Ui.keyboard_area (action_handler image))
    in
    Ui_loop.run ui
  | Error _ -> print_endline "failed to load image"
;;

let run_program path =
  match Program.read path with
  | Ok image ->
    setup ();
    Program.run image
  | Error _ -> print_endline "failed to load image"
;;

let () =
  if Array.length Sys.argv >= 3
  then
    if Sys.argv.(1) = "explore"
    then explore_program Sys.argv.(2)
    else if Sys.argv.(1) = "run"
    then run_program Sys.argv.(2)
    else ()
  else print_endline "lc3 explore [image-file]\nlc3 run [image-file]"
;;
