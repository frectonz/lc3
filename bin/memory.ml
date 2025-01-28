let memory_max = 1 lsl 16

module Memory = struct
  type t = int array

  let empty () = Array.make memory_max 0
  let mr_kbsr = 0xFE00
  let mr_kbdr = 0xFE02

  let check_key () =
    match Unix.select [ Unix.stdin ] [] [] 0. with
    | [], _, _ -> false
    | _ :: _, _, _ -> true
  ;;

  let get memory pos =
    if pos = mr_kbsr
    then
      if check_key ()
      then (
        memory.(mr_kbsr) <- 1 lsl 15;
        memory.(mr_kbdr) <- input_char stdin |> int_of_char)
      else memory.(mr_kbsr) <- 0;
    Array.get memory pos
  ;;

  let set memory pos value = Array.set memory pos value

  let make image_path =
    let memory = empty () in
    In_channel.with_open_bin image_path (fun ic ->
      let buf = Bytes.create 2 in
      match In_channel.really_input ic buf 0 2 with
      | None -> Error `FailedToReadImage
      | Some () ->
        let origin = Bytes.get_uint16_be buf 0 in
        let max_read = memory_max - origin in
        let buf = Bytes.create max_read in
        let read = In_channel.input ic buf 0 max_read in
        for pos = 0 to read / 2 do
          let n = Bytes.get_uint16_be buf (pos * 2) in
          memory.(origin + pos) <- n
        done;
        Ok memory)
  ;;
end
