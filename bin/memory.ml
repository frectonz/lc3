module Memory = struct
  type memory_type =
    | WithIoCheck
    | WithoutIoCheck

  type t = int array * memory_type

  let empty () = Array.make Constants.memory_max 0

  let read_image (image_path : string) (mem_typ : memory_type) =
    let memory = empty () in
    In_channel.with_open_bin image_path (fun ic ->
      let buf = Bytes.create 2 in
      match In_channel.really_input ic buf 0 2 with
      | None -> Error `FailedToReadImage
      | Some () ->
        let origin = Bytes.get_uint16_be buf 0 in
        let max_read = Constants.memory_max - origin in
        let buf = Bytes.create max_read in
        let read = In_channel.input ic buf 0 max_read in
        for pos = 0 to read / 2 do
          let n = Bytes.get_uint16_be buf (pos * 2) in
          memory.(origin + pos) <- n
        done;
        Ok (memory, mem_typ))
  ;;

  let check_key () =
    match Unix.select [ Unix.stdin ] [] [] 0. with
    | [], _, _ -> false
    | _ :: _, _, _ -> true
  ;;

  let read ~pos ((memory, mem_typ) : t) =
    match mem_typ with
    | WithIoCheck ->
      if pos = Constants.mr_kbsr
      then
        if check_key ()
        then (
          memory.(Constants.mr_kbsr) <- 1 lsl 15;
          memory.(Constants.mr_kbdr) <- input_char stdin |> int_of_char)
        else memory.(Constants.mr_kbsr) <- 0;
      memory.(pos)
    | WithoutIoCheck -> memory.(pos)
  ;;

  let portion ~start ~count ((memory, _) : t) =
    Array.sub memory start count |> Array.to_list
  ;;

  let write ~pos ~value ((memory, _) : t) = memory.(pos) <- value
end
