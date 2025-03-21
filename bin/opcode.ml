open Trap
open Utils
open Registers

module OpCode = struct
  type op_br =
    { pc_offset : int
    ; cond_flag : int
    }

  type register_or_value =
    | Value of int
    | Register of Registers.register

  type two_operators =
    { dr : Registers.register
    ; sr1 : Registers.register
    ; sr2 : register_or_value
    }

  type op_jsr = { sr : register_or_value }

  type op_ldr =
    { dr : Registers.register
    ; sr : Registers.register
    ; offset : int
    }

  type op_not =
    { dr : Registers.register
    ; sr : Registers.register
    }

  type load_register =
    { dr : Registers.register
    ; pc_offset : int
    }

  type op_jmp = { dr : Registers.register }

  type t =
    | OP_ADD of two_operators (* add *)
    | OP_AND of two_operators (* bitwise and *)
    | OP_BR of op_br (* branch *)
    | OP_LD of load_register (* load *)
    | OP_ST of load_register (* store *)
    | OP_JSR of op_jsr (* jump register *)
    | OP_LDR of op_ldr (* load register *)
    | OP_STR of op_ldr (* store register *)
    | OP_RTI (* unused *)
    | OP_NOT of op_not (* bitwise not *)
    | OP_LDI of load_register (* load indirect *)
    | OP_STI of load_register (* store indirect *)
    | OP_JMP of op_jmp (* jump *)
    | OP_RES (* unused *)
    | OP_LEA of load_register (* load effective address *)
    | OP_TRAP of Trap.t (* execute trap *)

  let parse_two_operators instr =
    let imm_flag = bits instr ~pos:5 ~width:1 in
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let* sr1 = bits instr ~pos:6 ~width:3 |> Registers.register_of_int in
    if imm_flag = 0
    then
      let* r = bits instr ~width:3 |> Registers.register_of_int in
      Ok { dr; sr1; sr2 = Register r }
    else Ok { dr; sr1; sr2 = Value (signed_bits instr ~width:5) }
  ;;

  let parse_add instr = parse_two_operators instr |> Result.map (fun x -> OP_ADD x)
  let parse_and instr = parse_two_operators instr |> Result.map (fun x -> OP_AND x)

  let parse_not instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let* sr = bits instr ~pos:6 ~width:3 |> Registers.register_of_int in
    Ok (OP_NOT { dr; sr })
  ;;

  let parse_br instr =
    let pc_offset = signed_bits instr ~width:9 in
    let cond_flag = bits instr ~pos:9 ~width:3 in
    Ok (OP_BR { pc_offset; cond_flag })
  ;;

  let parse_jmp instr =
    let* dr = bits instr ~pos:6 ~width:3 |> Registers.register_of_int in
    Ok (OP_JMP { dr })
  ;;

  let parse_jsr instr =
    let long_flag = bits instr ~pos:11 ~width:1 in
    if long_flag = 0
    then
      let* r = bits instr ~pos:6 ~width:3 |> Registers.register_of_int in
      Ok (OP_JSR { sr = Register r })
    else (
      let long_pc_offset = signed_bits instr ~width:11 in
      Ok (OP_JSR { sr = Value long_pc_offset }))
  ;;

  let parse_ld instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let pc_offset = signed_bits instr ~width:9 in
    Ok (OP_LD { dr; pc_offset })
  ;;

  let parse_ldi instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let pc_offset = signed_bits instr ~width:9 in
    Ok (OP_LDI { dr; pc_offset })
  ;;

  let parse_ldr instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let* sr = bits instr ~pos:6 ~width:3 |> Registers.register_of_int in
    let offset = signed_bits instr ~width:6 in
    Ok (OP_LDR { dr; sr; offset })
  ;;

  let parse_lea instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let pc_offset = signed_bits instr ~width:9 in
    Ok (OP_LEA { dr; pc_offset })
  ;;

  let parse_st instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let pc_offset = signed_bits instr ~width:9 in
    Ok (OP_ST { dr; pc_offset })
  ;;

  let parse_sti instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let pc_offset = signed_bits instr ~width:9 in
    Ok (OP_STI { dr; pc_offset })
  ;;

  let parse_str instr =
    let* dr = bits instr ~pos:9 ~width:3 |> Registers.register_of_int in
    let* sr = bits instr ~pos:6 ~width:3 |> Registers.register_of_int in
    let offset = signed_bits instr ~width:6 in
    Ok (OP_STR { dr; sr; offset })
  ;;

  let parse_rti _ = Ok OP_RTI
  let parse_res _ = Ok OP_RES

  let parse_trap instr =
    bits instr ~width:8 |> Trap.of_int |> Result.map (fun x -> OP_TRAP x)
  ;;

  let parse instr =
    let op = bits instr ~pos:12 ~width:4 in
    match op with
    | 0 -> parse_br instr
    | 1 -> parse_add instr
    | 2 -> parse_ld instr
    | 3 -> parse_st instr
    | 4 -> parse_jsr instr
    | 5 -> parse_and instr
    | 6 -> parse_ldr instr
    | 7 -> parse_str instr
    | 8 -> parse_rti instr
    | 9 -> parse_not instr
    | 10 -> parse_ldi instr
    | 11 -> parse_sti instr
    | 12 -> parse_jmp instr
    | 13 -> parse_res instr
    | 14 -> parse_lea instr
    | 15 -> parse_trap instr
    | x -> Error (`UnknownOp x)
  ;;

  let render_register (r : Registers.register) =
    match r with
    | R_R0 -> "R0"
    | R_R1 -> "R1"
    | R_R2 -> "R2"
    | R_R3 -> "R3"
    | R_R4 -> "R4"
    | R_R5 -> "R5"
    | R_R6 -> "R6"
    | R_R7 -> "R7"
  ;;

  let render_register_or_value = function
    | Value v -> Printf.sprintf "0x%04x" v
    | Register r -> render_register r
  ;;

  let to_string (op : t) =
    let str =
      match op with
      | OP_ADD { dr; sr1; sr2 } ->
        Printf.sprintf
          "ADD %s %s %s"
          (render_register dr)
          (render_register sr1)
          (render_register_or_value sr2)
      | OP_AND { dr; sr1; sr2 } ->
        Printf.sprintf
          "AND %s %s %s"
          (render_register dr)
          (render_register sr1)
          (render_register_or_value sr2)
      | OP_BR { pc_offset; cond_flag } ->
        let n = if cond_flag land 4 != 0 then "n" else "" in
        let z = if cond_flag land 2 != 0 then "z" else "" in
        let p = if cond_flag land 1 != 0 then "p" else "" in
        Printf.sprintf "BR%s%s%s 0x%04x" n z p pc_offset
      | OP_LD { dr; pc_offset } ->
        Printf.sprintf "LD %s 0x%04x" (render_register dr) pc_offset
      | OP_ST { dr; pc_offset } ->
        Printf.sprintf "ST %s 0x%04x" (render_register dr) pc_offset
      | OP_JSR { sr } -> Printf.sprintf "JSR %s" (render_register_or_value sr)
      | OP_LDR { dr; sr; offset } ->
        Printf.sprintf "LDR %s %s 0x%04x" (render_register dr) (render_register sr) offset
      | OP_STR { dr; sr; offset } ->
        Printf.sprintf "STR %s %s 0x%04x" (render_register dr) (render_register sr) offset
      | OP_RTI -> "RTI"
      | OP_NOT { dr; sr } ->
        Printf.sprintf "NOT %s %s" (render_register dr) (render_register sr)
      | OP_LDI { dr; pc_offset } ->
        Printf.sprintf "LDI %s 0x%04x" (render_register dr) pc_offset
      | OP_STI { dr; pc_offset } ->
        Printf.sprintf "STI %s 0x%04x" (render_register dr) pc_offset
      | OP_JMP { dr } -> Printf.sprintf "JMP %s" (render_register dr)
      | OP_RES -> "RES"
      | OP_LEA { dr; pc_offset } ->
        Printf.sprintf "LEA %s 0x%04x" (render_register dr) pc_offset
      | OP_TRAP trap -> Printf.sprintf "TRAP %s" (Trap.to_string trap)
    in
    str ^ "\n"
  ;;

  let render (op : t) =
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let op_str = to_string op in
    W.string ~attr:A.(fg yellow) op_str |> Lwd.return
  ;;
end
