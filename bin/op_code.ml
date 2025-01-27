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
    | OP_BR of op_br (* branch *)
    | OP_ADD of two_operators (* add *)
    | OP_LD of load_register (* load *)
    | OP_ST of load_register (* store *)
    | OP_JSR of op_jsr (* jump register *)
    | OP_AND of two_operators (* bitwise and *)
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

  let sign_extend x bit_count =
    if (x lsr (bit_count - 1)) land 1 = 1 then x lor (0xFFFF lsl bit_count) else x
  ;;

  let parse_two_operators instr =
    let imm_flag = (instr lsr 5) land 1 in
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr1 = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    if imm_flag == 1
    then (
      let v = sign_extend (instr land 0x1F) 5 in
      Ok { dr; sr1; sr2 = Value v })
    else
      let* r = instr land 0x7 |> Registers.register_of_int in
      Ok { dr; sr1; sr2 = Register r }
  ;;

  let parse_add instr = parse_two_operators instr |> Result.map (fun x -> OP_ADD x)

  let parse_ldi instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x1F) 9 in
    Ok (OP_LDI { dr; pc_offset })
  ;;

  let parse_rti _ = Ok OP_RTI

  let parse_res _ = Ok OP_RES

  let parse_and instr = parse_two_operators instr |> Result.map (fun x -> OP_AND x)

  let parse_not instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    Ok (OP_NOT { dr; sr })
  ;;

  let parse_br instr =
    let pc_offset = sign_extend (instr land 0x1F) 9 in
    let cond_flag = (instr lsr 9) land 0x7 in
    Ok (OP_BR { pc_offset; cond_flag })
  ;;

  let parse_jmp instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    Ok (OP_JMP { dr })
  ;;

  let parse_jsr instr =
    let long_flag = (instr lsr 11) land 1 in
    if long_flag == 1
    then (
      let long_pc_offset = sign_extend (instr land 0x7FF) 11 in
      Ok (OP_JSR { sr = Value long_pc_offset }))
    else
      let* r = instr land 0x7 |> Registers.register_of_int in
      Ok (OP_JSR { sr = Register r })
  ;;

  let parse_ld instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x1FF) 9 in
    Ok (OP_LD { dr; pc_offset })
  ;;

  let parse_ldr instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    let offset = sign_extend (instr land 0x3F) 6 in
    Ok (OP_LDR { dr; sr; offset })
  ;;

  let parse_lea instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x3F) 9 in
    Ok (OP_LEA { dr; pc_offset })
  ;;

  let parse_st instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x3F) 9 in
    Ok (OP_ST { dr; pc_offset })
  ;;

  let parse_sti instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x3F) 9 in
    Ok (OP_STI { dr; pc_offset })
  ;;

  let parse_str instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    let offset = sign_extend (instr land 0x3F) 6 in
    Ok (OP_STR { dr; sr; offset })
  ;;

  let parse_trap instr = Trap.of_int (instr land 0xFF) |> Result.map (fun t -> OP_TRAP t)
end
