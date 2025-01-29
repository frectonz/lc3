[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-rec-flag"]
[@@@warning "-unused-var-strict"]

open Array_ext
open Utils
open Registers

let memory_max = 1 lsl 16
let ( +^ ) x y = (x + y) land 0xffff

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

module Trap = struct
  type t =
    | TRAP_GETC
    | TRAP_OUT
    | TRAP_PUTS
    | TRAP_IN
    | TRAP_PUTSP
    | TRAP_HALT

  let to_int = function
    | TRAP_GETC -> 0x20
    | TRAP_OUT -> 0x21
    | TRAP_PUTS -> 0x22
    | TRAP_IN -> 0x23
    | TRAP_PUTSP -> 0x24
    | TRAP_HALT -> 0x25
  ;;

  let of_int = function
    | 0x20 -> Ok TRAP_GETC
    | 0x21 -> Ok TRAP_OUT
    | 0x22 -> Ok TRAP_PUTS
    | 0x23 -> Ok TRAP_IN
    | 0x24 -> Ok TRAP_PUTSP
    | 0x25 -> Ok TRAP_HALT
    | x -> Error (`UnknownTrap x)
  ;;

  let trap_getc = to_int TRAP_GETC
  let trap_out = to_int TRAP_OUT
  let trap_puts = to_int TRAP_PUTS
  let trap_in = to_int TRAP_IN
  let trap_putsp = to_int TRAP_PUTSP
  let trap_halt = to_int TRAP_HALT

  let exec_trap_getc registers =
    Registers.set registers R_R0 (input_char stdin |> int_of_char)
    |> Registers.update_flags R_R0
  ;;

  let exec_trap_puts memory registers =
    let rec aux i =
      let c = Memory.get memory (Registers.get R_R0 registers + i) in
      print_string "c ";
      print_int c;
      print_newline ();
      if c = 0
      then ()
      else (
        output_char stdout (char_of_int c);
        aux (i + 1))
    in
    aux 0;
    flush stdout
  ;;

  let exec_trap_out registers =
    Registers.get R_R0 registers |> char_of_int |> output_char stdout;
    flush stdout
  ;;

  let exec_trap_in registers =
    print_string "Enter a character: ";
    flush stdout;
    let c = input_char stdin in
    output_char stdout c;
    flush stdout;
    Registers.set registers R_R0 (int_of_char c) |> Registers.update_flags R_R0
  ;;

  let exec_trap_putsp memory registers =
    let rec aux i =
      let c = Memory.get memory (Registers.get R_R0 registers + i) in
      if c = 0
      then ()
      else (
        let char1 = c land 0xFF in
        if char1 = 0
        then ()
        else (
          output_char stdout (char_of_int char1);
          let char2 = c lsl 8 in
          if char2 = 0
          then ()
          else (
            output_char stdout (char_of_int char2);
            aux (i + 1))))
    in
    aux 0;
    flush stdout
  ;;

  let exec_trap_halt () =
    print_endline "HALT";
    flush stdout
  ;;

  let exec t r m =
    match t with
    | TRAP_HALT ->
      exec_trap_halt ();
      r
    | TRAP_GETC -> exec_trap_getc r
    | TRAP_OUT ->
      exec_trap_out r;
      r
    | TRAP_PUTS ->
      exec_trap_puts m r;
      r
    | TRAP_IN -> exec_trap_in r
    | TRAP_PUTSP ->
      exec_trap_putsp m r;
      r
  ;;
end

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

  (* type t = *)
  (* | OP_BR of op_br (* branch *) *)
  (* | OP_ADD of two_operators (* add *) *)
  (* | OP_LD of load_register (* load *) *)
  (* | OP_ST of load_register (* store *) *)
  (* | OP_JSR of op_jsr (* jump register *) *)
  (* | OP_AND of two_operators (* bitwise and *) *)
  (* | OP_LDR of op_ldr (* load register *) *)
  (* | OP_STR of op_ldr (* store register *) *)
  (* | OP_RTI (* unused *) *)
  (* | OP_NOT of op_not (* bitwise not *) *)
  (* | OP_LDI of load_register (* load indirect *) *)
  (* | OP_STI of load_register (* store indirect *) *)
  (* | OP_JMP of op_jmp (* jump *) *)
  (* | OP_RES (* unused *) *)
  (* | OP_LEA of load_register (* load effective address *) *)
  (* | OP_TRAP of Trap.t (* execute trap *) *)

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

  let parse_add instr = parse_two_operators instr

  let run_add { dr; sr1; sr2 } registers =
    (Registers.get sr1 registers
     +^
     match sr2 with
     | Value x -> x
     | Register r -> Registers.get r registers)
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_ldi instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x1FF) 9 in
    Ok { dr; pc_offset }
  ;;

  let run_ldi { dr; pc_offset } registers memory =
    Registers.r_pc registers +^ pc_offset
    |> Memory.get memory
    |> Memory.get memory
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_rti _ = Ok ()
  let run_rti = Error `Unused
  let parse_res _ = Ok ()
  let run_res = Error `Unused
  let parse_and instr = parse_two_operators instr

  let run_and { dr; sr1; sr2 } registers =
    (Registers.get sr1 registers
     land
     match sr2 with
     | Value x -> x
     | Register r -> Registers.get r registers)
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_not instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    Ok { dr; sr }
  ;;

  let run_not { dr; sr } registers =
    Registers.get sr registers
    |> lnot
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_br instr =
    let pc_offset = sign_extend (instr land 0x1F) 9 in
    let cond_flag = (instr lsr 9) land 0x7 in
    Ok { pc_offset; cond_flag }
  ;;

  let run_br { pc_offset; cond_flag } registers =
    let cond = cond_flag land Registers.r_cond registers in
    if cond <> 0 && Registers.r_cond registers <> 0
    then Registers.r_cond registers +^ pc_offset |> Registers.set registers R_PC
    else registers
  ;;

  let parse_jmp instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    Ok { dr }
  ;;

  let run_jmp { dr } registers =
    Registers.get dr registers |> Registers.set registers R_PC
  ;;

  let parse_jsr instr =
    let long_flag = (instr lsr 11) land 1 in
    if long_flag == 1
    then (
      let long_pc_offset = sign_extend (instr land 0x7FF) 11 in
      Ok { sr = Value long_pc_offset })
    else
      let* r = instr land 0x7 |> Registers.register_of_int in
      Ok { sr = Register r }
  ;;

  let run_jsr ({ sr } : op_jsr) registers =
    let registers = Registers.set registers R_R7 (Registers.r_pc registers) in
    match sr with
    | Value v -> Registers.set registers R_PC (Registers.r_pc registers - 1 +^ v)
    | Register r -> Registers.set registers R_PC (Registers.get r registers)
  ;;

  let parse_ld instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x1FF) 9 in
    Ok { dr; pc_offset }
  ;;

  let run_ld { dr; pc_offset } registers memory =
    Registers.r_pc registers +^ pc_offset
    |> Memory.get memory
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_ldr instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    let offset = sign_extend (instr land 0x3F) 6 in
    Ok { dr; sr; offset }
  ;;

  let run_ldr { dr; sr; offset } registers memory =
    Registers.get sr registers +^ offset
    |> Memory.get memory
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_lea instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x3F) 9 in
    Ok { dr; pc_offset }
  ;;

  let run_lea { dr; pc_offset } registers =
    Registers.r_pc registers +^ pc_offset
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_st instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x3F) 9 in
    Ok { dr; pc_offset }
  ;;

  let run_st { dr; pc_offset } registers memory =
    Memory.set memory (Registers.r_pc registers +^ pc_offset) (Registers.get dr registers)
  ;;

  let parse_sti instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let pc_offset = sign_extend (instr land 0x3F) 9 in
    Ok { dr; pc_offset }
  ;;

  let run_sti { dr; pc_offset } registers memory =
    Memory.set
      memory
      (Registers.r_pc registers +^ pc_offset |> Memory.get memory)
      (Registers.get dr registers)
  ;;

  let parse_str instr =
    let* dr = (instr lsr 9) land 0x7 |> Registers.register_of_int in
    let* sr = (instr lsr 6) land 0x7 |> Registers.register_of_int in
    let offset = sign_extend (instr land 0x3F) 6 in
    Ok { dr; sr; offset }
  ;;

  let run_str { dr; sr; offset } registers memory =
    Memory.set memory (Registers.get sr registers +^ offset) (Registers.get dr registers)
  ;;

  let parse_trap instr =
    print_string "trap ";
    print_int (instr land 0xFF);
    print_newline ();
    Trap.of_int (instr land 0xFF) |> Result.map (fun t -> t)
  ;;
end

module Program = struct
  type t =
    { memory : Memory.t
    ; registers : Registers.t
    }

  let make image_path =
    let* memory = Memory.make image_path in
    Ok { memory; registers = Registers.make () }
  ;;

  let rec run program =
    let registers = Registers.inc_r_pc program.registers in
    let instr = Registers.r_pc registers |> Memory.get program.memory in
    let op = instr lsr 12 in
    print_string "opcode: ";
    print_int op;
    print_newline ();
    let new_program =
      match op with
      | 0 ->
        OpCode.parse_br instr
        |> Result.map (fun x -> OpCode.run_br x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 1 ->
        OpCode.parse_add instr
        |> Result.map (fun x -> OpCode.run_add x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 2 ->
        OpCode.parse_ld instr
        |> Result.map (fun x -> OpCode.run_ld x registers program.memory)
        |> Result.map (fun r -> { program with registers = r })
      | 3 ->
        OpCode.parse_st instr
        |> Result.map (fun x ->
          OpCode.run_st x registers program.memory;
          program)
      | 4 ->
        OpCode.parse_jsr instr
        |> Result.map (fun x -> OpCode.run_jsr x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 5 ->
        OpCode.parse_and instr
        |> Result.map (fun x -> OpCode.run_and x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 6 ->
        OpCode.parse_ldr instr
        |> Result.map (fun x -> OpCode.run_ldr x registers program.memory)
        |> Result.map (fun r -> { program with registers = r })
      | 7 ->
        OpCode.parse_str instr
        |> Result.map (fun x ->
          OpCode.run_str x registers program.memory;
          program)
      | 8 -> OpCode.run_rti
      | 9 ->
        OpCode.parse_not instr
        |> Result.map (fun x -> OpCode.run_not x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 10 ->
        OpCode.parse_ldi instr
        |> Result.map (fun x -> OpCode.run_ldi x registers program.memory)
        |> Result.map (fun r -> { program with registers = r })
      | 11 ->
        OpCode.parse_sti instr
        |> Result.map (fun x ->
          OpCode.run_sti x registers program.memory;
          program)
      | 12 ->
        OpCode.parse_jmp instr
        |> Result.map (fun x -> OpCode.run_jmp x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 13 -> OpCode.run_res
      | 14 ->
        OpCode.parse_lea instr
        |> Result.map (fun x -> OpCode.run_lea x registers)
        |> Result.map (fun r -> { program with registers = r })
      | 15 ->
        OpCode.parse_trap instr
        |> Result.map (fun t -> Trap.exec t registers program.memory)
        |> Result.map (fun r -> { program with registers = r })
      | x -> Error (`UnknownOp x)
    in
    match new_program with
    | Ok np -> run np
    | Error _ -> exit 0
  ;;
end

let run_program path =
  match Program.make path with
  | Ok image -> Program.run image
  | Error `FailedToReadImage -> print_endline "failed to load image"
;;

let run_programs paths = Array.iter run_program paths

let () =
  if Array.length Sys.argv >= 2
  then Sys.argv |> Array.remove_first |> run_programs
  else print_endline "lc3 [image-file1] ..."
;;
