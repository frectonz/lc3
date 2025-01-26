let memory_max = 1 lsl 16
let pc_start = 0x3000 (* Program counter starting address. *)

let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e
;;

module ConditionFlags = struct
  type t =
    | FL_POS
    | FL_ZRO
    | FL_NEG

  let to_int = function
    | FL_POS -> 1 lsl 0
    | FL_ZRO -> 1 lsl 1
    | FL_NEG -> 1 lsl 2
  ;;

  let fl_pos = to_int FL_POS
  let fl_zro = to_int FL_ZRO
  let fl_neg = to_int FL_NEG
end

module Register = struct
  type t =
    | R_R0
    | R_R1
    | R_R2
    | R_R3
    | R_R4
    | R_R5
    | R_R6
    | R_R7
    | R_PC
    | R_COND

  let of_int = function
    | 0 -> Ok R_R0
    | 1 -> Ok R_R1
    | 2 -> Ok R_R2
    | 3 -> Ok R_R3
    | 4 -> Ok R_R4
    | 5 -> Ok R_R5
    | 6 -> Ok R_R6
    | 7 -> Ok R_R7
    | 8 -> Ok R_PC
    | 9 -> Ok R_COND
    | r -> Error (`UnkownRegister r)
  ;;
end

module Registers = struct
  type t =
    { r_r0 : int
    ; r_r1 : int
    ; r_r2 : int
    ; r_r3 : int
    ; r_r4 : int
    ; r_r5 : int
    ; r_r6 : int
    ; r_r7 : int
    ; r_pc : int (* Program Counter *)
    ; r_cond : int (* Condition Register *)
    }

  let make () =
    { r_r0 = 0
    ; r_r1 = 0
    ; r_r2 = 0
    ; r_r3 = 0
    ; r_r4 = 0
    ; r_r5 = 0
    ; r_r6 = 0
    ; r_r7 = 0
    ; (* Set program counter to the starting point. *)
      r_pc = pc_start
    ; (* Condition flag shoul alway be set. *)
      r_cond = ConditionFlags.fl_pos
    }
  ;;

  let r_pc registers = registers.r_pc
  let inc_r_pc registers = { registers with r_pc = registers.r_pc + 1 }
  let r_cond registers = registers.r_cond

  let get (index : Register.t) registers =
    match index with
    | R_R0 -> registers.r_r0
    | R_R1 -> registers.r_r1
    | R_R2 -> registers.r_r2
    | R_R3 -> registers.r_r3
    | R_R4 -> registers.r_r4
    | R_R5 -> registers.r_r5
    | R_R6 -> registers.r_r6
    | R_R7 -> registers.r_r7
    | R_PC -> registers.r_pc
    | R_COND -> registers.r_cond
  ;;

  let set (registers : t) (index : Register.t) value =
    match index with
    | R_R0 -> { registers with r_r0 = value }
    | R_R1 -> { registers with r_r1 = value }
    | R_R2 -> { registers with r_r2 = value }
    | R_R3 -> { registers with r_r3 = value }
    | R_R4 -> { registers with r_r4 = value }
    | R_R5 -> { registers with r_r5 = value }
    | R_R6 -> { registers with r_r6 = value }
    | R_R7 -> { registers with r_r7 = value }
    | R_PC -> { registers with r_pc = value }
    | R_COND -> { registers with r_cond = value }
  ;;

  let update_flags (index : Register.t) (registers : t) =
    if get index registers == 0
    then set registers R_COND ConditionFlags.fl_zro
    else if get index registers lsr 15 == 1
    then set registers R_COND ConditionFlags.fl_neg
    else set registers R_COND ConditionFlags.fl_pos
  ;;
end

module Memory = struct
  type t = int array

  let empty () = Array.make memory_max 0
  let get memory pos = Array.get memory pos

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

module OpCode = struct
  type op_br =
    { pc_offset : int
    ; cond_flag : int
    }

  type register_or_value =
    | Value of int
    | Register of Register.t

  type two_operators =
    { dr : Register.t
    ; sr1 : Register.t
    ; sr2 : register_or_value
    }

  type op_jsr = { sr : register_or_value }

  type op_ldr =
    { dr : Register.t
    ; sr : Register.t
    ; offset : int
    }

  type op_not =
    { dr : Register.t
    ; sr : Register.t
    }

  type load_register =
    { dr : Register.t
    ; pc_offset : int
    }

  type op_jmp = { dr : Register.t }

  type t =
    | OP_BR of op_br (* branch *)
    | OP_ADD of two_operators (* add *)
    | OP_LD of load_register (* load *)
    | OP_ST (* store *)
    | OP_JSR of op_jsr (* jump register *)
    | OP_AND of two_operators (* bitwise and *)
    | OP_LDR of op_ldr (* load register *)
    | OP_STR (* store register *)
    | OP_RTI (* unused *)
    | OP_NOT of op_not (* bitwise not *)
    | OP_LDI of load_register (* load indirect *)
    | OP_STI (* store indirect *)
    | OP_JMP of op_jmp (* jump *)
    | OP_RES (* reserved (unused) *)
    | OP_LEA (* load effective address *)
    | OP_TRAP (* execute trap *)

  let sign_extend x bit_count =
    if (x lsr (bit_count - 1)) land 1 = 1 then x lor (0xFFFF lsl bit_count) else x
  ;;

  let parse_two_operators instr =
    let imm_flag = (instr lsr 5) land 1 in
    let* dr = (instr lsr 9) land 0x7 |> Register.of_int in
    let* sr1 = (instr lsr 6) land 0x7 |> Register.of_int in
    if imm_flag == 1
    then (
      let v = sign_extend (instr land 0x1F) 5 in
      Ok { dr; sr1; sr2 = Value v })
    else
      let* r = instr land 0x7 |> Register.of_int in
      Ok { dr; sr1; sr2 = Register r }
  ;;

  let parse_add instr = parse_two_operators instr |> Result.map (fun x -> OP_ADD x)

  let run_add { dr; sr1; sr2 } registers =
    (Registers.get sr1 registers
     +
     match sr2 with
     | Value x -> x
     | Register r -> Registers.get r registers)
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_ldi instr =
    let* dr = (instr lsr 9) land 0x7 |> Register.of_int in
    let pc_offset = sign_extend (instr land 0x1F) 9 in
    Ok (OP_LDI { dr; pc_offset })
  ;;

  let run_ldi { dr; pc_offset } registers memory =
    Registers.r_pc registers + pc_offset
    |> Memory.get memory
    |> Memory.get memory
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_rti _ = Ok OP_RTI
  let run_rti = Error `Unused
  let parse_res _ = Ok OP_RES
  let run_res = Error `Unused
  let parse_and instr = parse_two_operators instr |> Result.map (fun x -> OP_AND x)

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
    let* dr = (instr lsr 9) land 0x7 |> Register.of_int in
    let* sr = (instr lsr 6) land 0x7 |> Register.of_int in
    Ok (OP_NOT { dr; sr })
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
    Ok (OP_BR { pc_offset; cond_flag })
  ;;

  let run_br { pc_offset; cond_flag } registers =
    let cond = cond_flag land Registers.r_cond registers in
    if cond = 1
    then Registers.r_cond registers + pc_offset |> Registers.set registers R_PC
    else registers
  ;;

  let parse_jmp instr =
    let* dr = (instr lsr 9) land 0x7 |> Register.of_int in
    Ok (OP_JMP { dr })
  ;;

  let run_jmp { dr } registers =
    Registers.get dr registers |> Registers.set registers R_PC
  ;;

  let parse_jsr instr =
    let long_flag = (instr lsr 11) land 1 in
    if long_flag == 1
    then (
      let long_pc_offset = sign_extend (instr land 0x7FF) 11 in
      Ok (OP_JSR { sr = Value long_pc_offset }))
    else
      let* r = instr land 0x7 |> Register.of_int in
      Ok (OP_JSR { sr = Register r })
  ;;

  let run_jsr ({ sr } : op_jsr) registers =
    Registers.r_pc registers
    |> Registers.set registers R_R7
    |>
    match sr with
    | Value v ->
      fun registers -> Registers.set registers R_PC (Registers.r_pc registers + v)
    | Register r ->
      fun registers -> Registers.set registers R_PC (Registers.get r registers)
  ;;

  let parse_ld instr =
    let* dr = (instr lsr 9) land 0x7 |> Register.of_int in
    let pc_offset = sign_extend (instr land 0x1FF) 9 in
    Ok (OP_LD { dr; pc_offset })
  ;;

  let run_ld { dr; pc_offset } registers memory =
    Registers.r_pc registers + pc_offset
    |> Memory.get memory
    |> Registers.set registers dr
    |> Registers.update_flags dr
  ;;

  let parse_ldr instr =
    let* dr = (instr lsr 9) land 0x7 |> Register.of_int in
    let* sr = (instr lsr 6) land 0x7 |> Register.of_int in
    let offset = sign_extend (instr land 0x3F) 6 in
    Ok (OP_LDR { dr; sr; offset })
  ;;

  let run_ldr { dr; sr; offset } registers memory =
    Registers.get sr registers + offset
    |> Memory.get memory
    |> Registers.set registers dr
    |> Registers.update_flags dr
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

  let run (program : t) = ()

  let rec evolve program =
    let registers = Registers.inc_r_pc program.registers in
    let instr = Registers.r_pc registers |> Memory.get program.memory in
    let op = instr lsr 12 in
    match op with
    | 0 -> OpCode.parse_br instr
    | 1 -> OpCode.parse_add instr
    | 2 -> OpCode.parse_ld instr
    | 4 -> OpCode.parse_jsr instr
    | 5 -> OpCode.parse_and instr
    | 6 -> OpCode.parse_ldr instr
    | 8 -> OpCode.parse_rti instr
    | 9 -> OpCode.parse_not instr
    | 10 -> OpCode.parse_ldi instr
    | 12 -> OpCode.parse_jmp instr
    | 13 -> OpCode.parse_res instr
    | x -> Error (`UnknownOp x)
  ;;
end

let run_program path =
  match Program.make path with
  | Ok image -> Program.run image
  | Error `FailedToReadImage -> print_endline "failed to load image"
;;

let run_programs paths = Array.iter run_program paths

let () =
  if Array.length Sys.argv < 1
  then print_endline "lc3 [image-file1] ..."
  else run_programs Sys.argv
;;
