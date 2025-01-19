open Stdint

let memory_max = 1 lsl 16
let pc_start = Uint16.of_int 0x3000 (* Program counter starting address. *)
let ( let* ) x f = match x with Ok v -> f v | Error e -> Error e

module ConditionFlags = struct
  type t = FL_POS | FL_ZRO | FL_NEG

  let to_int = function
    | FL_POS -> 1 lsl 0 |> Uint16.of_int
    | FL_ZRO -> 1 lsl 1 |> Uint16.of_int
    | FL_NEG -> 1 lsl 2 |> Uint16.of_int

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
end

module Registers = struct
  type t = {
    r_r0 : uint16;
    r_r1 : uint16;
    r_r2 : uint16;
    r_r3 : uint16;
    r_r4 : uint16;
    r_r5 : uint16;
    r_r6 : uint16;
    r_r7 : uint16;
    r_pc : uint16; (* Program Counter *)
    r_cond : uint16; (* Condition Register *)
  }

  let make () =
    {
      r_r0 = Uint16.zero;
      r_r1 = Uint16.zero;
      r_r2 = Uint16.zero;
      r_r3 = Uint16.zero;
      r_r4 = Uint16.zero;
      r_r5 = Uint16.zero;
      r_r6 = Uint16.zero;
      r_r7 = Uint16.zero;
      (* Set program counter to the starting point. *)
      r_pc = pc_start;
      (* Condition flag shoul alway be set. *)
      r_cond = ConditionFlags.fl_pos;
    }

  let r_pc registers = registers.r_pc

  let inc_r_pc registers =
    { registers with r_pc = Uint16.(registers.r_pc + one) }
end

module Memory = struct
  type t = uint16 array

  let empty () = Array.make memory_max Uint16.zero
  let get memory pos = Array.get memory (Uint16.to_int pos)

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
              memory.(origin + pos) <- n |> Uint16.of_int
            done;
            Ok memory)
end

module Program = struct
  type t = { memory : Memory.t; registers : Registers.t }

  let make image_path =
    let* memory = Memory.make image_path in
    Ok { memory; registers = Registers.make () }

  let run (program : t) = ()

  let rec evolve program =
    let registers = Registers.inc_r_pc program.registers in
    let op = Registers.r_pc registers |> Memory.get program.memory in
    ()
end

module OpCode = struct
  type register_or_value = Value of uint16 | Register of Register.t

  type t =
    | OP_BR (* branch *)
    (* add *)
    | OP_ADD of { dr : Register.t; sr1 : Register.t; sr2 : register_or_value }
    | OP_LD (* load *)
    | OP_ST (* store *)
    | OP_JSR (* jump register *)
    | OP_AND (* bitwise and *)
    | OP_LDR (* load register *)
    | OP_STR (* store register *)
    | OP_RTI (* unused *)
    | OP_NOT (* bitwise not *)
    | OP_LDI (* load indirect *)
    | OP_STI (* store indirect *)
    | OP_JMP (* jump *)
    | OP_RES (* reserved (unused) *)
    | OP_LEA (* load effective address *)
    | OP_TRAP (* execute trap *)
end

let run_program path =
  match Program.make path with
  | Ok image -> Program.run image
  | Error `FailedToReadImage -> print_endline "failed to load image"

let run_programs paths = Array.iter run_program paths

let () =
  if Array.length Sys.argv < 1 then print_endline "lc3 [image-file1] ..."
  else run_programs Sys.argv
