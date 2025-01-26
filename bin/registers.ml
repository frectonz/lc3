open Register
open Condition_flags

let pc_start = 0x3000 (* Program counter starting address. *)

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
