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

  type register =
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

  let register_of_int = function
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

  let inc_r_pc registers = { registers with r_pc = registers.r_pc + 1 }

  let get index registers =
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

  let set registers index value =
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

  let update_flags index registers =
    if get index registers == 0
    then set registers R_COND ConditionFlags.fl_zro
    else if get index registers lsr 15 == 1
    then set registers R_COND ConditionFlags.fl_neg
    else set registers R_COND ConditionFlags.fl_pos
  ;;

  let r_r0 = get R_R0
  let r_r1 = get R_R1
  let r_r2 = get R_R2
  let r_r3 = get R_R3
  let r_r4 = get R_R4
  let r_r5 = get R_R5
  let r_r6 = get R_R6
  let r_r7 = get R_R7
  let r_pc = get R_PC
  let r_cond = get R_COND
end
