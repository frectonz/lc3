open Trap
open Utils
open Opcode
open Memory
open Registers

module Program = struct
  type t =
    { memory : Memory.t
    ; registers : Registers.t
    ; cond : int
    ; mutable pc : int
    ; mutable running : bool
    }

  let read image_path =
    let* memory = Memory.read_image image_path in
    Ok
      { memory
      ; registers = Registers.make ()
      ; cond = Constants.fl_zro
      ; pc = 0x3000
      ; running = true
      }
  ;;

  let update_flags r ({ registers; _ } as program : t) =
    let v = Registers.get r registers in
    let fl =
      if v = 0
      then Constants.fl_zro
      else if bits v ~pos:15 ~width:1 <> 0
      then
        (* A 1 in the left-most bit indicates negative. *)
        Constants.fl_neg
      else Constants.fl_pos
    in
    { program with cond = fl }
  ;;

  let run_add ({ dr; sr1; sr2 } : OpCode.two_operators) ({ registers; _ } as program : t) =
    let sum =
      Registers.get sr1 registers
      +^
      match sr2 with
      | Value x -> x
      | Register r -> Registers.get r registers
    in
    { program with registers = Registers.set ~index:dr ~value:sum registers }
    |> update_flags dr
  ;;

  let run_and ({ dr; sr1; sr2 } : OpCode.two_operators) ({ registers; _ } as program : t) =
    let value =
      Registers.get sr1 registers
      land
      match sr2 with
      | Value x -> x
      | Register r -> Registers.get r registers
    in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_not ({ dr; sr } : OpCode.op_not) ({ registers; _ } as program : t) =
    let sr = Registers.get sr registers in
    let value = lnot sr land 0xffff in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_br ({ pc_offset; cond_flag } : OpCode.op_br) ({ cond; pc; _ } as program : t) =
    if cond_flag land cond <> 0 then { program with pc = pc +^ pc_offset } else program
  ;;

  let run_jmp ({ dr } : OpCode.op_jmp) ({ registers; _ } as program : t) =
    { program with pc = Registers.get dr registers }
  ;;

  let run_jsr ({ sr } : OpCode.op_jsr) ({ pc; registers; _ } as program : t) =
    let dest =
      match sr with
      | Value offset -> pc +^ offset
      | Register r -> Registers.get r registers
    in
    { program with pc = dest; registers = Registers.set_r_r7 pc registers }
  ;;

  let run_ld
        ({ dr; pc_offset } : OpCode.load_register)
        ({ pc; registers; memory; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    let value = Memory.read ~pos memory in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_ldi
        ({ dr; pc_offset } : OpCode.load_register)
        ({ pc; registers; memory; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    let value = Memory.read ~pos memory in
    let value = Memory.read ~pos:value memory in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_ldr
        ({ dr; sr; offset } : OpCode.op_ldr)
        ({ registers; memory; _ } as program : t)
    =
    let r = Registers.get sr registers in
    let pos = r +^ offset in
    let value = Memory.read ~pos memory in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_lea
        ({ dr; pc_offset } : OpCode.load_register)
        ({ pc; registers; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    { program with registers = Registers.set ~index:dr ~value:pos registers }
    |> update_flags dr
  ;;

  let run_st
        ({ dr; pc_offset } : OpCode.load_register)
        ({ pc; registers; memory; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    let value = Registers.get dr registers in
    Memory.write ~pos ~value memory;
    program
  ;;

  let run_sti
        ({ dr; pc_offset } : OpCode.load_register)
        ({ pc; registers; memory; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    let pos = Memory.read ~pos memory in
    let value = Registers.get dr registers in
    Memory.write ~pos ~value memory;
    program
  ;;

  let run_str
        ({ dr; sr; offset } : OpCode.op_ldr)
        ({ registers; memory; _ } as program : t)
    =
    let r = Registers.get sr registers in
    let pos = r +^ offset in
    let value = Registers.get dr registers in
    Memory.write ~pos ~value memory;
    program
  ;;

  let run_rti (_ : t) : (t, _) result = Error `Unused
  let run_res (_ : t) : (t, _) result = Error `Unused

  let exec_trap_getc ({ registers; _ } as program : t) =
    let registers' =
      Registers.set ~index:R_R0 ~value:(input_char stdin |> int_of_char) registers
    in
    { program with registers = registers' } |> update_flags R_R0
  ;;

  let exec_trap_out ({ registers; _ } as program : t) =
    Registers.r_r0 registers |> char_of_int |> output_char stdout;
    flush stdout;
    program
  ;;

  let exec_trap_puts ({ registers; memory; _ } as program : t) =
    let rec aux i =
      let c = Memory.read ~pos:(Registers.r_r0 registers + i) memory in
      if c = 0
      then ()
      else (
        output_char stdout (char_of_int c);
        aux (i + 1))
    in
    aux 0;
    flush stdout;
    program
  ;;

  let exec_trap_in ({ registers; _ } as program : t) =
    print_string "Enter a character: ";
    flush stdout;
    let c = input_char stdin in
    output_char stdout c;
    flush stdout;
    let registers' = Registers.set ~index:R_R0 ~value:(int_of_char c) registers in
    { program with registers = registers' } |> update_flags R_R0
  ;;

  let exec_trap_putsp ({ registers; memory; _ } as program : t) =
    let rec aux i =
      let c = Memory.read ~pos:(Registers.get R_R0 registers + i) memory in
      if c = 0
      then ()
      else (
        let char1 = bits c ~width:8 in
        if char1 = 0
        then ()
        else (
          output_char stdout (char_of_int char1);
          let char2 = bits c ~pos:8 ~width:8 in
          if char2 = 0
          then ()
          else (
            output_char stdout (char_of_int char2);
            aux (i + 1))))
    in
    aux 0;
    flush stdout;
    program
  ;;

  let exec_trap_halt (program : t) =
    print_endline "HALT";
    flush stdout;
    program.running <- false;
    program
  ;;

  let run_opcode (op : OpCode.t) (program : t) =
    match op with
    | OpCode.OP_ADD x -> Ok (run_add x program)
    | OpCode.OP_RTI -> run_rti program
    | OpCode.OP_RES -> run_res program
    | OpCode.OP_AND x -> Ok (run_and x program)
    | OpCode.OP_BR x -> Ok (run_br x program)
    | OpCode.OP_LD x -> Ok (run_ld x program)
    | OpCode.OP_ST x -> Ok (run_st x program)
    | OpCode.OP_JSR x -> Ok (run_jsr x program)
    | OpCode.OP_LDR x -> Ok (run_ldr x program)
    | OpCode.OP_STR x -> Ok (run_str x program)
    | OpCode.OP_NOT x -> Ok (run_not x program)
    | OpCode.OP_LDI x -> Ok (run_ldi x program)
    | OpCode.OP_STI x -> Ok (run_sti x program)
    | OpCode.OP_JMP x -> Ok (run_jmp x program)
    | OpCode.OP_LEA x -> Ok (run_lea x program)
    | OpCode.OP_TRAP Trap.TRAP_GETC -> Ok (exec_trap_getc program)
    | OpCode.OP_TRAP Trap.TRAP_OUT -> Ok (exec_trap_out program)
    | OpCode.OP_TRAP Trap.TRAP_PUTS -> Ok (exec_trap_puts program)
    | OpCode.OP_TRAP Trap.TRAP_IN -> Ok (exec_trap_in program)
    | OpCode.OP_TRAP Trap.TRAP_PUTSP -> Ok (exec_trap_putsp program)
    | OpCode.OP_TRAP Trap.TRAP_HALT -> Ok (exec_trap_halt program)
  ;;

  let step prog =
    if not prog.running
    then prog
    else (
      let pos = prog.pc in
      prog.pc <- pos + 1;
      let instr = Memory.read ~pos prog.memory in
      let prog =
        OpCode.parse instr |> Result.map (fun op -> run_opcode op prog) |> Result.join
      in
      match prog with
      | Ok prog -> prog
      | Error `Unused -> failwith "unused"
      | Error (`UnknownOp _) -> failwith "unknown op"
      | Error (`UnknownTrap _) -> failwith "unkown trap"
      | Error (`UnkownRegister _) -> failwith "unkown register")
  ;;

  let run (program : t) =
    let rec aux prog =
      if not prog.running
      then ()
      else (
        let pos = prog.pc in
        prog.pc <- pos + 1;
        let instr = Memory.read ~pos prog.memory in
        let prog =
          OpCode.parse instr |> Result.map (fun op -> run_opcode op prog) |> Result.join
        in
        match prog with
        | Ok prog -> aux prog
        | Error `Unused -> failwith "unused"
        | Error (`UnknownOp _) -> failwith "unknown op"
        | Error (`UnknownTrap _) -> failwith "unkown trap"
        | Error (`UnkownRegister _) -> failwith "unkown register")
    in
    aux program
  ;;

  let render (program : t) =
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    (* Render registers panel *)
    let registers = Registers.render program.registers in
    (* Render current instruction panel *)
    let current_op =
      let current_instr = Memory.read ~pos:program.pc program.memory in
      match OpCode.parse current_instr with
      | Ok op ->
        let op_str = OpCode.render op in
        W.vbox
          [ Utils.header "CURRENT INSTRUCTION"
          ; Ui.space 0 1 |> Lwd.return
          ; W.hbox
              [ W.string ~attr:A.(fg (gray 5)) "0x" |> Lwd.return
              ; W.string ~attr:A.(fg white) (Printf.sprintf "%04x" program.pc)
                |> Lwd.return
              ; W.string ~attr:A.(fg (gray 5)) " → " |> Lwd.return
              ; op_str
              ]
          ]
      | Error _ ->
        W.vbox
          [ Utils.header "CURRENT INSTRUCTION"
          ; Ui.space 0 1 |> Lwd.return
          ; W.string
              ~attr:A.(fg red)
              (Printf.sprintf "Invalid instruction: 0x%04x" current_instr)
            |> Lwd.return
          ]
    in
    (* Render output buffer panel *)
    let output_panel =
      W.vbox
        (Utils.header "PROGRAM OUTPUT"
         :: (Ui.space 0 1 |> Lwd.return)
         :: List.map (fun line -> W.string line |> Lwd.return) [ "" ])
    in
    W.v_pane (W.h_pane registers current_op) output_panel
  ;;
end
