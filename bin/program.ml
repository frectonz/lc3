open Trap
open Utils
open Opcode
open Memory
open Registers

module Program = struct
  type input_state =
    | NotAsked
    | InputingChar of char option

  type t =
    { memory : Memory.t
    ; registers : Registers.t
    ; cond : int
    ; pc : int
    ; running : bool
    ; output_buffer : string
    ; input_state : input_state
    ; escape_sequence_state : int
    }

  let read image_path mem_typ =
    let* memory = Memory.read_image image_path mem_typ in
    Ok
      { memory
      ; registers = Registers.make ()
      ; cond = Constants.fl_zro
      ; pc = 0x3000
      ; running = true
      ; output_buffer = ""
      ; input_state = NotAsked
      ; escape_sequence_state = 0
      }
  ;;

  let unwrap_format mem acc pc prog =
    match prog with
    | Ok prog -> prog
    | Error (`UnknownOp _) -> mem, acc ^ "unknown op\n", pc + 1
    | Error (`UnknownTrap _) -> mem, acc ^ "unkown trap\n", pc + 1
    | Error (`UnkownRegister _) -> mem, acc ^ "unkown register\n", pc + 1
  ;;

  let to_string ({ memory; _ } : t) =
    let rec aux (mem, acc, pc) =
      if pc + 1 == 65536
      then mem, acc, pc
      else (
        let opcode = Memory.read ~pos:pc mem in
        if opcode == 0
        then aux (mem, acc, pc + 1)
        else
          OpCode.parse opcode
          |> Result.map (fun op -> mem, acc ^ OpCode.to_string op, pc + 1)
          |> unwrap_format mem acc pc
          |> aux)
    in
    aux (memory, "", 0) |> fun (_, acc, _) -> acc
  ;;

  let update_flags r ({ registers; _ } as program : t) =
    let v = Registers.get r registers in
    let fl =
      if v = 0
      then Constants.fl_zro
      else if bits v ~pos:15 ~width:1 <> 0
      then Constants.fl_neg
      else Constants.fl_pos
    in
    { program with cond = fl }
  ;;

  let read_memory ~pos (program : t) =
    match snd program.memory with
    | Memory.WithIoCheck -> `Int (Memory.read ~pos program.memory)
    | Memory.WithoutIoCheck ->
      if pos = Constants.mr_kbsr
      then (
        match program.input_state with
        | NotAsked ->
          `Program { program with input_state = InputingChar None; pc = program.pc - 1 }
        | InputingChar (Some c) ->
          Memory.write ~pos:Constants.mr_kbsr ~value:(1 lsl 15) program.memory;
          Memory.write ~pos:Constants.mr_kbdr ~value:(int_of_char c) program.memory;
          `Int (Memory.read ~pos program.memory)
        | InputingChar None -> `Program program)
      else `Int (Memory.read ~pos program.memory)
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
        ({ pc; registers; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    let@ value = read_memory ~pos program in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_ldi
        ({ dr; pc_offset } : OpCode.load_register)
        ({ pc; registers; _ } as program : t)
    =
    let pos = pc +^ pc_offset in
    let@ value = read_memory ~pos program in
    let@ value = read_memory ~pos:value program in
    { program with registers = Registers.set ~index:dr ~value registers }
    |> update_flags dr
  ;;

  let run_ldr ({ dr; sr; offset } : OpCode.op_ldr) ({ registers; _ } as program : t) =
    let r = Registers.get sr registers in
    let pos = r +^ offset in
    let@ value = read_memory ~pos program in
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
    let@ pos = read_memory ~pos program in
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

  let add_key (ch : char) ({ input_state; _ } as program : t) =
    let input_state' =
      match input_state with
      | NotAsked -> NotAsked
      | InputingChar _ -> InputingChar (Some ch)
    in
    { program with input_state = input_state' }
  ;;

  let update_output_buffer ch (program : t) =
    let new_state =
      match program.escape_sequence_state with
      | 0 -> if ch = '\x1B' then 1 else 0
      | 1 -> if ch = '[' then 2 else 0
      | 2 -> if ch = '2' then 3 else 0
      | 3 -> if ch = 'J' then 4 else 0
      | _ -> 0
    in
    if new_state = 4
    then { program with output_buffer = ""; escape_sequence_state = 0 }
    else
      { program with
        output_buffer = program.output_buffer ^ String.make 1 ch
      ; escape_sequence_state = new_state
      }
  ;;

  let exec_trap_getc (program : t) =
    match program.input_state with
    | NotAsked -> { program with input_state = InputingChar None; pc = program.pc - 1 }
    | InputingChar (Some c) ->
      let registers' = Registers.set_r_r0 (int_of_char c) program.registers in
      { program with registers = registers'; input_state = NotAsked } |> update_flags R_R0
    | InputingChar None -> program
  ;;

  let exec_trap_getc_unix (program : t) =
    let registers' =
      Registers.set_r_r0 (input_char stdin |> int_of_char) program.registers
    in
    { program with registers = registers' } |> update_flags R_R0
  ;;

  let exec_trap_in (program : t) =
    match program.input_state with
    | NotAsked ->
      { program with
        output_buffer = program.output_buffer ^ "Enter a character: "
      ; input_state = InputingChar None
      ; pc = program.pc - 1
      }
    | InputingChar (Some c) ->
      { program with
        registers = Registers.set_r_r0 (int_of_char c) program.registers
      ; input_state = NotAsked
      }
      |> update_output_buffer c
      |> update_flags R_R0
    | InputingChar None -> program
  ;;

  let exec_trap_in_unix (program : t) =
    print_string "Enter a character: ";
    flush stdout;
    let c = input_char stdin in
    output_char stdout c;
    flush stdout;
    { program with registers = Registers.set_r_r0 (int_of_char c) program.registers }
    |> update_output_buffer c
    |> update_flags R_R0
  ;;

  let exec_trap_out ({ registers; _ } as program : t) =
    let char = Registers.r_r0 registers |> char_of_int in
    update_output_buffer char program
  ;;

  let exec_trap_out_unix ({ registers; _ } as program : t) =
    let char = Registers.r_r0 registers |> char_of_int in
    output_char stdout char;
    flush stdout;
    program
  ;;

  let exec_trap_puts (program : t) =
    let rec aux i prog =
      let@ c = read_memory ~pos:(Registers.r_r0 prog.registers + i) prog in
      if c = 0 then prog else aux (i + 1) (update_output_buffer (char_of_int c) prog)
    in
    aux 0 program
  ;;

  let exec_trap_puts_unix (program : t) =
    let rec aux i prog =
      let@ c = read_memory ~pos:(Registers.r_r0 prog.registers + i) prog in
      if c = 0
      then prog
      else (
        output_char stdout (char_of_int c);
        aux (i + 1) prog)
    in
    let prog = aux 0 program in
    flush stdout;
    prog
  ;;

  let exec_trap_putsp (program : t) =
    let rec aux i prog =
      let@ c = read_memory ~pos:(Registers.r_r0 prog.registers + i) prog in
      if c = 0
      then prog
      else (
        let char1 = bits c ~width:8 in
        if char1 = 0
        then prog
        else (
          let prog = update_output_buffer (char_of_int char1) prog in
          let char2 = bits c ~pos:8 ~width:8 in
          if char2 = 0
          then prog
          else (
            let prog = update_output_buffer (char_of_int char2) prog in
            aux (i + 1) prog)))
    in
    aux 0 program
  ;;

  let exec_trap_putsp_unix (program : t) =
    let rec aux i prog =
      let@ c = read_memory ~pos:(Registers.r_r0 prog.registers + i) prog in
      if c = 0
      then prog
      else (
        let char1 = bits c ~width:8 in
        if char1 = 0
        then prog
        else (
          output_char stdout (char_of_int char1);
          let char2 = bits c ~pos:8 ~width:8 in
          if char2 = 0
          then prog
          else (
            output_char stdout (char_of_int char1);
            aux (i + 1) prog)))
    in
    let prog = aux 0 program in
    flush stdout;
    prog
  ;;

  let exec_trap_halt (program : t) =
    { program with running = false; output_buffer = program.output_buffer ^ "\n\nHALT\n" }
  ;;

  let exec_trap_halt_unix (program : t) =
    print_newline ();
    print_endline "HALT";
    flush stdout;
    { program with running = false }
  ;;

  let run_opcode (program : t) (op : OpCode.t) =
    match op with
    | OpCode.OP_RTI -> run_rti program
    | OpCode.OP_RES -> run_res program
    | OpCode.OP_ADD x -> Ok (run_add x program)
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
    | OpCode.OP_TRAP t ->
      let prog =
        { program with registers = Registers.set_r_r7 program.pc program.registers }
      in
      (match t with
       | Trap.TRAP_GETC -> Ok (exec_trap_getc prog)
       | Trap.TRAP_OUT -> Ok (exec_trap_out prog)
       | Trap.TRAP_PUTS -> Ok (exec_trap_puts prog)
       | Trap.TRAP_IN -> Ok (exec_trap_in prog)
       | Trap.TRAP_PUTSP -> Ok (exec_trap_putsp prog)
       | Trap.TRAP_HALT -> Ok (exec_trap_halt prog))
  ;;

  let run_opcode_unix (program : t) (op : OpCode.t) =
    match op with
    | OpCode.OP_RTI -> run_rti program
    | OpCode.OP_RES -> run_res program
    | OpCode.OP_ADD x -> Ok (run_add x program)
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
    | OpCode.OP_TRAP t ->
      let prog =
        { program with registers = Registers.set_r_r7 program.pc program.registers }
      in
      (match t with
       | Trap.TRAP_GETC -> Ok (exec_trap_getc_unix prog)
       | Trap.TRAP_OUT -> Ok (exec_trap_out_unix prog)
       | Trap.TRAP_PUTS -> Ok (exec_trap_puts_unix prog)
       | Trap.TRAP_IN -> Ok (exec_trap_in_unix prog)
       | Trap.TRAP_PUTSP -> Ok (exec_trap_putsp_unix prog)
       | Trap.TRAP_HALT -> Ok (exec_trap_halt_unix prog))
  ;;

  let should_continue ({ input_state; _ } : t) =
    match input_state with
    | NotAsked -> true
    | InputingChar opt -> Option.is_some opt
  ;;

  let unwrap_prog prog =
    match prog with
    | Ok prog -> prog
    | Error `Unused -> failwith "unused"
    | Error (`UnknownOp _) -> failwith "unknown op"
    | Error (`UnknownTrap _) -> failwith "unkown trap"
    | Error (`UnkownRegister _) -> failwith "unkown register"
  ;;

  let step prog =
    if (not (should_continue prog)) || not prog.running
    then prog
    else
      Memory.read ~pos:prog.pc prog.memory
      |> OpCode.parse
      |> Result.map (run_opcode { prog with pc = prog.pc + 1 })
      |> Result.join
      |> unwrap_prog
  ;;

  let run (program : t) =
    let rec aux prog =
      if not prog.running
      then ()
      else
        Memory.read ~pos:prog.pc prog.memory
        |> OpCode.parse
        |> Result.map (run_opcode_unix { prog with pc = prog.pc + 1 })
        |> Result.join
        |> unwrap_prog
        |> aux
    in
    aux program
  ;;

  let render_special_registers (program : t) =
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let title = Utils.header "SPECIAL REGISTERS" in
    let reg_line name value =
      W.hbox
        [ W.string ~attr:A.(fg green ++ st bold) (Printf.sprintf "%-8s" name)
          |> Lwd.return
        ; W.string ~attr:A.(fg (gray 5)) "0x" |> Lwd.return
        ; W.string ~attr:A.(fg white) (Printf.sprintf "%04x" value) |> Lwd.return
        ]
    in
    W.vbox
      [ title
      ; Ui.space 0 1 |> Lwd.return
      ; reg_line "R_PC" program.pc
      ; reg_line "R_COND" program.cond
      ]
  ;;

  let render_current_op pc op =
    let module W = Nottui_widgets in
    let module A = Notty.A in
    W.hbox
      [ W.string ~attr:A.(fg green ++ st bold) " + " |> Lwd.return
      ; W.string ~attr:A.(fg (gray 5)) "0x" |> Lwd.return
      ; W.string ~attr:A.(fg white) (Printf.sprintf "%04x" pc) |> Lwd.return
      ; W.string ~attr:A.(fg (gray 5)) " → " |> Lwd.return
      ; OpCode.render op
      ]
  ;;

  let render_op pc op =
    let module W = Nottui_widgets in
    let module A = Notty.A in
    W.hbox
      [ W.string ~attr:A.(fg (gray 5)) " - " |> Lwd.return
      ; W.string ~attr:A.(fg (gray 5)) "0x" |> Lwd.return
      ; W.string ~attr:A.(fg white) (Printf.sprintf "%04x" pc) |> Lwd.return
      ; W.string ~attr:A.(fg (gray 5)) " → " |> Lwd.return
      ; OpCode.render op
      ]
  ;;

  let render_op_parse_error instr =
    let module W = Nottui_widgets in
    let module A = Notty.A in
    W.string ~attr:A.(fg red) (Printf.sprintf "Invalid instruction: 0x%04x" instr)
    |> Lwd.return
  ;;

  let list_next_n_ops n (program : t) =
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let pc = program.pc in
    Memory.portion ~start:pc ~count:n program.memory
    |> List.mapi (fun i instr ->
      match OpCode.parse instr with
      | Ok op -> render_op (pc + i + 1) op
      | Error _ -> render_op_parse_error instr)
  ;;

  let render_ops (program : t) =
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let header = Utils.header "INSTRUCTIONS" in
    let current_instr = Memory.read ~pos:(program.pc - 1) program.memory in
    match OpCode.parse current_instr with
    | Ok op ->
      W.vbox
        (header
         :: (Ui.space 0 1 |> Lwd.return)
         :: render_current_op program.pc op
         :: list_next_n_ops 5 program)
    | Error _ ->
      W.vbox
        [ header
        ; Ui.space 0 1 |> Lwd.return
        ; W.string
            ~attr:A.(fg red)
            (Printf.sprintf "Invalid instruction: 0x%04x" current_instr)
          |> Lwd.return
        ]
  ;;

  let render_input_state (program : t) =
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let input_state_str =
      match program.input_state with
      | NotAsked -> "Input State: Not Asked"
      | InputingChar None -> "Input State: Inputting Character (waiting)"
      | InputingChar (Some c) -> Printf.sprintf "Input State: Inputting Character (%c)" c
    in
    let input_state = W.string ~attr:A.(fg white) input_state_str |> Lwd.return in
    W.vbox [ Utils.header "INPUT STATE"; Ui.space 0 1 |> Lwd.return; input_state ]
  ;;

  let render_output_panel (program : t) =
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let strip_ansi str =
      let re = Str.regexp "\027\\[[0-9;]*[mHJ]" in
      Str.global_replace re "" str
    in
    W.vbox
      (Utils.header "PROGRAM OUTPUT"
       :: (Ui.space 0 1 |> Lwd.return)
       :: (program.output_buffer
           |> strip_ansi
           |> String.split_on_char '\n'
           |> List.map (fun line -> W.string line |> Lwd.return)))
  ;;

  let render (program : t) =
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let registers = Registers.render program.registers in
    let special_registers = render_special_registers program in
    let ops = render_ops program in
    let input_state_panel = render_input_state program in
    let output_panel = render_output_panel program in
    W.h_pane
      output_panel
      (W.v_pane (W.v_pane registers special_registers) (W.v_pane ops input_state_panel))
  ;;
end
