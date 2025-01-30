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

  let register_of_int = function
    | 0 -> Ok R_R0
    | 1 -> Ok R_R1
    | 2 -> Ok R_R2
    | 3 -> Ok R_R3
    | 4 -> Ok R_R4
    | 5 -> Ok R_R5
    | 6 -> Ok R_R6
    | 7 -> Ok R_R7
    | r -> Error (`UnkownRegister r)
  ;;

  let make () =
    { r_r0 = 0; r_r1 = 0; r_r2 = 0; r_r3 = 0; r_r4 = 0; r_r5 = 0; r_r6 = 0; r_r7 = 0 }
  ;;

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
  ;;

  let set ~index ~value registers =
    match index with
    | R_R0 -> { registers with r_r0 = value }
    | R_R1 -> { registers with r_r1 = value }
    | R_R2 -> { registers with r_r2 = value }
    | R_R3 -> { registers with r_r3 = value }
    | R_R4 -> { registers with r_r4 = value }
    | R_R5 -> { registers with r_r5 = value }
    | R_R6 -> { registers with r_r6 = value }
    | R_R7 -> { registers with r_r7 = value }
  ;;

  let r_r0 = get R_R0
  let r_r1 = get R_R1
  let r_r2 = get R_R2
  let r_r3 = get R_R3
  let r_r4 = get R_R4
  let r_r5 = get R_R5
  let r_r6 = get R_R6
  let r_r7 = get R_R7
  let set_r_r0 v = set ~index:R_R0 ~value:v
  let set_r_r1 v = set ~index:R_R1 ~value:v
  let set_r_r2 v = set ~index:R_R2 ~value:v
  let set_r_r3 v = set ~index:R_R3 ~value:v
  let set_r_r4 v = set ~index:R_R4 ~value:v
  let set_r_r5 v = set ~index:R_R5 ~value:v
  let set_r_r6 v = set ~index:R_R6 ~value:v
  let set_r_r7 v = set ~index:R_R7 ~value:v

  let render (registers : t) =
    let open Nottui in
    let module W = Nottui_widgets in
    let module A = Notty.A in
    let reg_line name value =
      W.hbox
        [ W.string ~attr:A.(fg green ++ st bold) (Printf.sprintf "%-4s" name)
          |> Lwd.return
        ; W.string ~attr:A.(fg (gray 5)) "0x" |> Lwd.return
        ; W.string ~attr:A.(fg white) (Printf.sprintf "%04x" value) |> Lwd.return
        ]
    in
    W.vbox
      [ Utils.header "REGISTERS"
      ; Ui.space 0 1 |> Lwd.return
      ; reg_line "R0" registers.r_r0
      ; reg_line "R1" registers.r_r1
      ; reg_line "R2" registers.r_r2
      ; reg_line "R3" registers.r_r3
      ; reg_line "R4" registers.r_r4
      ; reg_line "R5" registers.r_r5
      ; reg_line "R6" registers.r_r6
      ; reg_line "R7" registers.r_r7
      ]
  ;;
end
