open Registers

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
end
