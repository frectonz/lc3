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
