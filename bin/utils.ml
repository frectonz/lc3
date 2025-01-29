let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e
;;

let bits ?(pos = 0) ~width n =
  let mask = (1 lsl width) - 1 in
  (n lsr pos) land mask
;;

let sign_extend x bit_count =
  if bits x ~pos:(bit_count - 1) ~width:1 = 0 then x else x lor (0xffff lsl bit_count)
;;

let signed_bits ?(pos = 0) ~width n = sign_extend (bits ~pos ~width n) width
let ( +^ ) x y = (x + y) land 0xffff
