let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e
;;

let ( let@ ) x f =
  match x with
  | `Int v -> f v
  | `Program p -> p
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

let header title =
  let module W = Nottui_widgets in
  let module A = Notty.A in
  W.string ~attr:A.(fg lightblue ++ st bold) title |> Lwd.return
;;
