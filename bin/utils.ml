let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e
;;
