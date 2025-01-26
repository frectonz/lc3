module Array = struct
  include Array

  let remove_first arr =
    if Array.length arr = 0 then [||] else Array.sub arr 1 (Array.length arr - 1)
  ;;
end
