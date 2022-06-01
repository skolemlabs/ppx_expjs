let f (x : string) (y : int) =
  print_endline (Printf.sprintf "ppx_expjs: %s %d" x y)
  [@@expjs]

let g ~(x : int) ~(y : string) ?(z : int option) () =
  let z = Option.value z ~default:10 in
  print_endline
    (Printf.sprintf "ppx_expjs (g): x=%d y=%s z=%d" x
       (String.uppercase_ascii y)
       z)
  [@@expjs]
