let f (x : string) (y : int) : string =
  print_endline (Printf.sprintf "ppx_expjs: %s %d" x y);
  "return :)"
  [@@expjs]

let g ~(x : int) ~(y : string) ?(z : int = 10) () =
  print_endline
    (Printf.sprintf "ppx_expjs (g): x=%d y=%s z=%d" x (String.uppercase_ascii y)
       z)
  [@@expjs { name = "g_js" }]

let return_unit (a : unit) : unit = ignore a [@@expjs]
let some_string () : string option = Some "a" [@@expjs]
let none_string () : string option = None [@@expjs]
let my_str : string = "my_string" [@@expjs]
let my_int : int = 4 [@@expjs]

let sum_array (arr : int array) : int =
  Array.fold_left (fun acc elt -> acc + elt) 0 arr
  [@@expjs]

let split_num (x : int) : int array =
  let y = x / 2 in
  [| y; x - y |]
  [@@expjs]

let not_exported = 0xbeef
let () = ignore not_exported
