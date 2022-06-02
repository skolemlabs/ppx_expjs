open Js_of_ocaml

exception Missing_required of string

let get_required obj key =
  let v : _ Js.optdef = Js.Unsafe.get obj key in
  Js.Optdef.get v (fun () -> raise (Missing_required key))

external int_of_js : float Js.t -> int = "%identity"
external float_of_js : float Js.t -> int = "%identity"
