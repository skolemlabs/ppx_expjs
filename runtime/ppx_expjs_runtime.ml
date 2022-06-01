open Js_of_ocaml

let get_opt obj key =
  let v : _ Js.optdef = Js.Unsafe.get obj key in
  Js.Optdef.to_option v

external int_of_js : float Js.t -> int = "%identity"
external float_of_js : float Js.t -> int = "%identity"
