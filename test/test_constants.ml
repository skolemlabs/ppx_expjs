open Tezt
open Helpers

let () =
  Test.register ~__FILE__ ~title:"Export int"
    ~tags:[ "constant"; "to_js"; "int" ]
  @@ fun () ->
  let to_transform = [%str let x : int = 4 [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let x : int = 4 [@@expjs]
      let () = Js_of_ocaml.Js.export "x" (Ppx_expjs_runtime.int_to_js x)]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export string"
    ~tags:[ "constant"; "to_js"; "string" ]
  @@ fun () ->
  let to_transform = [%str let x : string = "string" [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let x : string = "string" [@@expjs]
      let () = Js_of_ocaml.Js.export "x" (Js_of_ocaml.Js.string x)]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export unit"
    ~tags:[ "constant"; "to_js"; "unit" ]
  @@ fun () ->
  let to_transform = [%str let x : unit = () [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let x : unit = () [@@expjs]

      let () =
        Js_of_ocaml.Js.export "x" ((fun () -> Js_of_ocaml.Js.undefined) x)]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export string array"
    ~tags:[ "constant"; "to_js"; "string"; "array" ]
  @@ fun () ->
  let to_transform =
    [%str let x : string array = [| "x"; "y"; "z" |] [@@expjs]]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let x : string array = [| "x"; "y"; "z" |] [@@expjs]

      let () =
        Js_of_ocaml.Js.export "x"
          ((fun v -> Js_of_ocaml.Js.array (Array.map Js_of_ocaml.Js.string v))
             x)]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export unknown"
    ~tags:[ "constant"; "to_js"; "unknown" ]
  @@ fun () ->
  let to_transform = [%str let x = "string" [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let x = "string" [@@expjs]
      let () = Js_of_ocaml.Js.export "x" x]
  in
  test_structures ~expected ~received:transformed
