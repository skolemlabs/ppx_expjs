open Tezt
open Tezt.Base
open Helpers

let () =
  Test.register ~__FILE__ ~title:"Export int"
    ~tags:[ "constant"; "to_js"; "int" ]
  @@ fun () ->
  let to_transform = [%str let x : int = 4 [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x : int = 4 [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          (Ppx_expjs_runtime.int_to_js x)

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
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
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x : string = "string" [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          (Js_of_ocaml.Js.string x)

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
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
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x : unit = () [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          ((fun () -> Js_of_ocaml.Js.undefined) x)

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
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
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x : string array = [| "x"; "y"; "z" |] [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          ((fun v -> Js_of_ocaml.Js.array (Array.map Js_of_ocaml.Js.string v))
             x)

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
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
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x = "string" [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          x

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__
    ~title:"Export constant in strict mode without type or conversion"
    ~tags:[ "constant"; "to_js"; "strict" ]
  @@ fun () ->
  let run () =
    let to_transform = [%str let x = 10 [@@expjs { strict = true }]] in
    ignore @@ Ppx_expjs.expand_expjs to_transform
  in
  let should_raise =
    Ppx_expjs.Unknown_to_js { txt = "x"; loc = !Ast_helper.default_loc }
  in
  Check.raises should_raise run ~error_msg:"Expected PPX to raise %L, got %R";
  unit

let () =
  Test.register ~__FILE__
    ~title:"Export constant in strict mode without type but with conversion"
    ~tags:[ "constant"; "to_js"; "strict" ]
  @@ fun () ->
  let to_transform =
    [%str
      let (x [@expjs.conv string_conv]) = "string" [@@expjs { strict = true }]]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let (x [@expjs.conv string_conv]) = "string" [@@expjs { strict = true }]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          (string_conv x)

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in

  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export constant with structure extension"
    ~tags:[ "constant"; "to_js"; "extension" ]
  @@ fun () ->
  let to_transform = [%str let%test_ext x : string = "string" [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]

      let%test_ext x : string = "string" [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
          (Js_of_ocaml.Js.string "x")
          (Js_of_ocaml.Js.string x)

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export constant inside module"
    ~tags:[ "constant"; "to_js"; "module" ]
  @@ fun () ->
  let to_transform =
    [%str
      module M = struct
        let x : string = "string" [@@expjs]
      end]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]

      module M = struct
        let __ppx_expjs_parent = __ppx_expjs_export
        let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]

        let () =
          Js_of_ocaml.Js.Unsafe.set __ppx_expjs_parent
            (Js_of_ocaml.Js.string "M")
            __ppx_expjs_export

        let x : string = "string" [@@expjs]

        let () =
          Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
            (Js_of_ocaml.Js.string "x")
            (Js_of_ocaml.Js.string x)
      end

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Don't export unit (export = false)"
    ~tags:[ "constant"; "to_js"; "unit" ]
  @@ fun () ->
  let to_transform = [%str let x : unit = () [@@expjs { export = false }]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x : unit = () [@@expjs { export = false }]
      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Locally bind int"
    ~tags:[ "constant"; "to_js"; "unit" ]
  @@ fun () ->
  let to_transform =
    [%str let x : int = 1 [@@expjs { local_bind = true; export = false }]]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let x : int = 1 [@@expjs { local_bind = true; export = false }]
      let x_js = Ppx_expjs_runtime.int_to_js x
      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed
