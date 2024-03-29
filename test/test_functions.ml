open Tezt
open Tezt.Base
open Helpers

let () =
  Test.register ~__FILE__ ~title:"Export function with positional int arg"
    ~tags:[ "function"; "of_js"; "int"; "positional" ]
  @@ fun () ->
  let to_transform = [%str let f (x : int) = print_int x [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let f (x : int) = print_int x [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export (Js_of_ocaml.Js.string "f")
          (fun x -> f (Ppx_expjs_runtime.int_of_js x))

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__
    ~title:"Export function with positional string option arg"
    ~tags:[ "function"; "of_js"; "string"; "option"; "positional" ]
  @@ fun () ->
  let to_transform =
    [%str let f (x : string option) = Option.iter print_endline x [@@expjs]]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let f (x : string option) = Option.iter print_endline x [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export (Js_of_ocaml.Js.string "f")
          (fun x ->
            f
              ((fun v ->
                 Option.map Js_of_ocaml.Js.to_string
                   (Js_of_ocaml.Js.Optdef.to_option v))
                 x))

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__ ~title:"Export function with named string arg"
    ~tags:[ "function"; "of_js"; "string"; "named" ]
  @@ fun () ->
  let to_transform = [%str let f ~(x : string) = print_endline x [@@expjs]] in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let f ~(x : string) = print_endline x [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export (Js_of_ocaml.Js.string "f")
          (fun labelled ->
            f
              ~x:
                (Js_of_ocaml.Js.to_string
                   (Ppx_expjs_runtime.get_required labelled "x")))

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__
    ~title:"Export function with optional string arg (with default)"
    ~tags:[ "function"; "of_js"; "string"; "optional"; "default" ]
  @@ fun () ->
  let to_transform =
    [%str let f ?(x : string = "foobar") = print_endline x [@@expjs]]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]
      let f ?(x : string = "foobar") = print_endline x [@@expjs]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export (Js_of_ocaml.Js.string "f")
          (fun labelled ->
            f
              ?x:
                (Option.map Js_of_ocaml.Js.to_string
                   (Ppx_expjs_runtime.get_opt labelled "x")))

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__
    ~title:
      "Export function in strict mode without type or conversion for argument"
    ~tags:[ "function"; "of_js"; "strict" ]
  @@ fun () ->
  let run () =
    let to_transform =
      [%str let f x = print_int x [@@expjs { strict = true }]]
    in
    ignore @@ Ppx_expjs.expand_expjs to_transform
  in
  let should_raise =
    Ppx_expjs.Unknown_of_js { txt = "x"; loc = !Ast_helper.default_loc }
  in
  Check.raises should_raise run ~error_msg:"Expected PPX to raise %L, got %R";
  unit

let () =
  Test.register ~__FILE__
    ~title:
      "Export function in strict mode without type but with conversion for \
       argument"
    ~tags:[ "function"; "of_js"; "strict" ]
  @@ fun () ->
  let to_transform =
    [%str
      let f (x [@expjs.conv Ppx_expjs_runtime.int_of_js]) : int = print_int x
        [@@expjs { strict = true }]]
  in
  let transformed = Ppx_expjs.expand_expjs to_transform in
  let expected =
    [%str
      let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]

      let f (x [@expjs.conv Ppx_expjs_runtime.int_of_js]) : int = print_int x
        [@@expjs { strict = true }]

      let () =
        Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export (Js_of_ocaml.Js.string "f")
          (fun x ->
            Ppx_expjs_runtime.int_to_js (f (Ppx_expjs_runtime.int_of_js x)))

      let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
  in
  test_structures ~expected ~received:transformed
