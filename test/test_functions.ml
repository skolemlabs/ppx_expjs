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
      let f (x : int) = print_int x [@@expjs]

      let () =
        Js_of_ocaml.Js.export "f" (fun x -> f (Ppx_expjs_runtime.int_of_js x))]
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
      let f (x : string option) = Option.iter print_endline x [@@expjs]

      let () =
        Js_of_ocaml.Js.export "f" (fun x ->
            f
              ((fun v ->
                 Option.map Js_of_ocaml.Js.to_string
                   (Js_of_ocaml.Js.Optdef.to_option v))
                 x))]
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
      let f ~(x : string) = print_endline x [@@expjs]

      let () =
        Js_of_ocaml.Js.export "f" (fun labelled ->
            f
              ~x:
                (Js_of_ocaml.Js.to_string
                   (Ppx_expjs_runtime.get_required labelled "x")))]
  in
  test_structures ~expected ~received:transformed

let () =
  Test.register ~__FILE__
    ~title:"Export function in strict mode without conversion"
    ~tags:[ "function"; "of_js"; "strict" ]
  @@ fun () ->
  let run () =
    let to_transform =
      [%str let f x = print_int x [@@expjs { strict = true }]]
    in
    ignore @@ Ppx_expjs.expand_expjs to_transform
  in
  Check.raises (Ppx_expjs.No_conversion_specified "x") run
    ~error_msg:"Expected PPX to raise %L, got %R";
  unit
