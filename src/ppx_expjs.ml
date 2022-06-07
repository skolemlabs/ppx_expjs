open Ppxlib
open Ast_builder.Default

exception Unsupported_type of core_type
exception Unsupported_pattern of pattern
exception Unsupported_expression of expression
exception Invalid_payload of payload

let get_loc () = !Ast_helper.default_loc

let contains_attr attrs str =
  List.exists (fun { attr_name = { txt; _ }; _ } -> txt = str) attrs

let get_attr attrs str =
  List.find (fun { attr_name = { txt; _ }; _ } -> txt = str) attrs

let contains_field expr name =
  match expr with
  | { pexp_desc = Pexp_record (fields, _); _ } ->
      List.exists
        (fun (f, _) ->
          match f with { txt = Lident n; _ } -> n = name | _ -> false)
        fields
  | e -> raise (Unsupported_expression e)

let get_field expr name =
  match expr with
  | { pexp_desc = Pexp_record (fields, _); _ } ->
      let _, e =
        List.find
          (fun (f, _) ->
            match f with { txt = Lident n; _ } -> n = name | _ -> false)
          fields
      in
      e
  | e -> raise (Unsupported_expression e)

(** Extracts the name of the function from the value binding *)
let get_fun_name = function
  | { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; _ } -> txt
  | { pvb_pat; _ } -> raise (Unsupported_pattern pvb_pat)

(** Determine what we should export the value as. If the user has specified a [name], we use that. Otherwise, it's just the function name *)
let get_exp_name = function
  | {
      pvb_attributes =
        [ { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ } ];
      _;
    }
    when contains_field e "name" -> (
      match get_field e "name" with
      | { pexp_desc = Pexp_constant (Pconst_string (name, _, _)); _ } -> name
      | e -> raise (Unsupported_expression e))
  | vb -> get_fun_name vb

(** Extracts the custom convertor from the attribute, and fails if the payload is malformed. *)
let get_custom_conv = function
  | { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ } -> e
  | { attr_payload; _ } -> raise (Invalid_payload attr_payload)

(** Based on the type of the argument, generates/uses a known conversion from JS -> OCaml *)
let rec of_js = function
  (* If the user has specified a custom conversion on this type, always use it *)
  | { ptyp_attributes; _ } when contains_attr ptyp_attributes "expjs.conv" ->
      let attr = get_attr ptyp_attributes "expjs.conv" in
      Some (get_custom_conv attr)
  | [%type: string] -> Some [%expr Js_of_ocaml.Js.to_string]
  | [%type: bool] -> Some [%expr Js_of_ocaml.Js.to_bool]
  | [%type: int] -> Some [%expr Ppx_expjs_runtime.int_of_js]
  | [%type: float] -> Some [%expr Ppx_expjs_runtime.float_of_js]
  (* The following types are all recursive, i.e. they contain another type. In order to convert these,
     we use functors like [Option.map] and recurse to determine how to convert the "boxed" type. *)
  | { ptyp_desc = Ptyp_constr ({ txt = [%lid option]; _ }, [ t ]); _ } -> (
      match of_js t with
      | Some c ->
          Some
            [%expr
              fun v -> Option.map [%e c] (Js_of_ocaml.Js.Optdef.to_option v)]
      | None -> Some [%expr Js_of_ocaml.Js.Optdef.to_option])
  | _ -> None

let get_arg = function
  | {
      ppat_desc =
        Ppat_constraint ({ ppat_desc = Ppat_var { txt = name; _ }; _ }, typ);
      _;
    } ->
      let conv = of_js typ in
      (name, conv)
  | { ppat_desc = Ppat_var { txt = name; _ }; _ } -> (name, None)
  | [%pat? ()] -> ("()", None)
  | p -> raise (Unsupported_pattern p)

let rec collect_args expr curr =
  match expr with
  | { pexp_desc = Pexp_fun (l, _, p, expr); _ } ->
      let name, conv = get_arg p in
      collect_args expr ((l, name, conv) :: curr)
  | _ -> List.rev curr

(** Constructs the prototype of the lambda to be exported. *)
let build_prototype args =
  let prototype, named =
    List.fold_left
      (fun (f, named) (label, name, _) ->
        let loc = get_loc () in
        let labelled = label <> Nolabel in
        let named' = named || labelled in
        let f' =
          (* We skip unit arguments and labelled ones.
             Unit arguments are not useful in the JS world, because there is no partial application.
             Labelled arguments we bundle into the [labelled] arg, which is a JS object we access fields from. *)
          if labelled || name = "()" then f
          else
            let parg = ppat_var ~loc { loc; txt = name } in
            let fexp = pexp_fun ~loc Nolabel None parg in
            fun e -> f (fexp e)
        in
        (f', named'))
      ((fun e -> e), false)
      args
  in
  (* If we found any named/labelled arguments, we add the [labelled] arg *)
  if named then
    let loc = get_loc () in
    let parg = ppat_var ~loc { loc; txt = "labelled" } in
    fun e -> pexp_fun ~loc Nolabel None parg (prototype e)
  else prototype

(** Constructs the body of the function, which converts all the arguments and calls the OCaml function *)
let build_body fname args =
  let eargs =
    List.fold_left
      (fun acc (l, name, conv) ->
        let loc = get_loc () in
        let apply_opt_conv exp =
          match conv with
          | Some c -> pexp_apply ~loc c [ (Nolabel, exp) ]
          | None -> exp
        in
        match (name, l) with
        | "()", _ -> (Nolabel, [%expr ()]) :: acc
        | _, Nolabel -> (Nolabel, apply_opt_conv @@ evar ~loc name) :: acc
        | _, Labelled l ->
            let name_str =
              pexp_constant ~loc (Pconst_string (name, loc, None))
            in
            ( Labelled l,
              apply_opt_conv
              @@ pexp_apply ~loc [%expr Ppx_expjs_runtime.get_required]
                   [ (Nolabel, evar ~loc "labelled"); (Nolabel, name_str) ] )
            :: acc
        | _, Optional o ->
            let name_str =
              pexp_constant ~loc (Pconst_string (name, loc, None))
            in
            ( Optional o,
              apply_opt_conv
              @@ pexp_apply ~loc [%expr Js_of_ocaml.Js.Unsafe.get]
                   [ (Nolabel, evar ~loc "labelled"); (Nolabel, name_str) ] )
            :: acc)
      [] args
    |> List.rev
  in
  let loc = get_loc () in
  pexp_apply ~loc (evar ~loc fname) eargs

let build_fun fname args =
  let prototype = build_prototype args in
  let body = build_body fname args in
  prototype body

let build_export fname fexpr =
  let loc = get_loc () in
  let export = [%expr Js_of_ocaml.Js.export] in
  let fname_str = pexp_constant ~loc (Pconst_string (fname, loc, None)) in
  pexp_apply ~loc export [ (Nolabel, fname_str); (Nolabel, fexpr) ]

class attribute_mapper =
  object
    inherit Ast_traverse.map as super

    method! structure s =
      let s = super#structure s in
      let l =
        List.fold_left
          (fun acc si ->
            match si.pstr_desc with
            | Pstr_value (r, vbs) ->
                let vbs' =
                  List.fold_left
                    (fun acc vb ->
                      if contains_attr vb.pvb_attributes "expjs" then
                        let fname = get_fun_name vb in
                        let ename = get_exp_name vb in
                        let args = collect_args vb.pvb_expr [] in
                        let fexpr = build_fun fname args in
                        let export = build_export ename fexpr in
                        let loc = get_loc () in
                        let expvb =
                          value_binding ~loc ~pat:[%pat? ()] ~expr:export
                        in
                        expvb :: acc
                      else acc)
                    [] vbs
                in
                let loc = get_loc () in
                pstr_value ~loc r vbs' :: acc
            | _ -> acc)
          [] s
      in
      s @ List.rev l
  end

let expand_expjs = (new attribute_mapper)#structure
let () = Driver.register_transformation "expjs" ~impl:expand_expjs
