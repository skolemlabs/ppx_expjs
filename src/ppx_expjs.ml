open Ppxlib
open Ast_builder.Default

type config = { exp_name : string; strict : bool }

exception Unsupported_pattern of pattern
exception Unsupported_expression of expression
exception Invalid_payload of payload
exception Unknown_of_js of string loc
exception Unknown_to_js of string loc

let () =
  Printexc.record_backtrace true;
  Printexc.register_printer (function
    | Unsupported_pattern p ->
        Some (Format.asprintf "Unsupported_pattern: %a" Pprintast.pattern p)
    | Unsupported_expression e ->
        Some
          (Format.asprintf "Unsupported_expression: %a" Pprintast.expression e)
    | Unknown_of_js { txt; loc } ->
        Some (Format.asprintf "Unknown_of_js: %s @ %a" txt Location.print loc)
    | Unknown_to_js { txt; loc } ->
        Some (Format.asprintf "Unknown_to_js: %s @ %a" txt Location.print loc)
    | _ -> None)

module Consts = struct
  let ppx_name = "expjs"
  let conv_attr_name = "expjs.conv"
end

module Structures = struct
  let rebind_parent_obj = [%str let __ppx_expjs_parent = __ppx_expjs_export]

  let export_obj =
    [%str let __ppx_expjs_export = Js_of_ocaml.Js.Unsafe.obj [||]]

  let export_module name =
    ([%str
       let () =
         Js_of_ocaml.Js.Unsafe.set __ppx_expjs_parent
           (Js_of_ocaml.Js.string "n")
           __ppx_expjs_export]
    [@subst let n : string = name])

  let export_root_obj =
    [%str let () = Js_of_ocaml.Js.export_all __ppx_expjs_export]
end

module Expressions = struct
  let export fname fexpr =
    ([%expr
       Js_of_ocaml.Js.Unsafe.set __ppx_expjs_export
         (Js_of_ocaml.Js.string "fn")
         [%e fexpr]]
    [@subst let fn : string = fname])
end

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
let get_var_name vb =
  let rec helper = function
    | { ppat_desc = Ppat_var { txt; _ }; _ } -> txt
    | { ppat_desc = Ppat_constraint (p, _); _ } -> helper p
    | p -> raise (Unsupported_pattern p)
  in
  helper vb.pvb_pat

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
  | vb -> get_var_name vb

(** Determine if we should be using strict mode. In strict mode, all types must explicitly have convertors specified. *)
let get_strict = function
  | {
      pvb_attributes =
        [ { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ } ];
      _;
    }
    when contains_field e "strict" -> (
      match get_field e "strict" with
      | [%expr true] -> true
      | [%expr false] -> false
      | e -> raise (Unsupported_expression e))
  | _ -> false

let get_config vb = { exp_name = get_exp_name vb; strict = get_strict vb }

(** Extracts the custom convertor from the attribute, and fails if the payload is malformed. *)
let get_custom_conv = function
  | { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ } -> e
  | { attr_payload; _ } -> raise (Invalid_payload attr_payload)

(** Based on the type of the argument, generates/uses a known conversion from JS -> OCaml *)
let rec of_js = function
  (* If the user has specified a custom conversion on this type, always use it *)
  | { ptyp_attributes; _ }
    when contains_attr ptyp_attributes Consts.conv_attr_name ->
      let attr = get_attr ptyp_attributes Consts.conv_attr_name in
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
  | { ptyp_desc = Ptyp_constr ({ txt = [%lid array]; _ }, [ t ]); _ } -> (
      match of_js t with
      | Some c ->
          Some [%expr fun v -> Array.map [%e c] (Js_of_ocaml.Js.to_array v)]
      | None -> Some [%expr Js_of_ocaml.Js.Optdef.to_array])
  | _ -> None

let rec to_js = function
  (* If the user has specified a custom conversion on this type, always use it *)
  | { ptyp_attributes; _ }
    when contains_attr ptyp_attributes Consts.conv_attr_name ->
      let attr = get_attr ptyp_attributes Consts.conv_attr_name in
      Some (get_custom_conv attr)
  | [%type: string] -> Some [%expr Js_of_ocaml.Js.string]
  | [%type: bool] -> Some [%expr Js_of_ocaml.Js.bool]
  | [%type: int] -> Some [%expr Ppx_expjs_runtime.int_to_js]
  | [%type: float] -> Some [%expr Ppx_expjs_runtime.float_to_js]
  | [%type: unit] -> Some [%expr fun () -> Js_of_ocaml.Js.undefined]
  | { ptyp_desc = Ptyp_constr ({ txt = [%lid option]; _ }, [ t ]); _ } -> (
      match to_js t with
      | Some c ->
          Some
            (* Map the value inside the option, then unbox it to make it a JS value *)
            [%expr fun v -> Js_of_ocaml.Js.Opt.option (Option.map [%e c] v)]
      | None -> Some [%expr Js_of_ocaml.Js.Optdef.option])
  | { ptyp_desc = Ptyp_constr ({ txt = [%lid array]; _ }, [ t ]); _ } -> (
      match to_js t with
      | Some c ->
          Some
            (* Map the value inside the array, then unbox it to make it a JS value *)
            [%expr fun v -> Js_of_ocaml.Js.array (Array.map [%e c] v)]
      | None -> Some [%expr Js_of_ocaml.Js.Optdef.array])
  | _ -> None

let get_arg ~strict = function
  (* Optimal case: we get type info and a possible JS -> OCaml convertor. *)
  | {
      ppat_desc =
        Ppat_constraint
          ({ ppat_desc = Ppat_var ({ txt = name; _ } as loc); _ }, typ);
      _;
    } ->
      let conv = of_js typ in
      (* If we're in strict mode and we weren't able to generate a convertor, we raise. *)
      if strict && Option.is_none conv then raise (Unknown_of_js loc);
      (name, conv)
  (* When we don't have any type info, the user still may have specified a conversion. We should use that. *)
  | { ppat_desc = Ppat_var { txt = name; _ }; ppat_attributes; _ }
    when contains_attr ppat_attributes Consts.conv_attr_name ->
      let attr = get_attr ppat_attributes Consts.conv_attr_name in
      (name, Some (get_custom_conv attr))
  (* If we're in strict mode and we have no type/conversion info, we raise. *)
  | { ppat_desc = Ppat_var loc; _ } when strict -> raise (Unknown_of_js loc)
  | [%pat? ()] -> ("()", None)
  | p -> raise (Unsupported_pattern p)

let rec get_args_and_conv ~strict ~fname expr pat curr =
  match (expr, strict) with
  (* If we get a function, recurse through all the arguments. *)
  | { pexp_desc = Pexp_fun (lbl, def_expr, p, rest); _ }, _ ->
      let name, conv = get_arg ~strict p in
      get_args_and_conv ~strict ~fname rest pat
        ((lbl, def_expr, name, conv) :: curr)
  | { pexp_desc = Pexp_constraint (_, t); pexp_loc; _ }, _ ->
      let conv = to_js t in
      (* If we're in strict mode and got no conversion, we raise *)
      if strict && Option.is_none conv then
        raise (Unknown_to_js { loc = pexp_loc; txt = fname });
      (List.rev curr, to_js t)
  (* If the user has specified a custom convertor, use it *)
  | _, _ when contains_attr pat.ppat_attributes Consts.conv_attr_name ->
      let attr = get_attr pat.ppat_attributes Consts.conv_attr_name in
      (List.rev curr, Some (get_custom_conv attr))
  (* If we're in strict mode and no type is specified, we raise *)
  | { pexp_loc; _ }, true ->
      raise (Unknown_to_js { loc = pexp_loc; txt = fname })
  | _, false -> (List.rev curr, None)

(** Constructs the prototype of the lambda to be exported. *)
let build_prototype args =
  let prototype, named =
    List.fold_left
      (fun (f, named) (label, _, name, _) ->
        let loc = get_loc () in
        let labelled = label <> Nolabel in
        let named' = named || labelled in
        let f' =
          (* We skip labelled arguments
             Labelled arguments we bundle into the [labelled] arg, which is a JS object we access fields from. *)
          if labelled then f
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
let build_body fname args conv =
  let eargs =
    List.fold_left
      (fun acc (lbl, def_expr, name, conv) ->
        let loc = get_loc () in
        let apply_opt_conv exp =
          match conv with
          | Some c -> pexp_apply ~loc c [ (Nolabel, exp) ]
          | None -> exp
        in
        match (name, lbl, def_expr) with
        | "()", _, _ -> (Nolabel, [%expr ()]) :: acc
        | _, Nolabel, _ -> (Nolabel, apply_opt_conv @@ evar ~loc name) :: acc
        | _, Labelled l, _ ->
            let name_str =
              pexp_constant ~loc (Pconst_string (name, loc, None))
            in
            ( Labelled l,
              apply_opt_conv
              @@ pexp_apply ~loc [%expr Ppx_expjs_runtime.get_required]
                   [ (Nolabel, evar ~loc "labelled"); (Nolabel, name_str) ] )
            :: acc
        | ( _,
            Optional o,
            None (* If it's optional and has no body, there's no default *) ) ->
            let name_str =
              pexp_constant ~loc (Pconst_string (name, loc, None))
            in
            ( Optional o,
              apply_opt_conv
              @@ pexp_apply ~loc [%expr Js_of_ocaml.Js.Unsafe.get]
                   [ (Nolabel, evar ~loc "labelled"); (Nolabel, name_str) ] )
            :: acc
        | ( _,
            Optional o,
            Some _ (* If it's optional and has a body, there's a default *) ) ->
            let name_str =
              pexp_constant ~loc (Pconst_string (name, loc, None))
            in
            let from_js =
              pexp_apply ~loc [%expr Ppx_expjs_runtime.get_opt]
                [ (Nolabel, evar ~loc "labelled"); (Nolabel, name_str) ]
            in
            ( Optional o,
              match conv with
              | Some c ->
                  pexp_apply ~loc [%expr Option.map]
                    [ (Nolabel, c); (Nolabel, from_js) ]
              | None -> from_js )
            :: acc)
      [] args
    |> List.rev
  in
  let loc = get_loc () in
  let f = pexp_apply ~loc (evar ~loc fname) eargs in
  match conv with
  | Some to_js -> pexp_apply ~loc to_js [ (Nolabel, f) ]
  | None -> f

let build_fun fname args conv =
  let prototype = build_prototype args in
  let body = build_body fname args conv in
  prototype body

class attribute_mapper =
  object
    inherit Ast_traverse.map as super

    method! structure s =
      let s = super#structure s in
      let l =
        List.fold_left
          (fun acc si ->
            match si.pstr_desc with
            | Pstr_module
                ({
                   pmb_name = { txt = Some mod_name; _ };
                   pmb_expr =
                     { pmod_desc = Pmod_structure mod_str; _ } as mod_expr;
                   _;
                 } as mod') ->
                let mod_str' =
                  Structures.(
                    rebind_parent_obj @ export_obj @ export_module mod_name
                    @ mod_str)
                in
                let si' =
                  {
                    si with
                    pstr_desc =
                      Pstr_module
                        {
                          mod' with
                          pmb_expr =
                            {
                              mod_expr with
                              pmod_desc = Pmod_structure mod_str';
                            };
                        };
                  }
                in
                si' :: acc
            (* WARNING: this is very hacky but unfortunately necessary. Touch this code at your peril!
                The Ast_traverse classes traverse the parsetree in a bottom-up manner. This means that if the value being exported
                is attached to some structure extension (such as in jsoo-react), the export is as well. This doesn't play well with
                other PPXs, some of which assume that there is only a single value_binding per structure. Ideally other PPXs shouldn't
                mess with the exports anyway, so we just breakout the export from the extension if we see it. The bottom-up aspect enforces the
                invariant that we created the export before we see it in an extension, which means this code should always work. *)
            | Pstr_extension
                ( ( ext_loc,
                    PStr
                      [
                        ({
                           pstr_desc =
                             Pstr_value
                               (_, [ { pvb_attributes = val_attrs; _ } ]);
                           (* If we see a value structure with a single binding... *)
                         _;
                         } as val_str);
                        exp_str;
                      ] ),
                  ext_attrs )
              when contains_attr val_attrs Consts.ppx_name
                   (* ...we test if it contains the magic attribute. *) ->
                let ext_str' =
                  {
                    si with
                    pstr_desc =
                      Pstr_extension ((ext_loc, PStr [ val_str ]), ext_attrs);
                  }
                in
                exp_str :: ext_str' :: acc
            | Pstr_value (r, vbs) ->
                let vbs' =
                  List.fold_left
                    (fun acc vb ->
                      if contains_attr vb.pvb_attributes Consts.ppx_name then
                        let fname = get_var_name vb in
                        let { strict; exp_name } = get_config vb in
                        let args, conv =
                          get_args_and_conv ~strict ~fname vb.pvb_expr
                            vb.pvb_pat []
                        in
                        let fexpr = build_fun fname args conv in
                        let export = Expressions.export exp_name fexpr in
                        let loc = get_loc () in
                        let expvb =
                          value_binding ~loc ~pat:[%pat? ()] ~expr:export
                        in
                        expvb :: acc
                      else acc)
                    [] vbs
                in
                let loc = get_loc () in
                (* This prevents a parsetree invariant where you have a let binding with no value *)
                if vbs' <> [] then pstr_value ~loc r vbs' :: si :: acc
                else si :: acc
            | _ -> si :: acc)
          [] s
      in
      List.rev l
  end

let expand_expjs s =
  Structures.(export_obj @ (new attribute_mapper)#structure s @ export_root_obj)

let () = Driver.register_transformation Consts.ppx_name ~impl:expand_expjs
