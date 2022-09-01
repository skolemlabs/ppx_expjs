open Js_of_ocaml

exception Missing_required of string
(** Raised when a required labelled argument is missing *)

val get_required : 'a -> string -> 'b
(** [get_required obj key] indexes into JS object [obj] using [key], raising [Missing_required key] if the [obj.key] is [undefined] *)

val get_opt : 'a -> string -> 'b option
(** [get_opt obj key] indexed into JS object [obj] using [key] and returns [Some v] if [obj.key] is not [undefined], otherwise [None] *)

val int_of_js : float Js.t -> int
val float_of_js : float Js.t -> float
val int_to_js : int -> float Js.t
val float_to_js : float -> float Js.t
