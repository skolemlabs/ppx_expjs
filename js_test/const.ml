let my_str : string = "my_str" [@@expjs]
let two_plus_two : int = 4 [@@expjs]
let none_four : int option = None [@@expjs]
let some_four : int option = Some 4 [@@expjs]

module Caml_module = struct
  (** This name is used to make sure the other my_str isn't shadowed *)
  let my_str : string = "Caml_module.my_str" [@@expjs]
end
