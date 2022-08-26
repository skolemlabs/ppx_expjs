let add (x : int) (y : int) : int = x + y [@@expjs]
let prepend ~(prefix : string) (str : string) : string = prefix ^ str [@@expjs]

let substr (str : string) ?(i : int = String.length str) () : string =
  String.sub str 0 i
  [@@expjs]
