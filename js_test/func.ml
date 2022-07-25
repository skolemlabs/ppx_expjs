let add (x : int) (y : int) : int = x + y [@@expjs]
let prepend ~(prefix : string) (str : string) : string = prefix ^ str [@@expjs]
