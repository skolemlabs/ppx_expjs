# `ppx_expjs`
![CI status](https://github.com/skolemlabs/ppx_expjs/actions/workflows/main.yml/badge.svg)

A ppx for exporting OCaml values to Javascript using type annotations.

🚧 This ppx is a WIP, the API should not be considered stable. 🚧

### Example

`my_module.ml`:
```ocaml
let concat (s1 : string) (s2 : string) : string = s1 ^ s2
[@@expjs]
```
`node`:
```
Welcome to Node.js v16.13.0.
Type ".help" for more information.
> const my_module = require("./_build/default/my_module.bc.js");
undefined
> x.concat("Hello ", "World")
"Hello World"
>
```
