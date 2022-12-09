# `ppx_expjs`
![CI status](https://github.com/skolemlabs/ppx_expjs/actions/workflows/main.yml/badge.svg)

A ppx for exporting OCaml values to JavaScript using type annotations.

## Example

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

## Usage
### Basic
If you want to convert between JS and OCaml values of "standard" types (i.e. integers, strings, optional values), simply add the
`[@@expjs]` attribute to any value binding with type annotations, i.e.

```ocaml
let my_int : int = 10 [@@expjs]
```

### Custom conversions
You can specify a custom convertor function to/from JS using the `[@expjs.conv]` attribute:

```ocaml
let f (x [@expjs.conv int_of_js]) = ... [@@expjs]
```

### Strict mode
You can require that all values have conversions by enabling strict mode. For example, the following block will fail since there are no type annotations
for `x`:
```ocaml
let f x = ... [@@expjs { strict = true }]
```

Without strict mode, values that don't have known or explicit conversion functions are passed directly as JS values.

### Named arguments
Named arguments use the standard JavaScript convention of having the first argument be an object with fields corresponding to the argument names. For example,
the following OCaml function:

```ocaml
let f ~x = ... [@@expjs]
```
can be called in JS as:
```javascript
const foo = f({x: y});
```

### Default type mappings
| **OCaml**           | **JavaScript** |
|---------------------|----------------|
| `option` : `Some x` | `x`            |
| `option` : `None`   | `null`         |
| `int`               | `number`       |
| `float`             | `number`       |
| `string`            | `String`       |
| `unit`              | `undefined`    |

`list`s are not yet supported, but are planned to be.

## License
This project is licensed under the MIT License.
