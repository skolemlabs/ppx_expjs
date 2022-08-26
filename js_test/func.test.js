const func = require("../_build/default/js_test/func.bc.js")

test('func.add(2, 3) is 5', () => {
  const res = func.add(2, 3)
  expect(res).toBe(5)
});

test('func.prepend({prefix: "Sk"}, "olem") is "Skolem"', () => {
  const res = func.prepend({prefix: "Sk"}, "olem")
  expect(res).toBe("Skolem")
});

test('func.prepend({}, "olem") throws required argument error', () => {
  expect(() => {
    func.prepend({}, "olem")
  }).toThrow(Array) /* OCaml exceptions look like arrays
                      We might want to change this */
});

test('func.substr({}, "Skolem") is "Skolem"', () => {
  const res = func.substr({}, "Skolem")
  expect(res).toBe("Skolem")
});

test('func.substr({i: 2}, "Skolem") is "Sk"', () => {
  const res = func.substr({i: 2}, "Skolem")
  expect(res).toBe("Sk")
});
