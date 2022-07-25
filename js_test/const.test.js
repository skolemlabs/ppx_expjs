const cnst = require("../_build/default/js_test/const.bc.js")

test('cnst.my_str is "my_str"', () => {
  expect(cnst.my_str).toBe("my_str");
});

test('cnst.two_plus_two is 4', () => {
  expect(cnst.two_plus_two).toBe(4);
});

test('cnst.none_four is null', () => {
  expect(cnst.none_four).toBe(null);
});

test('cnst.some_four is 4', () => {
  expect(cnst.some_four).toBe(4);
});
