open Tezt
open Tezt.Base

let test_structures ~expected ~received =
  if expected <> received then
    Test.fail "Structures not equal; expected:\n%a\nreceived:\n%a"
      Ppxlib.Pprintast.structure expected Ppxlib.Pprintast.structure received;
  unit
