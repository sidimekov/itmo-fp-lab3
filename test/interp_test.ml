open Alcotest
open My_lab3_lib

let float_eps a b =
  let d = Float.abs (a -. b) in
  d <= 1e-9

let test_linear_between () =
  let p1 : Interp.point = { x = 0.0; y = 0.0 } in
  let p2 : Interp.point = { x = 1.0; y = 1.0 } in
  let y = Interp.Linear.interpolate_between p1 p2 0.7 in
  check bool "y=0.7" true (float_eps y 0.7)

let test_newton_line () =
  let pts : Interp.point list =
    [
      { x = 0.0; y = 0.0 };
      { x = 1.0; y = 1.0 };
      { x = 2.0; y = 2.0 };
      { x = 3.0; y = 3.0 };
    ]
  in
  let y = Interp.Newton.interpolate_at pts 2.5 in
  check bool "y=2.5" true (float_eps y 2.5)

let () =
  run "my_lab3"
    [
      ( "interp",
        [
          test_case "linear_between" `Quick test_linear_between;
          test_case "newton_line" `Quick test_newton_line;
        ] );
    ]
