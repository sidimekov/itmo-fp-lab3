open My_lab3_lib

let () =
  let p1 : Interp.point = { x = 0.0; y = 0.0 } in
  let p2 : Interp.point = { x = 1.0; y = 1.0 } in
  let y = Interp.Linear.interpolate_between p1 p2 0.7 in
  Printf.printf "y(0.7)=%.6f\n" y
