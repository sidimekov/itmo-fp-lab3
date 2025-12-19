open Interp

type algorithm =
  | Linear
  | Newton of int

type config = {
  algorithms : algorithm list;
  step : float;
}

type labeled_point = {
  algo : string;
  point : point;
}

let x_grid ~start ~stop ~step =
  if step <= 0.0 then invalid_arg "x_grid: step must be positive";
  let rec loop acc x =
    if x > stop then List.rev acc
    else loop (x :: acc) (x +. step)
  in
  loop [] start

let apply_linear points xs : labeled_point list =
  let interpolate_one x =
    let left, right = Linear.find_segment points x in
    let y = Linear.interpolate_between left right x in
    { algo = "linear"; point = { x; y } }
  in
  List.map interpolate_one xs

let apply_newton ~window_size points xs : labeled_point list =
  if window_size <= 0 then
    invalid_arg "apply_newton: window_size must be positive";
  (* берем либо все точки, либо первые window_size
     в потоковом режиме сюда передаётся уже готовое окно *)
  let pts =
    let len = List.length points in
    if len <= window_size then points
    else
      let rec take n acc = function
        | _ when n <= 0 -> List.rev acc
        | [] -> List.rev acc
        | p :: ps -> take (n - 1) (p :: acc) ps
      in
      take window_size [] points
  in
  let interpolate_one x =
    let y = Newton.interpolate_at pts x in
    { algo = "newton"; point = { x; y } }
  in
  List.map interpolate_one xs

let run_offline (cfg : config) (points : point list) : labeled_point list =
  match points with
  | [] | [_] -> []
  | p0 :: _ ->
      let last_x =
        List.fold_left (fun acc p -> if p.x > acc then p.x else acc) p0.x points
      in
      let xs = x_grid ~start:p0.x ~stop:last_x ~step:cfg.step in
      let add_for_algo acc algo =
        match algo with
        | Linear ->
            let pts = apply_linear points xs in
            pts @ acc
        | Newton window_size ->
            let pts = apply_newton ~window_size points xs in
            pts @ acc
      in
      List.rev (List.fold_left add_for_algo [] cfg.algorithms)
