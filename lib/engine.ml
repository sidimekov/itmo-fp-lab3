open Interp

type algorithm = Linear | Newton of int
type config = { algorithms : algorithm list; step : float }
type labeled_point = { algo : string; point : point }

let x_grid ~start ~stop ~step =
  if step <= 0.0 then invalid_arg "x_grid: step must be positive";
  let eps = 1e-12 in
  let rec loop acc x =
    if x > stop +. eps then List.rev acc else loop (x :: acc) (x +. step)
  in
  loop [] start

let apply_linear points xs : labeled_point list =
  let f x =
    let left, right = Linear.find_segment points x in
    let y = Linear.interpolate_between left right x in
    { algo = "linear"; point = { x; y } }
  in
  List.map f xs

let take n lst =
  let rec loop k acc = function
    | [] -> List.rev acc
    | _ when k <= 0 -> List.rev acc
    | x :: xs -> loop (k - 1) (x :: acc) xs
  in
  loop n [] lst

let apply_newton ~window_size points xs : labeled_point list =
  if window_size <= 1 then invalid_arg "apply_newton: window_size must be > 1";
  let pts =
    if List.length points <= window_size then points
    else take window_size points
  in
  let f x =
    let y = Newton.interpolate_at pts x in
    { algo = "newton"; point = { x; y } }
  in
  List.map f xs

let run_offline (cfg : config) (points : point list) : labeled_point list =
  match points with
  | [] | [ _ ] -> []
  | p0 :: _ ->
      let last_x =
        List.fold_left (fun acc p -> if p.x > acc then p.x else acc) p0.x points
      in
      let xs = x_grid ~start:p0.x ~stop:last_x ~step:cfg.step in
      let add_for_algo acc algo =
        match algo with
        | Linear -> apply_linear points xs @ acc
        | Newton n -> apply_newton ~window_size:n points xs @ acc
      in
      List.rev (List.fold_left add_for_algo [] cfg.algorithms)
