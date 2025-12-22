type point = { x : float; y : float }

module Linear = struct
  let interpolate_between (p1 : point) (p2 : point) (x : float) : float =
    let dx = p2.x -. p1.x in
    if dx = 0.0 then invalid_arg "linear.interpolate_between: duplicate x"
    else
      let t = (x -. p1.x) /. dx in
      p1.y +. (t *. (p2.y -. p1.y))

  let find_segment (points : point list) (x : float) : point * point =
    match points with
    | [] | [ _ ] -> invalid_arg "linear.find_segment: need at least two points"
    | p0 :: p1 :: rest ->
        if x <= p0.x then (p0, p1)
        else
          let rec loop prev curr = function
            | [] -> (prev, curr)
            | q :: qs -> if x <= q.x then (curr, q) else loop curr q qs
          in
          loop p0 p1 rest

  let interpolate_on_uniform_grid (points : point list) ~(step : float) :
      point list =
    match points with
    | [] | [ _ ] -> []
    | p0 :: _ as pts ->
        if step <= 0.0 then
          invalid_arg
            "linear.interpolate_on_uniform_grid: step must be positive";
        let min_x = p0.x in
        let max_x =
          List.fold_left (fun acc p -> if p.x > acc then p.x else acc) p0.x pts
        in
        let eps = 1e-12 in
        let rec loop acc x =
          if x > max_x +. eps then List.rev acc
          else
            let left, right = find_segment pts x in
            let y = interpolate_between left right x in
            loop ({ x; y } :: acc) (x +. step)
        in
        loop [] min_x
end

module Newton = struct
  let build_coefficients (points : point list) : float array * float array =
    let n = List.length points in
    if n = 0 then invalid_arg "newton.build_coefficients: empty points";
    let xs = Array.make n 0.0 in
    let coeffs = Array.make n 0.0 in
    let () =
      let rec fill i = function
        | [] -> ()
        | p :: ps ->
            xs.(i) <- p.x;
            coeffs.(i) <- p.y;
            fill (i + 1) ps
      in
      fill 0 points
    in
    for j = 1 to n - 1 do
      for i = n - 1 downto j do
        let num = coeffs.(i) -. coeffs.(i - 1) in
        let den = xs.(i) -. xs.(i - j) in
        if den = 0.0 then invalid_arg "newton.build_coefficients: duplicate x"
        else coeffs.(i) <- num /. den
      done
    done;
    (xs, coeffs)

  let evaluate (xs : float array) (coeffs : float array) (x : float) : float =
    let n = Array.length coeffs in
    if n = 0 then invalid_arg "newton.evaluate: empty coefficients";
    let acc = ref coeffs.(n - 1) in
    for i = n - 2 downto 0 do
      acc := (!acc *. (x -. xs.(i))) +. coeffs.(i)
    done;
    !acc

  let interpolate_at (points : point list) (x : float) : float =
    let xs, coeffs = build_coefficients points in
    evaluate xs coeffs x
end
