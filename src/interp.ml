type point = {
  x : float;
  y : float;
}

module Linear = struct
  (* линейная интерполяция между двумя точками
     предполагается что x лежит между p1.x и p2.x *)
  let interpolate_between (p1 : point) (p2 : point) (x : float) : float =
    let dx = p2.x -. p1.x in
    if dx = 0.0 then
      invalid_arg "linear.interpolate_between: duplicate x coordinates"
    else
      let t = (x -. p1.x) /. dx in
      p1.y +. t *. (p2.y -. p1.y)

  (* поиск отрезка [p_left; p_right] для заданного x в отсортированном списке точек
     если x левее всех или правее всех, возвращаем крайние отрезки *)
  let find_segment (points : point list) (x : float) : point * point =
    match points with
    | [] | [_] ->
        invalid_arg "linear.find_segment: need at least two points"
    | p0 :: p1 :: _ as pts ->
        let rec aux prev rest =
          match prev, rest with
          | _, [] ->
              (* x правее всех точек - берем последние две *)
              let rec last_two acc = function
                | [] -> acc
                | q :: qs ->
                    let acc' =
                      match acc with
                      | None -> Some q
                      | Some r -> Some r
                    in
                    last_two acc' qs
              in
              let rec collect_last2 a b = function
                | [] -> (a, b)
                | q :: qs -> collect_last2 b q qs
              in
              let a, b =
                match pts with
                | r1 :: r2 :: rs -> collect_last2 r1 r2 rs
                | _ -> assert false
              in
              (a, b)
          | None, q :: qs ->
              if x <= q.x then (p0, q) else aux (Some q) qs
          | Some p, q :: qs ->
              if x <= q.x then (p, q) else aux (Some q) qs
        in
        aux None pts

  (* интерполяция на равномерной сетке
     points - исходные точки, должны быть отсортированы по x по возрастанию.
     step - шаг по x.
     результат - список точек от минимального x до максимального x включительно *)
  let interpolate_on_uniform_grid (points : point list) ~(step : float) :
      point list =
    match points with
    | [] | [_] -> []
    | p0 :: _ as pts ->
        if step <= 0.0 then
          invalid_arg "linear.interpolate_on_uniform_grid: step must be positive";
        let min_x = p0.x in
        let max_x =
          List.fold_left (fun acc p -> if p.x > acc then p.x else acc) p0.x pts
        in
        let rec loop acc x =
          if x > max_x then List.rev acc
          else
            let left, right = find_segment pts x in
            let y = interpolate_between left right x in
            let p = { x; y } in
            loop (p :: acc) (x +. step)
        in
        loop [] min_x
end

module Newton = struct
  (* построение массивов x и коэффициентов Ньютона по списку точек *)
  let build_coefficients (points : point list) :
      float array * float array =
    let n = List.length points in
    if n = 0 then
      invalid_arg "newton.build_coefficients: empty points list";
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
    (* схема разделенных разностей inplace *)
    for j = 1 to n - 1 do
      for i = n - 1 downto j do
        let num = coeffs.(i) -. coeffs.(i - 1) in
        let den = xs.(i) -. xs.(i - j) in
        if den = 0.0 then
          invalid_arg "newton.build_coefficients: duplicate x coordinates"
        else
          coeffs.(i) <- num /. den
      done
    done;
    (xs, coeffs)

  (* вычисление значения полинома ньютона в точке x *)
  let evaluate (xs : float array) (coeffs : float array) (x : float) : float =
    let n = Array.length coeffs in
    if n = 0 then invalid_arg "newton.evaluate: empty coefficients"
    else
      let acc = ref coeffs.(n - 1) in
      for i = n - 2 downto 0 do
        acc := !acc *. (x -. xs.(i)) +. coeffs.(i)
      done;
      !acc

  (* по списку точек и нужному x считает значение *)
  let interpolate_at (points : point list) (x : float) : float =
    let xs, coeffs = build_coefficients points in
    evaluate xs coeffs x
end
