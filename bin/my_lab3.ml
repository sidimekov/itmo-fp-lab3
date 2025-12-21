open My_lab3_lib
open Interp

let eps = 1e-12

let normalize_separators (s : string) : string =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with ';' | ',' | '\t' -> Bytes.set b i ' ' | _ -> ()
  done;
  Bytes.to_string b

(* разбиение строки на токены x и y *)
let split_fields (s : string) : string list =
  s |> normalize_separators |> String.split_on_char ' '
  |> List.filter (fun t -> String.length t > 0)

(* парсит одну строку в точку допускает пустые строки *)
let parse_point (line : string) : point option =
  let trimmed = String.trim line in
  if trimmed = "" then None
  else if String.length trimmed > 0 && trimmed.[0] = '#' then None
  else
    match split_fields trimmed with
    | [ sx; sy ] -> (
        try
          let x = float_of_string sx in
          let y = float_of_string sy in
          Some { x; y }
        with Failure _ -> None)
    | _ -> None

(* печать результата в формате алгоритм x y *)
let print_point (algo : string) (p : point) =
  Printf.printf "%s: %.10g %.10g\n%!" algo p.x p.y

module Linear_stream = struct
  type state = {
    step : float;
    prev : point option;
    prev2 : point option;
    next_x : float option;
  }

  let init ~step = { step; prev = None; prev2 = None; next_x = None }

  (* вычисляет точки на отрезке от next_x до b.x включительно *)
  let emit_segment step (a : point) (b : point) (next_x : float) :
      point list * float =
    let rec loop acc x =
      if x > b.x +. eps then (List.rev acc, x)
      else
        let y = Linear.interpolate_between a b x in
        loop ({ x; y } :: acc) (x +. step)
    in
    loop [] next_x

  (* обновляет состояние при получении новой точки *)
  let on_point st (p : point) : state * point list =
    match (st.prev, st.next_x) with
    | None, _ ->
        let nx = Some p.x in
        ({ st with prev = Some p; prev2 = None; next_x = nx }, [])
    | Some prev, Some nx ->
        let pts, nx' = emit_segment st.step prev p nx in
        ({ st with prev2 = Some prev; prev = Some p; next_x = Some nx' }, pts)
    | Some prev, None ->
        let pts, nx' = emit_segment st.step prev p prev.x in
        ({ st with prev2 = Some prev; prev = Some p; next_x = Some nx' }, pts)

  (* финальный вывод для последнего сегмента при eof *)
  let on_eof st : point list =
    match (st.prev2, st.prev, st.next_x) with
    | Some a, Some b, Some nx ->
        let pts, _ = emit_segment st.step a b nx in
        pts
    | _ -> []
end

module Newton_stream = struct
  type state = {
    step : float;
    n : int;
    window : point list;
    next_x : float option;
  }

  let init ~step ~n =
    if n <= 1 then invalid_arg "newton: n must be > 1";
    { step; n; window = []; next_x = None }

  (* добавляет точку и обрезает окно до n элементов *)
  let append_trim n (win : 'a list) (x : 'a) : 'a list =
    let win' = win @ [ x ] in
    let len = List.length win' in
    if len <= n then win'
    else
      let drop = len - n in
      let rec drop_k k = function
        | [] -> []
        | ys when k <= 0 -> ys
        | _ :: ys -> drop_k (k - 1) ys
      in
      drop_k drop win'

  let last_point (pts : point list) : point option =
    let rec loop = function
      | [] -> None
      | [ p ] -> Some p
      | _ :: ps -> loop ps
    in
    loop pts

  let nth_point_x (pts : point list) (i : int) : float option =
    let rec loop k = function
      | [] -> None
      | p :: ps -> if k = 0 then Some p.x else loop (k - 1) ps
    in
    loop i pts

  (* вычисляет значения на сетке от nx до stop_x включительно *)
  let emit_until (win : point list) step nx stop_x : point list * float =
    let rec loop acc x =
      if x > stop_x +. eps then (List.rev acc, x)
      else
        let y = Newton.interpolate_at win x in
        loop ({ x; y } :: acc) (x +. step)
    in
    loop [] nx

  (* выдача значений идет до центра окна для повышения устойчивости *)
  let on_point st (p : point) : state * point list =
    let win = append_trim st.n st.window p in
    let len = List.length win in
    let nx =
      match st.next_x with
      | Some v -> v
      | None -> ( match win with p0 :: _ -> p0.x | [] -> 0.0)
    in
    if len < st.n then ({ st with window = win; next_x = Some nx }, [])
    else
      let center_i = (st.n - 1) / 2 in
      match nth_point_x win center_i with
      | None -> ({ st with window = win; next_x = Some nx }, [])
      | Some center_x ->
          let pts, nx' = emit_until win st.step nx center_x in
          ({ st with window = win; next_x = Some nx' }, pts)

  (* финальный вывод до последней точки при eof *)
  let on_eof st : point list =
    match (st.next_x, last_point st.window) with
    | Some nx, Some last ->
        if List.length st.window < 2 then []
        else
          let pts, _ = emit_until st.window st.step nx last.x in
          pts
    | _ -> []
end

let () =
  let cfg = Cli.parse Sys.argv in
  let step = cfg.step in
  let algos = cfg.algos in

  let has_linear =
    List.exists (function Cli.A_linear -> true | _ -> false) algos
  in
  let has_newton =
    List.exists (function Cli.A_newton _ -> true | _ -> false) algos
  in

  let lst0 = Linear_stream.init ~step:step in
  let newton_n = 
    match List.find_opt (function Cli.A_newton _ -> true | _ -> false) algos with
    | Some (Cli.A_newton n) -> n
    | _ -> 4
  in
  let nst0 = Newton_stream.init ~step:step ~n:newton_n in

  (* основной цикл без мутабельности состояния *)
  let rec loop (lst : Linear_stream.state) (nst : Newton_stream.state) =
    match input_line stdin with
    | line -> (
        match parse_point line with
        | None -> loop lst nst
        | Some p ->
            let lst', out_l =
              if has_linear then Linear_stream.on_point lst p else (lst, [])
            in
            List.iter (print_point "linear") out_l;

            let nst', out_n =
              if has_newton then Newton_stream.on_point nst p else (nst, [])
            in
            List.iter (print_point "newton") out_n;

            loop lst' nst')
    | exception End_of_file ->
        (* финальный вывод после завершения входного потока *)
        if has_linear then
          Linear_stream.on_eof lst |> List.iter (print_point "linear");
        if has_newton then
          Newton_stream.on_eof nst |> List.iter (print_point "newton")
  in

  loop lst0 nst0
