open My_lab3_lib
open Interp

let eps = 1e-12

(* заменяет разделители на пробел для единого парсинга *)
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
let parse_point (line : string) : Interp.point option =
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

(* печать результата в формате algorithm x y *)
let print_point (algo : string) (p : Interp.point) =
  Printf.printf "%s: %.10g %.10g\n%!" algo p.x p.y

module Linear_stream = struct
  type state = {
    step : float;
    prev : Interp.point option;
    prev2 : Interp.point option;
    next_x : float option;
  }

  let init ~step = { step; prev = None; prev2 = None; next_x = None }

  (* вычисляет точки на отрезке от next_x до b.x включительно *)
  let emit_segment step (a : Interp.point) (b : Interp.point) (next_x : float) :
      Interp.point list * float =
    let rec loop acc x =
      if x > b.x +. eps then (List.rev acc, x)
      else
        let y = Interp.Linear.interpolate_between a b x in
        loop ({ x; y } :: acc) (x +. step)
    in
    loop [] next_x

  (* обновляет состояние при получении новой точки *)
  let on_point st (p : Interp.point) : state * Interp.point list =
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
  let on_eof st : Interp.point list =
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
    window : Interp.point list;
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

  let last_point (pts : Interp.point list) : Interp.point option =
    let rec loop = function
      | [] -> None
      | [ p ] -> Some p
      | _ :: ps -> loop ps
    in
    loop pts

  let nth_point_x (pts : Interp.point list) (i : int) : float option =
    let rec loop k = function
      | [] -> None
      | p :: ps -> if k = 0 then Some p.x else loop (k - 1) ps
    in
    loop i pts

  (* вычисляет значения на сетке от nx до stop_x включительно *)
  let emit_until (win : Interp.point list) step nx stop_x :
      Interp.point list * float =
    let rec loop acc x =
      if x > stop_x +. eps then (List.rev acc, x)
      else
        let y = Interp.Newton.interpolate_at win x in
        loop ({ x; y } :: acc) (x +. step)
    in
    loop [] nx

  (* выдача значений идет до центра окна для повышения устойчивости *)
  let on_point st (p : Interp.point) : state * Interp.point list =
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
  let on_eof st : Interp.point list =
    match (st.next_x, last_point st.window) with
    | Some nx, Some last ->
        if List.length st.window < 2 then []
        else
          let pts, _ = emit_until st.window st.step nx last.x in
          pts
    | _ -> []
end

type selected = A_linear | A_newton of int

let () =
  let use_linear = ref false in
  let use_newton = ref false in
  let newton_n = ref 4 in
  let step = ref 1.0 in
  let speclist =
    [
      ("--linear", Arg.Set use_linear, "use linear interpolation");
      ("--newton", Arg.Set use_newton, "use newton interpolation");
      ("-n", Arg.Set_int newton_n, "newton window size");
      ("--step", Arg.Set_float step, "output grid step");
    ]
  in
  let usage = "my_lab3 [--linear] [--newton -n N] --step S" in
  Arg.parse speclist (fun _ -> ()) usage;

  if !step <= 0.0 then invalid_arg "step must be positive";

  (* если алгоритмы не выбраны то используется linear *)
  let algos =
    let a =
      ([] |> fun acc -> if !use_linear then A_linear :: acc else acc)
      |> fun acc -> if !use_newton then A_newton !newton_n :: acc else acc
    in
    if a = [] then [ A_linear ] else List.rev a
  in

  let linear_state = ref (Linear_stream.init ~step:!step) in
  let newton_state = ref (Newton_stream.init ~step:!step ~n:!newton_n) in

  (* обработка одной входной точки и немедленная печать результата *)
  let feed_point (p : Interp.point) =
    List.iter
      (function
        | A_linear ->
            let st', out = Linear_stream.on_point !linear_state p in
            linear_state := st';
            List.iter (print_point "linear") out
        | A_newton _ ->
            let st', out = Newton_stream.on_point !newton_state p in
            newton_state := st';
            List.iter (print_point "newton") out)
      algos
  in

  (* потоковое чтение stdin и вычисление по мере поступления данных *)
  (try
     while true do
       let line = input_line stdin in
       match parse_point line with None -> () | Some p -> feed_point p
     done
   with End_of_file -> ());

  (* финальный вывод после завершения входного потока *)
  List.iter
    (function
      | A_linear ->
          Linear_stream.on_eof !linear_state |> List.iter (print_point "linear")
      | A_newton _ ->
          Newton_stream.on_eof !newton_state |> List.iter (print_point "newton"))
    algos
