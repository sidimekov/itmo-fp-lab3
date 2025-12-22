open Interp

let eps = 1e-12

module type STREAM_ALGO = sig
  val name : string
  val window_size : int

  (* обрезает окно до нужного размера *)
  val update_window : point list -> point -> point list

  (* вычисляет y по текущему окну и x *)
  val eval : point list -> float -> float

  (* до какого x печатать после получения очередной точки *)
  val stop_x_on_point : point list -> float option

  (* до какого x печатать при eof *)
  val stop_x_on_eof : point list -> float option
end

module Make_stream (A : STREAM_ALGO) = struct
  type state = { step : float; window : point list; next_x : float option }

  let init ~step = { step; window = []; next_x = None }

  let emit_until (win : point list) step nx stop_x : point list * float =
    let rec loop acc x =
      if x > stop_x +. eps then (List.rev acc, x)
      else
        let y = A.eval win x in
        loop ({ x; y } :: acc) (x +. step)
    in
    loop [] nx

  let on_point st (p : point) : state * point list =
    let win = A.update_window st.window p in
    let nx =
      match st.next_x with
      | Some v -> v
      | None -> ( match win with p0 :: _ -> p0.x | [] -> 0.0)
    in
    if List.length win < A.window_size then
      ({ st with window = win; next_x = Some nx }, [])
    else
      match A.stop_x_on_point win with
      | None -> ({ st with window = win; next_x = Some nx }, [])
      | Some stop_x ->
          let pts, nx' = emit_until win st.step nx stop_x in
          ({ st with window = win; next_x = Some nx' }, pts)

  let on_eof st : point list =
    match (st.next_x, A.stop_x_on_eof st.window) with
    | Some nx, Some stop_x ->
        if List.length st.window < 2 then []
        else
          let pts, _ = emit_until st.window st.step nx stop_x in
          pts
    | _ -> []
end

let drop_k k lst =
  let rec loop n = function
    | [] -> []
    | xs when n <= 0 -> xs
    | _ :: xs -> loop (n - 1) xs
  in
  loop k lst

let append_trim n (win : 'a list) (x : 'a) : 'a list =
  let win' = win @ [ x ] in
  let len = List.length win' in
  if len <= n then win'
  else
    let drop = len - n in
    drop_k drop win'

let nth_point_x (pts : point list) (i : int) : float option =
  let rec loop k = function
    | [] -> None
    | p :: ps -> if k = 0 then Some p.x else loop (k - 1) ps
  in
  loop i pts

let last_point (pts : point list) : point option =
  let rec loop = function [] -> None | [ p ] -> Some p | _ :: ps -> loop ps in
  loop pts

module Linear_algo : STREAM_ALGO = struct
  let name = "linear"
  let window_size = 2

  let update_window win p =
    match win with
    | [] -> [ p ]
    | [ a ] -> [ a; p ]
    | [ _a; b ] -> [ b; p ]
    | _ -> [ p ]

  let eval win x =
    match win with
    | [ a; b ] -> Linear.interpolate_between a b x
    | _ -> invalid_arg "linear_algo.eval: need 2 points"

  let stop_x_on_point win = match win with [ _a; b ] -> Some b.x | _ -> None
  let stop_x_on_eof win = stop_x_on_point win
end

module Newton_algo (N : sig
  val n : int
end) : STREAM_ALGO = struct
  let name = "newton"
  let window_size = N.n
  let update_window win p = append_trim N.n win p
  let eval win x = Newton.interpolate_at win x

  let stop_x_on_point win =
    if List.length win < N.n then None
    else
      let center_i = (N.n - 1) / 2 in
      nth_point_x win center_i

  let stop_x_on_eof win =
    match last_point win with None -> None | Some p -> Some p.x
end
