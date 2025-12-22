open My_lab3_lib
open Interp

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

module Linear_stream = Stream.Make_stream (Stream.Linear_algo)

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

  let newton_n = 
    match List.find_opt (function Cli.A_newton _ -> true | _ -> false) algos with
    | Some (Cli.A_newton n) -> n
    | _ -> 4
  in
  
  let module Newton_stream =
    Stream.Make_stream (Stream.Newton_algo (struct let n = newton_n end))
  in
  
  let lst0 = Linear_stream.init ~step:step in
  let nst0 = Newton_stream.init ~step in

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
