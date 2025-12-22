type selected = A_linear | A_newton of int

type cli = {
  use_linear : bool;
  use_newton : bool;
  newton_n : int;
  step : float option;
}

type config = { algos : selected list; step : float }

let default_cli =
  { use_linear = false; use_newton = false; newton_n = 4; step = None }

let usage () =
  prerr_endline "usage: my_lab3 [--linear] [--newton -n N] --step S";
  exit 2

let parse_int s =
  try int_of_string s
  with Failure _ ->
    prerr_endline "error: expected int";
    usage ()

let parse_float s =
  try float_of_string s
  with Failure _ ->
    prerr_endline "error: expected float";
    usage ()

let rec parse_args (cfg : cli) (args : string list) : cli =
  match args with
  | [] -> cfg
  | "--linear" :: rest -> parse_args { cfg with use_linear = true } rest
  | "--newton" :: rest -> parse_args { cfg with use_newton = true } rest
  | "-n" :: n :: rest -> parse_args { cfg with newton_n = parse_int n } rest
  | "--step" :: s :: rest ->
      parse_args { cfg with step = Some (parse_float s) } rest
  | "--help" :: _ -> usage ()
  | x :: _ ->
      prerr_endline ("error: unknown argument " ^ x);
      usage ()

let finalize (cfg : cli) : config =
  let step =
    match cfg.step with
    | None ->
        prerr_endline "error: --step is required";
        usage ()
    | Some s -> s
  in
  if step <= 0.0 then (
    prerr_endline "error: step must be positive";
    usage ());
  let algos =
    let a =
      [] |> fun acc ->
      if cfg.use_linear then A_linear :: acc
      else
        acc |> fun acc ->
        if cfg.use_newton then A_newton cfg.newton_n :: acc else acc
    in
    if a = [] then [ A_linear ] else List.rev a
  in
  { algos; step }

let parse (argv : string array) : config =
  let args = argv |> Array.to_list |> List.tl in
  parse_args default_cli args |> finalize
