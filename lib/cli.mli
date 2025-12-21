type selected = A_linear | A_newton of int
type config = { algos : selected list; step : float }

val parse : string array -> config
