
(* config type *)
type config = bool * int * (bool * bool) * int  * int list

(* initial config *)
val default : config

(* config repl *)
val create_config : unit -> config

(* checks if config is configured properly *)
val validate_config : config -> bool
