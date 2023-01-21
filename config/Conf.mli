
type config = bool * int * (bool * bool) * int  * int list

val create_config : unit -> config

val validate_config : config -> bool
