type config = Conf.config

val default_config : config
val create_config  : unit -> config

module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
  val newline : string
end

module type S = sig
  type key

  val wfc1 : config -> key list            -> key option list
  val wfc2 : config -> key list list       -> key option list list
  val wfc3 : config -> key list list list  -> key option list list list
  
  val show1 : key list -> unit
  val show2 : key list list -> unit
  val show3 : key list list list -> unit

  val show1_opt : key option list -> unit
  val show2_opt : key option list list -> unit
  val show3_opt : key option list list list -> unit
end

module Make(Key : OrderedStringableType) : S with type key = Key.t
