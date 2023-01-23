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
end

module Make(Key : OrderedStringableType) : S with type key = Key.t
