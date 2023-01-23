type config = Conf.config

module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
  val newline : string
end

module type S = sig
  type key

  val wfc1 : config -> (key option list           -> unit) -> key list            -> key option list
  val wfc2 : config -> (key option list list      -> unit) -> key list list       -> key option list list
  val wfc3 : config -> (key option list list list -> unit) -> key list list list  -> key option list list list
end

module Make(Key : OrderedStringableType) : S with type key = Key.t
