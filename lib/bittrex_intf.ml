module type HTTP_CLIENT = sig
  include Cohttp.S.IO
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  val get : string -> (string * string) list -> [`Ok of string | `Error of string] t
end

module type EXCHANGE = sig
  type 'a io
  type pair
  type ticker
  type book_entry
  type trade

  val name : string
  val pairs : pair list

  val ticker : pair ->
    [`Ok of ticker | `Error of string] io

  val book : pair ->
    [`Ok of book_entry Mt.orderbook | `Error of string] io

  val trades : ?since:int64 -> ?limit:int -> pair ->
    [`Ok of trade list | `Error of string] io
end
