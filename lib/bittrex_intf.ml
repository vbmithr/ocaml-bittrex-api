module type IO = sig
  include Cohttp.S.IO
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val all : 'a t list -> 'a list t
end

module type HTTP_CLIENT = sig
  include IO

  val get : string -> (string * string) list -> [`Ok of string | `Error of string] t
end

module type EXCHANGE_SIMPLE = sig
  include IO
  type pair
  type ticker
  type book_entry
  type trade

  val name : string
  val pairs : pair list

  val ticker : pair ->
    [`Ok of ticker | `Error of string] t

  val book : pair ->
    [`Ok of book_entry Mt.orderbook | `Error of string] t

  val trades : ?since:int64 -> ?limit:int -> pair ->
    [`Ok of trade list | `Error of string] t
    (** [trades ?since ?limit pair] is a thread that returns a list of
        trades contained in an error monad. If the underlying exchange
        supports it, the list will be limited to [limit] entries, and
        will not contain trades older than [since], where [since] is
        an unix timestamp in nanoseconds. *)
end

module type EXCHANGE = sig
  include EXCHANGE_SIMPLE

  class exchange : object
    method name : string
    method pairs : pair list
    method ticker : pair -> [`Ok of ticker | `Error of string] t
    method book : pair -> [`Ok of book_entry Mt.orderbook | `Error of string] t
    method trades : ?since:int64 -> ?limit:int -> pair ->
      [`Ok of trade list | `Error of string] t
    method all_tickers : ((pair * ticker) list, string) CCError.t t
    method all_books :
      ((pair * book_entry Mt.orderbook) list, string) CCError.t t
    method all_trades : ?since:int64 -> ?limit:int -> unit ->
      [`Ok of (pair * trade list) list | `Error of string] t
  end
end

module Bitfinex = struct
  module type S = EXCHANGE
    with type pair = [`XBTUSD | `LTCXBT]
    and type ticker = (int64, int64) Mt.ticker_with_vwap
    and type book_entry = int64 Mt.tick
    and type trade = (int64, int64) Mt.tick_with_d_ts_ns
end

module BTCE = struct
  module type S = EXCHANGE
    with type pair = [`XBTUSD | `LTCXBT]
    and type ticker = (int64, int64) Mt.ticker_with_vwap
    and type book_entry = int64 Mt.tick
    and type trade = (int64, int64) Mt.tick_with_d_ts_ns
end

module Kraken = struct
  module type S = EXCHANGE
    with type pair = [`XBTUSD | `XBTLTC]
     and type ticker = (int64, int64) Mt.ticker_with_vwap
     and type book_entry = (int64, int64) Mt.tick_with_timestamp
     and type trade = < d : [ `Ask | `Bid | `Unset ];
                        kind : [ `Limit | `Market | `Unset ]; misc : string;
                        p : int64; ts : int64; ns : int64; v : int64 >
end
