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

module Minimum = struct
  module type S = EXCHANGE
    with type pair = [`BTCUSD | `LTCBTC]
    and type ticker = (int64, int64) Mt.ticker_with_vwap
    and type book_entry = int64 Mt.tick
    and type trade = (int64, int64) Mt.tick_with_direction_ts
end

module Bitfinex = struct
  module type S = Minimum.S
end

module BTCE = struct
  module type S = Minimum.S
end

module Kraken = struct
  module type S = EXCHANGE
    with type pair = [`BTCUSD | `BTCLTC]
     and type ticker = (int64, int64) Mt.ticker_with_vwap
     and type book_entry = (int64, int64) Mt.tick_with_timestamp
     and type trade = < d : [ `Ask | `Bid | `Unset ];
                        kind : [ `Limit | `Market | `Unset ]; misc : string;
                        p : int64; ts : int64; v : int64 >
end
