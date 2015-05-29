open Bittrex_intf

module Bitfinex (H: HTTP_CLIENT) : EXCHANGE
  with type 'a io = 'a H.t
   and type pair = [`BTCUSD | `LTCBTC]
   and type ticker = (int64, int64) Mt.ticker_with_vwap
   and type book_entry = int64 Mt.tick
   and type trade = (int64, int64) Mt.tick_with_direction_ts

module BTCE (H: HTTP_CLIENT) : EXCHANGE
  with type 'a io = 'a H.t
   and type pair = [`BTCUSD | `LTCBTC]
   and type ticker = (int64, int64) Mt.ticker_with_vwap
   and type book_entry = int64 Mt.tick
   and type trade = (int64, int64) Mt.tick_with_direction_ts

module Kraken (H: HTTP_CLIENT) : EXCHANGE
  with type 'a io = 'a H.t
   and type pair = [`BTCUSD | `BTCLTC]
   and type ticker = (int64, int64) Mt.ticker_with_vwap
   and type book_entry = (int64, int64) Mt.tick_with_timestamp
   and type trade = < d : [ `Ask | `Bid | `Unset ];
                      kind : [ `Limit | `Market | `Unset ]; misc : string;
                      p : int64; ts : int64; v : int64 >
