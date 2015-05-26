open Bittrex_intf

module Bitfinex (H: HTTP_CLIENT) :
  EXCHANGE with type pair = [`BTCUSD | `LTCBTC]
            and type 'a io = 'a H.t

module BTCE (H: HTTP_CLIENT) :
  EXCHANGE with type pair = [`BTCUSD | `LTCBTC]
            and type 'a io = 'a H.t
