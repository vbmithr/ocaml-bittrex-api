open Bittrex_intf
open Async.Std

module type ASYNC_EXCHANGE =
  Bittrex_intf.EXCHANGE with type 'a io = 'a Deferred.t

module Bitfinex : ASYNC_EXCHANGE with type pair = [`BTCUSD | `LTCBTC]
module BTCE : ASYNC_EXCHANGE with type pair = [`BTCUSD | `LTCBTC]
