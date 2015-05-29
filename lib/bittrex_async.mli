open Bittrex_intf
open Async.Std

module Bitfinex : Bitfinex.S with type 'a io := 'a Deferred.t
module BTCE : BTCE.S with type 'a io := 'a Deferred.t
module Kraken : Kraken.S with type 'a io := 'a Deferred.t
