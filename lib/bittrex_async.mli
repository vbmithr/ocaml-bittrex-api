open Async.Std

include module type of Bittrex_intf

(** Specific modules. *)

module Bitfinex : BITFINEX with type 'a t := 'a Deferred.t
module Bitstamp : BITSTAMP with type 'a t := 'a Deferred.t
module BTCE : BTCE with type 'a t := 'a Deferred.t
module Kraken : KRAKEN with type 'a t := 'a Deferred.t

(** Generic module. Use this when you need to use multiple exchanges
    dynamically, for example if you have a program that takes an
    exchange name as an argument. *)

module Generic : GENERIC with type 'a t := 'a Deferred.t
