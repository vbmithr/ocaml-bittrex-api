open Async.Std

module type ASYNC_HTTP_CLIENT = sig
  include Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Reader.t
     and type oc = Writer.t

  val get : string -> (string * string) list -> string t
end

module Bitfinex : ASYNC_HTTP_CLIENT
module Bittrex : ASYNC_HTTP_CLIENT
module Cryptsy : ASYNC_HTTP_CLIENT
module BTCE : ASYNC_HTTP_CLIENT
module Poloniex : ASYNC_HTTP_CLIENT
module Kraken : ASYNC_HTTP_CLIENT
module Hitbtc : ASYNC_HTTP_CLIENT
