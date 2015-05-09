open Async.Std

module type ASYNC_HTTP_CLIENT = sig
  include Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Reader.t
     and type oc = Writer.t

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t

  (* val post : Bitstamp.Credentials.t -> string -> (string * string) list -> *)
  (*   (string -> [< `Error of string | `Ok of 'a ]) -> 'a t *)
end

module Bitfinex : ASYNC_HTTP_CLIENT
module Bittrex : ASYNC_HTTP_CLIENT
module Cryptsy : ASYNC_HTTP_CLIENT
module BTCE : ASYNC_HTTP_CLIENT

