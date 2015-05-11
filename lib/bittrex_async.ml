open Async.Std
open Cohttp_async
open Bittrex

module type ASYNC_HTTP_CLIENT = sig
  include Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Reader.t
     and type oc = Writer.t

  val get : string -> (string * string) list -> string t
end

module Bitfinex = struct
  include Cohttp_async_io

  let base_uri = "https://api.bitfinex.com/v1/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint params =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body
end

module BTCE = struct
  include Cohttp_async_io

  let version = "3"
  let base_uri = "https://btc-e.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint _ =
    let uri = mk_uri endpoint in
    Client.get uri >>= fun (resp, body) ->
    Body.to_string body
end

module Bittrex = struct
  include Cohttp_async_io

  let version = "v1.1"
  let base_uri = "https://bittrex.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint params =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body
end

module Cryptsy = struct
  include Cohttp_async_io

  let version = "v2"
  let base_uri = "https://api.cryptsy.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint params =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body
end

module Poloniex = struct
  include Cohttp_async_io

  let base_uri = "https://poloniex.com/public"

  let get endpoint params =
    Client.get Uri.(with_query' (Uri.of_string base_uri) params) >>= fun (resp, body) ->
    Body.to_string body
end

module Kraken = struct
  include Cohttp_async_io

  let version = "0"
  let base_uri = "https://api.kraken.com/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint params =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body
end

module Hitbtc = struct
  include Cohttp_async_io

  let version = "1"
  let base_uri = "https://api.hitbtc.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint params =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body
end
