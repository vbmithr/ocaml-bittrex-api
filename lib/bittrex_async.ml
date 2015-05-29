open Core.Std
open Async.Std
open Cohttp_async
open Bittrex

let try_with_convert f =
  try_with f >>| function
  | Ok r -> `Ok r
  | Error exn -> `Error (Exn.to_string exn)

module Bitfinex = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let base_uri = "https://api.bitfinex.com/v1/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint params =
      let f () =
        let uri = mk_uri endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
  include Bitfinex(H)
end

module BTCE = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let version = "3"
    let base_uri = "https://btc-e.com/api/" ^ version ^ "/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint _ =
      let f () =
        let uri = mk_uri endpoint in
        Client.get uri >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
  include BTCE(H)
end

module Bittrex = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let version = "v1.1"
    let base_uri = "https://bittrex.com/api/" ^ version ^ "/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint params =
      let f () =
        let uri = mk_uri endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
end

module Cryptsy = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let version = "v2"
    let base_uri = "https://api.cryptsy.com/api/" ^ version ^ "/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint params =
      let f () =
        let uri = mk_uri endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
end

module Poloniex = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let base_uri = "https://poloniex.com/public"

    let get _ params =
      let f () =
        Client.get Uri.(with_query' (Uri.of_string base_uri) params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
end

module Kraken = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let version = "0"
    let base_uri = "https://api.kraken.com/" ^ version ^ "/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint params =
      let f () =
        let uri = mk_uri endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
  include Kraken(H)
end

module Hitbtc = struct
  module H = struct
    include Cohttp_async_io
    include Deferred.Infix

    let version = "1"
    let base_uri = "https://api.hitbtc.com/api/" ^ version ^ "/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint params =
      let f () =
        let uri = mk_uri endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
end
