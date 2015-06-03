open Core.Std
open Async.Std
open Cohttp_async
open Bittrex

let try_with_convert f =
  try_with f >>| function
  | Ok r -> `Ok r
  | Error exn -> `Error (Exn.to_string exn)

module Make_with_obj (E: Bittrex_intf.EXCHANGE_SIMPLE) = struct
  include E

  let all_trades ?since ?limit () =
    let map_f p =
      trades ?since ?limit p >>= fun ts ->
      return @@ CCError.map (fun ts -> p, ts) ts
    in
    let ts = List.map ~f:map_f pairs in
    all ts >>= fun ts ->
    return @@ CCError.map_l (fun a -> a) ts

  class exchange =
    object
      method name : string = name
      method pairs : pair list = pairs
      method ticker : pair -> (ticker, string) CCError.t t = ticker
      method book : pair -> (book_entry Mt.orderbook, string) CCError.t t = book
      method trades : ?since:int64 -> ?limit:int ->
        pair -> (trade list, string) CCError.t t = trades
      method all_trades : ?since:int64 -> ?limit:int -> unit ->
        ((E.pair * trade list) list, string) CCError.t t = all_trades
    end

  let exchange = new exchange
end

module AsyncIO = struct
  include Cohttp_async_io
  include Deferred.Infix
  let all = Deferred.all
end

module Bitfinex = struct
  module H = struct
    include AsyncIO

    let base_uri = "https://api.bitfinex.com/v1/"
    let mk_uri section = Uri.of_string @@ base_uri ^ section

    let get endpoint params =
      let f () =
        let uri = mk_uri endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      try_with_convert f
  end
  include Make_with_obj(Bitfinex(H))
end

module BTCE = struct
  module H = struct
    include AsyncIO

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
  include Make_with_obj(BTCE(H))
end

module Bittrex = struct
  module H = struct
    include AsyncIO

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
    include AsyncIO

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
    include AsyncIO

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
    include AsyncIO

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
  include Make_with_obj(Kraken(H))
end

module Hitbtc = struct
  module H = struct
    include AsyncIO

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
