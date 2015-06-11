open Core.Std
open Async.Std
open Cohttp_async
open Mt
open Bittrex

let try_with_convert f =
  try_with f >>| function
  | Ok r -> `Ok r
  | Error exn -> `Error (Exn.to_string exn)

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
  include Bitfinex(H)
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
  include BTCE(H)
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
  include Kraken(H)
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

module Generic = struct
  include AsyncIO
  type ticker = (int64, int64) Ticker.tvwap
  type book_entry = int64 Mt.Tick.t
  type trade = (int64, int64) Mt.Tick.tdts

  let ticker c = function
    | "bitfinex" ->
      Bitfinex.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> ticker c)
    | "btce" ->
      BTCE.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> ticker c)
    | "kraken" ->
      Kraken.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> ticker c)
    | _ -> return @@ `Error "unsupported"

  let book c = function
    | "bitfinex" ->
      Bitfinex.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> book c)
    | "btce" ->
      BTCE.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> book c)
    | "kraken" ->
      (Kraken.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> book c)
       :> (book_entry OrderBook.t, string) CCError.t Deferred.t)
    | _ -> return @@ `Error "unsupported"

  let trades ?since ?limit c = function
    | "bitfinex" ->
      Bitfinex.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> trades ?since ?limit c)
    | "btce" ->
      BTCE.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> trades ?since ?limit c)
    | "kraken" ->
      (Kraken.(accept c |> function
        | None -> return @@ `Error "unsupported"
        | Some c -> trades ?since ?limit c)
      :> (trade list, string) CCError.t Deferred.t)
    | _ -> return @@ `Error "unsupported"
end
