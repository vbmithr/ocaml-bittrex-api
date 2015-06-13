open Rresult
open Core.Std
open Async.Std
open Cohttp_async
open Mt
open Bittrex

include Bittrex_intf

let trap_exn f =
  try_with f >>| function
  | Ok r -> Rresult.Ok r
  | Error exn -> Rresult.Error (`Exn_trap (exn, Caml.Printexc.get_raw_backtrace ()))

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
      trap_exn f
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
      trap_exn f
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
      trap_exn f
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
      trap_exn f
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
      trap_exn f
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
      trap_exn f
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
      trap_exn f
  end
end

module Generic = struct
  include AsyncIO
  type ticker = (int64, int64) Ticker.tvwap
  type book_entry = int64 Mt.Tick.t
  type trade = (int64, int64) Mt.Tick.tdts

  let symbols = function
    | `Bitfinex -> Bitfinex.symbols
    | `BTCE -> BTCE.symbols
    | `Kraken -> Kraken.symbols

  let price_increment = function
    | `Bitfinex -> Bitfinex.price_increment
    | `BTCE -> BTCE.price_increment
    | `Kraken -> Kraken.price_increment

  let trade_increment = function
    | `Bitfinex -> Bitfinex.trade_increment
    | `BTCE -> BTCE.trade_increment
    | `Kraken -> Kraken.trade_increment

  let ticker ~symbol ~exchange = match exchange with
    | `Bitfinex ->
      Bitfinex.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> ticker symbol)
    | `BTCE ->
      BTCE.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> ticker symbol)
    | `Kraken ->
      Kraken.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> ticker symbol)

  let book ~symbol ~exchange = match exchange with
    | `Bitfinex ->
      Bitfinex.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> book symbol)
    | `BTCE ->
      BTCE.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> book symbol)
    | `Kraken ->
      (Kraken.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> book symbol)
       :> (book_entry OrderBook.t, err) result Deferred.t)

  let trades ?since ?limit ~symbol ~exchange () = match exchange with
    | `Bitfinex ->
      Bitfinex.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> trades ?since ?limit symbol)
    | `BTCE ->
      BTCE.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> trades ?since ?limit symbol)
    | `Kraken ->
      (Kraken.(accept symbol |> function
        | None -> return @@ internal_error "unsupported"
        | Some symbol -> trades ?since ?limit symbol)
      :> (trade list, err) result Deferred.t)
end
