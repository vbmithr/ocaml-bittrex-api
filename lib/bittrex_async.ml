open Rresult
open Core.Std
open Async.Std
open Cohttp_async
open Mt
open Bittrex

include Bittrex_intf

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

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

    let base_uri = "https://api.bitfinex.com"

    let get ~endp ~params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endp in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let buf = Bigstring.create 1024

    let post ~creds:{ key; secret } ~endp ~body =
      let f () =
        let open Nocrypto in
        Log.debug log "-> %s" body;
        let uri = Uri.of_string @@ base_uri ^ endp in
        let nonce = Time_ns.(now () |> to_int63_ns_since_epoch) |> Int63.to_string in
        let body =
          try Yojson.Safe.from_string body
          with _ -> `Assoc [] in
        let payload =
          match body with
          | `Assoc params ->
            `Assoc (["request", `String endp;
                     "nonce", `String nonce;
                    ] @ params)
          | _ -> invalid_arg "bitfinex post body must be a json dict"
        in
        let body = Yojson.Safe.to_string payload in
        let body_b64 = Base64.encode Cstruct.(of_string body) in
        let signature = Hash.SHA384.hmac ~key:secret body_b64 in
        let signature = Hex.of_cstruct signature in
        let headers = Cohttp.Header.of_list
            ["X-BFX-APIKEY", Cstruct.to_string key;
             "X-BFX-PAYLOAD", Cstruct.to_string body_b64;
             "X-BFX-SIGNATURE", match signature with `Hex sign -> sign;
            ] in
        let body = Cohttp_async.Body.of_string body in
        Client.post ~headers ~body uri >>= fun (resp, body) ->
        Body.to_string body >>| fun body ->
        Log.debug log "<- %s" body;
        body
      in
      trap_exn f
  end
  include Bitfinex(H)
end

module Bitstamp = struct
  module H = struct
    include AsyncIO

    let base_uri = "https://www.bitstamp.net/api/"

    let get ~endp ~params =
      let f () =
        let uri =  Uri.of_string @@ base_uri ^ endp in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body
      in
      trap_exn f

    let post ~creds ~endp ~body = return not_implemented
  end
  include Bitstamp(H)
end

module BTCE = struct
  module H = struct
    include AsyncIO

    let version = "3"
    let base_uri = "https://btc-e.com/api/" ^ version ^ "/"

    let get ~endp ~params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endp in
        Client.get uri >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post ~creds ~endp ~body = return not_implemented
  end
  include BTCE(H)
end

module Bittrex = struct
  module H = struct
    include AsyncIO

    let version = "v1.1"
    let base_uri = "https://bittrex.com/api/" ^ version ^ "/"

    let get endpoint params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f
  end

  let post key endpoint params = return not_implemented
end

module Cryptsy = struct
  module H = struct
    include AsyncIO

    let version = "v2"
    let base_uri = "https://api.cryptsy.com/api/" ^ version ^ "/"

    let get endpoint params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post key endpoint params = return not_implemented
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

    let post key endpoint params = return not_implemented
  end
end

module Kraken = struct
  module H = struct
    include AsyncIO

    let base_uri = "https://api.kraken.com"

    let get ~endp ~params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endp in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let buf = Bigstring.create 1024

    let post ~creds:{key; secret} ~endp ~body =
      let f () =
        let open Nocrypto in
        let endp_len = String.length endp in
        let nonce = Time_ns.(now () |> to_int63_ns_since_epoch |> Int63.to_string) in
        let body = Uri.(encoded_of_query @@ ("nonce", [nonce]) :: query_of_encoded body) in
        let nonce_len = String.length nonce in
        let body_len = String.length body in

        let sha256 = Hash.SHA256.init () in
        Bigstring.From_string.blito nonce buf ();
        Hash.SHA256.feed sha256 @@ Cstruct.of_bigarray buf ~len:nonce_len;
        Bigstring.From_string.blito body buf ();
        Hash.SHA256.feed sha256 (Cstruct.of_bigarray buf ~len:body_len);
        let sha256 = Hash.SHA256.get sha256 in

        Bigstring.From_string.blito endp buf ();
        Cstruct.(Bigstring.blito ~src:sha256.buffer ~src_len:sha256.len
                   ~dst:buf ~dst_pos:(String.length endp) ());
        let sign = Hash.SHA512.hmac ~key:(Base64.decode secret) @@
          Cstruct.(of_bigarray buf ~len:(endp_len + sha256.len))
        in
        let headers = Cohttp.Header.of_list
            ["content-type", "application/x-www-form-urlencoded";
             "API-Key", Cstruct.to_string key;
             "API-Sign", Cstruct.to_string @@ Base64.encode sign;
            ] in
        let uri = Uri.of_string @@ base_uri ^ endp in
        let body = Cohttp_async.Body.of_string body in
        Client.post ~headers ~body uri >>= fun (resp, body) ->
        Body.to_string body >>| fun body ->
        Log.debug log "<- %s" body;
        body in
      trap_exn f
  end
  include Kraken(H)
end

module Hitbtc = struct
  module H = struct
    include AsyncIO

    let version = "1"
    let base_uri = "https://api.hitbtc.com/api/" ^ version ^ "/"

    let get endpoint params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post key endpoint params = return not_implemented
  end
end

module Generic = struct
  include AsyncIO
  type ticker = (int64, int64) Ticker.Tvwap.t
  type book_entry = int64 Mt.Tick.T.t
  type trade = (int64, int64) Mt.Tick.TDTS.t

  let symbols = function
    | `Bitfinex -> Bitfinex.symbols
    | `Bitstamp -> Bitstamp.symbols
    | `BTCE -> BTCE.symbols
    | `Kraken -> Kraken.symbols
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let price_increment = function
    | `Bitfinex -> Bitfinex.price_increment
    | `Bitstamp -> Bitstamp.price_increment
    | `BTCE -> BTCE.price_increment
    | `Kraken -> Kraken.price_increment
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let trade_increment = function
    | `Bitfinex -> Bitfinex.trade_increment
    | `Bitstamp -> Bitstamp.trade_increment
    | `BTCE -> BTCE.trade_increment
    | `Kraken -> Kraken.trade_increment
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let ticker ~symbol ~exchange = match exchange with
    | `Bitfinex ->
      Bitfinex.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> ticker symbol)
    | `Bitstamp ->
      Bitstamp.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> ticker symbol)
    | `BTCE ->
      BTCE.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> ticker symbol)
    | `Kraken ->
      Kraken.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> ticker symbol)
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let book ~symbol ~exchange = match exchange with
    | `Bitfinex ->
      Bitfinex.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> book symbol)
    | `Bitstamp ->
      Bitstamp.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> book symbol)
    | `BTCE ->
      BTCE.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> book symbol)
    | `Kraken ->
      (Kraken.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> book symbol)
       :> (book_entry list * book_entry list, err) result Deferred.t)
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let trades ?since ?limit ~symbol ~exchange () = match exchange with
    | `Bitfinex ->
      Bitfinex.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> trades ?since ?limit symbol)
    | `Bitstamp ->
      Bitstamp.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> trades ?since ?limit symbol)
    | `BTCE ->
      BTCE.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> trades ?since ?limit symbol)
    | `Kraken ->
      (Kraken.(accept symbol |> function
        | None -> return unsupported
        | Some symbol -> trades ?since ?limit symbol)
       :> (trade list, err) result Deferred.t)
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let balance credentials ~exchange = match exchange with
    | `Bitfinex -> Bitfinex.balance credentials
    | `Bitstamp -> Bitstamp.balance credentials
    | `BTCE -> BTCE.balance credentials
    | `Kraken -> Kraken.balance credentials
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let positions credentials ~exchange = match exchange with
    | `Bitfinex -> (Bitfinex.positions credentials :>
                      (< id : int; p : int64; v : int64;
                       symbol : Symbol.t > list, err) result t)
    | `Bitstamp -> invalid_arg "Not implemented"
    | `BTCE -> invalid_arg "Not implemented"
    | `Kraken -> invalid_arg "Not implemented"
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let orders credentials ~exchange = match exchange with
    | `Bitfinex -> Bitfinex.orders credentials
    | `Bitstamp -> invalid_arg "Not implemented"
    | `BTCE -> invalid_arg "Not implemented"
    | `Kraken -> invalid_arg "Not implemented"
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"

  let filled_orders ?after ?before credentials ~exchange =
    match exchange with
    | `Bitfinex -> (Bitfinex.filled_orders ?after ?before credentials :>
                      (< order_id : int64; p : int64; side : [ `Buy | `Sell ];
                       symbol: Symbol.t;
                       tid : int64; ts : int64; v : int64 > list, err) result t)
    | `Bitstamp -> invalid_arg "Not implemented"
    | `BTCE -> invalid_arg "Not implemented"
    | `Kraken -> invalid_arg "Not implemented"
    | `OKCoin -> invalid_arg "Not implemented"
    | `Coinbase -> invalid_arg "Not implemented"
end
