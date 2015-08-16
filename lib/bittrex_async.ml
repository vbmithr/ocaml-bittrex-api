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

    let get endpoint params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post key endpoint params = Deferred.return @@
      R.fail @@ `Internal_error "Unsupported"
  end
  include Bitfinex(H)
end

module BTCE = struct
  module H = struct
    include AsyncIO

    let version = "3"
    let base_uri = "https://btc-e.com/api/" ^ version ^ "/"

    let get endpoint _ =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get uri >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post key endpoint params = Deferred.return @@
      R.fail @@ `Internal_error "Unsupported"
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

  let post key endpoint params = Deferred.return @@
      R.fail @@ `Internal_error "Unsupported"
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

    let post key endpoint params = Deferred.return @@
      R.fail @@ `Internal_error "Unsupported"
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

    let post key endpoint params = Deferred.return @@
      R.fail @@ `Internal_error "Unsupported"
  end
end

module Kraken = struct
  module H = struct
    include AsyncIO

    let version = "0"
    let base_uri = "https://api.kraken.com/" ^ version ^ "/"

    let get endpoint params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post_nonce = ref 0L
    let buf = Bigstring.create 1024

    let nonce () =
      let ret = !post_nonce in
      Int64.(post_nonce := !post_nonce + 1L);
      ret

    let post creds endpoint params =
      let f () =
        let open Nocrypto in
        let uri_str = base_uri ^ endpoint in
        let uri_str_len = String.length uri_str in
        let nonce = nonce () in
        let body = Uri.encoded_of_query @@
          List.map ~f:(fun (a, b) -> a, [b]) params in
        let body_len = String.length body in

        let sha256 = Hash.SHA256.init () in
        EndianBigstring.BigEndian.set_int64 buf 0 nonce;
        Hash.SHA256.feed sha256 (Cstruct.of_bigarray buf ~off:0 ~len:8);
        Bigstring.From_string.blit body 0 buf 0 body_len;
        Hash.SHA256.feed sha256 (Cstruct.of_bigarray buf ~off:0 ~len:body_len);
        Bigstring.From_string.blit uri_str 0 buf 0 uri_str_len;
        let sha256 = Hash.SHA256.get sha256 in
        Cstruct.(Bigstring.blit ~src:sha256.buffer ~src_pos:0
                   ~dst:buf ~dst_pos:uri_str_len ~len:sha256.len);
        let key = Base64.decode creds.secret in
        let sign = Hash.SHA512.hmac ~key @@
          Cstruct.(of_bigarray buf ~off:0 ~len:(uri_str_len + sha256.len))
        in
        let headers = Cohttp.Header.of_list
            ["content-type", "application/x-www-form-urlencoded";
             "API-Key", Cstruct.to_string creds.key;
             "API-Sign", Cstruct.to_string @@ Base64.encode sign;
            ] in
        let uri = Uri.of_string uri_str in
        let body = Cohttp_async.Body.of_string body in
        Client.post ~headers ~body uri >>= fun (resp, body) ->
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

    let get endpoint params =
      let f () =
        let uri = Uri.of_string @@ base_uri ^ endpoint in
        Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
        Body.to_string body in
      trap_exn f

    let post key endpoint params = Deferred.return @@
      R.fail @@ `Internal_error "Unsupported"
  end
end

module Generic = struct
  include AsyncIO
  type ticker = (int64, int64) Ticker.Tvwap.t
  type book_entry = int64 Mt.Tick.T.t
  type trade = (int64, int64) Mt.Tick.TDTS.t

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
       :> (book_entry list * book_entry list, err) result Deferred.t)

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

  let balance credentials ~exchange ~currency = match exchange with
    | `Bitfinex -> Bitfinex.balance credentials currency
    | `BTCE -> BTCE.balance credentials currency
    | `Kraken -> Kraken.balance credentials currency
end
