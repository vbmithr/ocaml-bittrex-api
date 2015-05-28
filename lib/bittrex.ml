open Bittrex_intf

module type JSONABLE = sig
  type t
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> [`Ok of t | `Error of string]
end

module Stringable = struct
  module Of_jsonable (T: JSONABLE) = struct
    let to_string t = T.to_yojson t |> Yojson.Safe.to_string
    let pp ppf t = Format.fprintf ppf "%s" (to_string t)
    let of_string s = Yojson.Safe.from_string s |> T.of_yojson
    let ts_of_json = function
      | `List ts ->
        begin
          let ts = CCList.filter_map
              (fun t -> match T.of_yojson t with
                 | `Ok a -> Some a
                 | `Error s -> None) ts
          in `Ok ts
        end
      | json -> `Error (Yojson.Safe.to_string json)
  end
end

module OrderBook = struct
  type 'a book = {
    bids: 'a list;
    asks: 'a list;
  } [@@deriving show,create,yojson]

  type order = {
    price: int64;
    qty: int64;
  } [@@deriving show,create,yojson]

  type t = order book [@@deriving show,yojson]
end

module Ticker = struct
  type t = {
    last: int64;
    bid: int64;
    ask: int64;
    high: int64;
    low: int64;
    volume: int64;
    timestamp: int64;
    vwap: int64 option;
  } [@@deriving show,create]
end

module Trade = struct
  type kind = [`Ask | `Bid | `Unknown] [@@deriving show]

  type t = {
    ts: int64;
    price: int64;
    qty: int64;
    kind: kind;
  } [@@deriving show,create]
end

let yojson_of_string s =
  CCError.guard_str (fun () -> Yojson.Safe.from_string s)

module Int64 = struct
  include Int64
  let ( * ) = mul
  let ( + ) = add
end

let satoshis_of_string s =
  CCString.Split.list_ ~by:"." s |> function
  | [(a, _, _); (b, _, l)] ->
    Some Int64.(of_string a * 100_000_000L + of_string b * of_int (CCInt.pow 10 (8 - l)))
  | _ -> None

let satoshis_of_string_exn s =
  satoshis_of_string s |> function
  | Some v -> v
  | None -> invalid_arg "satoshis_of_string"

let satoshis_of_float v =
  satoshis_of_string @@ Printf.sprintf "%.8f" v

let satoshis_of_float_exn v =
  satoshis_of_float v |> function
  | Some v -> v
  | None -> invalid_arg "satoshis_of_float"

let gettimeofday_int64 () = Int64.of_float @@ Unix.gettimeofday () *. 1e6

module Bitfinex (H: HTTP_CLIENT) = struct
  open H
  type 'a io = 'a H.t

  let name = "bitfinex"

  let get endpoint params yojson_to_a =
    get endpoint params >>|
    CCError.(flat_map (fun s -> flat_map yojson_to_a (yojson_of_string s)))

  type pair = [`BTCUSD | `LTCBTC]
  let pairs = [`BTCUSD; `LTCBTC]

  let string_of_pair = function
    | `BTCUSD -> "BTCUSD"
    | `LTCBTC -> "LTCBTC"

  module Ticker = struct
    include Ticker

    module Raw = struct
      module T = struct
        type t = {
          mid: string;
          bid: string;
          ask: string;
          last_price: string;
          low: string;
          high: string;
          volume: string;
          timestamp: string;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let ticker p = get ("pubticker/" ^ string_of_pair p) [] of_yojson
    end

    let of_raw r =
      let vwap = satoshis_of_string_exn r.Raw.mid in
      let bid = satoshis_of_string_exn r.Raw.bid in
      let ask = satoshis_of_string_exn r.Raw.ask in
      let last = satoshis_of_string_exn r.Raw.last_price in
      let low = satoshis_of_string_exn r.Raw.low in
      let high = satoshis_of_string_exn r.Raw.high in
      let volume = satoshis_of_string_exn r.Raw.volume in
      let timestamp = Int64.of_float @@ 1e6 *. float_of_string r.Raw.timestamp in
      Ticker.create ~bid ~ask ~high ~low ~volume ~vwap ~last ~timestamp ()

    let ticker p = Raw.ticker p >>| CCError.map of_raw
  end

  module OrderBook = struct
    include OrderBook

    module Raw = struct
      module T = struct
        type order = {
          price: string;
          amount: string;
          timestamp: string;
        } [@@deriving show,yojson]

        type t = order OrderBook.book [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let book p = get ("book/" ^ string_of_pair p) [] of_yojson
    end

    let of_raw r =
      {
        price = satoshis_of_string_exn r.Raw.price;
        qty = satoshis_of_string_exn r.Raw.amount;
      }

    let of_raw { bids; asks; } =
      {
        bids = List.map of_raw bids;
        asks = List.map of_raw asks;
      }

    let book p = Raw.book p >>= fun p -> return @@ CCError.map of_raw p
  end

  module Trade = struct
    include Trade

    module Raw = struct
      module T = struct
        type t = {
          timestamp: int;
          tid: int;
          price: string;
          amount: string;
          exchange: string;
          type_ [@key "type"]: string;
        } [@@deriving yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let trades ?since ?limit p =
        get ("trades/" ^ string_of_pair p)
          (match since, limit with
           | None, None -> []
           | Some v, None -> ["timestamp", v |> int_of_float |> string_of_int]
           | None, Some v -> ["limit_trades", v]
           | Some ts, Some l ->
             ["timestamp", ts |> int_of_float |> string_of_int;
              "limit_trades", l])
          ts_of_json
    end

    let kind_of_raw = function
      | "sell" -> `Ask
      | "buy" -> `Bid
      | _ -> `Unknown

    let of_raw t = Trade.create
        ~ts:Int64.(1_000_000L * of_int t.Raw.timestamp)
        ~price:(satoshis_of_string_exn t.Raw.price)
        ~qty:(satoshis_of_string_exn t.Raw.amount)
        ~kind:(kind_of_raw t.Raw.type_) ()

    let trades ?since ?limit p =
      Raw.trades p >>= fun p -> return @@ CCError.map (List.map of_raw) p
  end
end

module Bittrex (H: HTTP_CLIENT) = struct
  open H

  type 'a error_monad = {
    success: bool;
    message: string;
    result: 'a option;
  } [@@deriving show,yojson]

  let get endpoint params yojson_to_a =
    let handle_err s =
      yojson_of_string s |>
      CCError.flat_map (error_monad_of_yojson yojson_to_a) |>
      CCError.flat_map
        (function | { success = true; result = Some r } -> `Ok r
                  | { success = false; message } -> `Error message
                  | _ -> `Error "success=true but result=None")
    in
    get endpoint params >>| CCError.flat_map handle_err

  type supported_curr = [`BTC | `LTC | `DOGE]

  let string_of_curr = function
    | `BTC -> "BTC"
    | `LTC -> "LTC"
    | `DOGE -> "DOGE"

  module Market = struct
    module Raw = struct
      module T = struct
        type t = {
          market_currency [@key "MarketCurrency"]: string;
          base_currency [@key "BaseCurrency"] : string;
          market_currency_long [@key "MarketCurrencyLong"] : string;
          base_currency_long [@key "BaseCurrencyLong"] : string;
          min_trade_size [@key "MinTradeSize"] : float;
          market_name [@key "MarketName"] : string;
          is_active [@key "IsActive"] : bool;
          created [@key "Created"] : string;
          notice [@key "Notice"] : string option;
          is_sponsored [@key "IsSponsored"] : bool option;
          logo_url [@key "LogoUrl"] : string option;
        } [@@deriving show,yojson]
      end

      include T
      include Stringable.Of_jsonable(T)

      let markets () = get "public/getmarkets" [] ts_of_json
    end
    include Raw
  end

  module MarketSummary = struct
    module Raw = struct
      module T = struct
        type t = {
          market_name [@key "MarketName"]: string;
          high [@key "High"]: float;
          low [@key "Low"]: float;
          volume [@key "Volume"]: float;
          last [@key "Last"]: float;
          base_volume [@key "BaseVolume"]: float;
          timestamp [@key "TimeStamp"]: string;
          bid [@key "Bid"]: float;
          ask [@key "Ask"]: float;
          open_buy_orders [@key "OpenBuyOrders"]: int;
          open_sell_orders [@key "OpenSellOrders"]: int;
          prev_day [@key "PrevDay"]: float;
          created [@key "Created"]: string;
        } [@@deriving show,yojson]
      end

      include T
      include Stringable.Of_jsonable(T)

      let summaries () = get "public/getmarketsummaries" [] ts_of_json
      let summary pair = get "public/getmarketsummary" ["market", pair] ts_of_json
    end
    include Raw
  end

  module Ticker = struct
    module Raw = struct
      module T = struct
        type t = {
          bid [@key "Bid"] : float;
          ask [@key "Ask"] : float;
          last [@key "Last"] : float;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)


      let ticker c1 c2 = get "public/getticker"
          ["market", string_of_curr c2 ^ "-" ^ string_of_curr c1] of_yojson
    end
    include Raw
  end

  module Currency = struct
    module T = struct
      type t = {
        currency [@key "Currency"] : string;
        currency_long [@key "CurrencyLong"] : string;
        min_confirmation [@key "MinConfirmation"] : int;
        tx_fee [@key "TxFee"] : float;
        is_active [@key "IsActive"] : bool;
        coin_type [@key "CoinType"] : string;
        base_addr [@key "BaseAddress"] : string option;
        notice [@key "Notice"] : string option;
      } [@@deriving show,yojson]
    end
    include T
    include Stringable.Of_jsonable(T)

    let currencies () = get "public/getcurrencies" [] ts_of_json
  end

  module OrderBook = struct
    module Raw = struct
      type order = {
        price [@key "Rate"] : float;
        qty [@key "Quantity"] : float;
      } [@@deriving yojson]

      type book = {
        buy: order list;
        sell: order list
      } [@@deriving yojson]

      let book c1 c2 = get "public/getorderbook"
          ["market", string_of_curr c2 ^ "-" ^ string_of_curr c1;
           "type", "both"; "depth", "50"] book_of_yojson
    end
    open OrderBook

    let of_raw t =
      let open Raw in
      { bids = List.map (fun { price; qty; } ->
            let price = satoshis_of_float_exn price in
            let qty = satoshis_of_float_exn qty in
            OrderBook.create_order ~price ~qty ()) t.Raw.buy;
        asks = List.map (fun { price; qty; } ->
            let price = satoshis_of_float_exn price in
            let qty = satoshis_of_float_exn qty in
            OrderBook.create_order ~price ~qty ()) t.Raw.sell;
      }

    let book c1 c2 = Raw.book c1 c2 >>| CCError.map of_raw
  end
end

module Cryptsy (H: HTTP_CLIENT) = struct
  open H

  type 'a error_monad = {
    success: bool;
    error: string [@default ""];
    data: 'a option;
  } [@@deriving show,yojson]

  let get endpoint params yojson_to_a =
    let handle_err s =
      yojson_of_string s |>
      CCError.flat_map (error_monad_of_yojson yojson_to_a) |>
      CCError.flat_map
        (function | { success = true; data = Some r } -> `Ok r
                  | { success = false; error } -> `Error error
                  | _ -> `Error "success=true but data=None")
    in get endpoint params >>| CCError.flat_map handle_err

  type supported_curr = [`BTC | `LTC | `DOGE]

  module Currency = struct
    module Raw = struct
      module T = struct
        type t = {
          id: string;
          name: string;
          code: string;
          maintenance: string;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let currencies () = get "currencies" [] ts_of_json
    end

    type t = {
      id: int;
      name: string;
      code: string;
      maintenance: int;
    } [@@deriving show,yojson]

    let of_raw r = {
      id = int_of_string r.Raw.id;
      name = r.Raw.name;
      code = r.Raw.code;
      maintenance = int_of_string r.Raw.maintenance;
    }

    let currencies () = Raw.currencies () >>| CCError.map @@ List.map of_raw
  end

  module Market = struct

    type stats = {
      volume: float;
      volume_btc: float;
      price_high: float;
      price_low: float;
    } [@@deriving show,yojson]

    type last_trade = {
      price: float;
      date: string;
      timestamp: int;
    } [@@deriving show,yojson]

    module Raw = struct
      module T = struct
        type t = {
          id: string;
          label: string;
          coin_currency_id: string;
          market_currency_id: string;
          maintenance_mode: string;
          verifiedonly: bool;
          stats [@key "24hr"] : stats;
          last_trade: last_trade;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let markets () = get "markets" [] ts_of_json
    end

    type t = {
      id: int;
      label: string;
      coin_currency_id: int;
      market_currency_id: int;
      maintenance_mode: int;
      verifiedonly: bool;
      stats : stats;
      last_trade: last_trade;
    } [@@deriving show,yojson]

    let of_raw t = {
      id = int_of_string t.Raw.id;
      label = t.Raw.label;
      coin_currency_id = int_of_string t.Raw.coin_currency_id;
      market_currency_id = int_of_string t.Raw.market_currency_id;
      maintenance_mode = int_of_string t.Raw.maintenance_mode;
      verifiedonly = t.Raw.verifiedonly;
      stats = t.Raw.stats;
      last_trade = t.Raw.last_trade;
    }

    let markets () = Raw.markets () >>| CCError.map @@ List.map of_raw
  end

  module Ticker = struct
    module Raw = struct
      module T = struct
        type t = {
          id: string;
          bid: float;
          ask: float;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let string_of_curr = function
        | `BTC -> "btc"
        | `LTC -> "ltc"
        | `DOGE -> "doge"

      let ticker c1 c2 = get
          ("markets/" ^ string_of_curr c1 ^ "_" ^ string_of_curr c2 ^ "/ticker")
          [] of_yojson

      let tickers () = get "markets/ticker" [] ts_of_json
    end

    type t = {
      id: int;
      bid: float;
      ask: float;
    } [@@deriving show,yojson]

    let of_raw t = {
      id = int_of_string t.Raw.id;
      bid = t.Raw.bid;
      ask = t.Raw.ask;
    }

    let ticker c1 c2 = Raw.ticker c1 c2 >>| CCError.map of_raw
    let tickers () = Raw.tickers () >>| CCError.map @@ List.map of_raw
  end
end

module BTCE (H: HTTP_CLIENT) = struct
  open H
  type 'a io = 'a H.t

  let name = "btce"

  let get endpoint params yojson_to_a =
    let handle_err s =
      yojson_of_string s |> CCError.flat_map
        (function
          | `Assoc [(_, ret)] -> yojson_to_a ret
          | _ -> `Error s)
    in
    get endpoint params >>| CCError.flat_map handle_err

  type pair = [`BTCUSD | `LTCBTC]
  let pairs = [`BTCUSD; `LTCBTC]

  let string_of_pair = function
    | `BTCUSD -> "btc_usd"
    | `LTCBTC -> "ltc_btc"

  module Ticker = struct
    include Ticker

    module Raw = struct
      module T = struct
        type t = {
          high: float;
          low: float;
          avg: float;
          vol: float;
          vol_cur: float;
          last: float;
          buy: float;
          sell: float;
          updated: int;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let ticker p = get ("ticker/" ^ string_of_pair p) [] of_yojson
    end
    let of_raw t =
      let open Raw in
      Ticker.create
        ~bid:(satoshis_of_float_exn t.buy)
        ~ask:(satoshis_of_float_exn t.sell)
        ~last:(satoshis_of_float_exn t.last)
        ~high:(satoshis_of_float_exn t.high)
        ~low:(satoshis_of_float_exn t.low)
        ~vwap:(satoshis_of_float_exn t.avg)
        ~volume:(satoshis_of_float_exn t.vol)
        ~timestamp:Int64.(1_000_000L * of_int t.updated) ()

    let ticker p = Raw.ticker p >>= fun t -> return @@ CCError.map of_raw t
  end

  module OrderBook = struct
    include OrderBook

    module Raw = struct
      type t = {
        asks: float list list;
        bids: float list list
      } [@@deriving create,yojson]
    end

    open OrderBook

    let book p = get ("depth/" ^ string_of_pair p) [] Raw.of_yojson >>|
      CCError.flat_map
        (fun t ->
           let f t =
             create_book
               ~bids:(List.map (function
                   | [price; qty] ->
                     { price = satoshis_of_float_exn price;
                       qty = satoshis_of_float_exn qty }
                   | _ -> raise Exit)
                   t.Raw.bids)
               ~asks:(List.map (function
                   | [price; qty] ->
                     { price = satoshis_of_float_exn price;
                       qty = satoshis_of_float_exn qty }
                   | _ -> raise Exit)
                   t.Raw.asks) ()
           in
           try `Ok (f t)
           with Exit -> `Error "book"
        )
  end

  module Trade = struct
    include Trade

    module Raw = struct
      module T = struct
        type t = {
          type_ [@key "type"]: string;
          price: float;
          amount: float;
          tid: int;
          timestamp: int;
        } [@@deriving yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let trades p = get ("trades/" ^ string_of_pair p) [] ts_of_json
    end

    open Trade

    let trades ?since ?limit p = Raw.trades p >>= fun trades ->
      return @@ CCError.map (fun trades -> List.map (fun t ->
          create
            ~ts:Int64.(1_000_000L * of_int t.Raw.timestamp)
            ~price:(satoshis_of_float_exn t.Raw.price)
            ~qty:(satoshis_of_float_exn t.Raw.amount)
            ~kind:(match t.Raw.type_ with
                | "bid" -> `Bid
                | "ask" -> `Ask
                | _ -> `Unknown) ()) trades)
        trades
  end
end

module Poloniex (H: HTTP_CLIENT) = struct
  open H

  type supported_curr = [`BTC | `LTC | `DOGE]

  let string_of_curr = function
    | `BTC -> "BTC"
    | `LTC -> "LTC"
    | `DOGE -> "DOGE"

  module Ticker = struct
    let get c1 c2 params yojson_to_a =
      let handle_err s =
        let curr_str = string_of_curr c2 ^ "_" ^ string_of_curr c1 in
        yojson_of_string s |> CCError.flat_map
          (function
            | `Assoc l ->
              (try
                 let t = List.assoc curr_str l in yojson_to_a t
               with Not_found -> `Error "Unknown currency")
            | _ -> `Error s)
      in
      get "" params >>| CCError.flat_map handle_err

    module Raw = struct
      module T = struct
        type t = {
          last: string;
          lowestAsk: string;
          highestBid: string;
          percentChange: string;
          baseVolume: string;
          quoteVolume: string;
          isFrozen: string;
          high24hr: string;
          low24hr: string;
        } [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let ticker c1 c2 = get c1 c2 ["command", "returnTicker"] of_yojson
    end

    include Ticker

    let of_raw t = create
      ~timestamp:(gettimeofday_int64 ())
      ~last:(satoshis_of_string_exn t.Raw.last)
      ~bid:(satoshis_of_string_exn t.Raw.highestBid)
      ~ask:(satoshis_of_string_exn t.Raw.lowestAsk)
      ~volume:(satoshis_of_string_exn t.Raw.baseVolume)
      ~high:(satoshis_of_string_exn t.Raw.high24hr)
      ~low:(satoshis_of_string_exn t.Raw.low24hr) ()

    let ticker c1 c2 = Raw.ticker c1 c2 >>| CCError.map of_raw
  end

  module OrderBook = struct
    open OrderBook

    let book c1 c2 =
      let curr_str = string_of_curr c2 ^ "_" ^ string_of_curr c1 in

      let handle_err s =
        let parse =
          let mapf = function
            | `List [`String price; `Float qty] ->
              { price = satoshis_of_string_exn price;
                qty = satoshis_of_float_exn qty
              }
            | `List [`String price; `Int qty] ->
              { price = satoshis_of_string_exn price;
                qty = Int64.(10_000_000L * of_int qty)
              }
            | json -> invalid_arg @@ Yojson.Safe.to_string json in
          List.map mapf in
        yojson_of_string s |> CCError.flat_map
          (function
            | `Assoc ["asks", `List asks; "bids", `List bids; "isFrozen", `String frz] ->
              if frz <> "0" then `Ok { asks = []; bids = [] }
              else
                let f () = create_book ~asks:(parse asks) ~bids:(parse bids) ()
                in
                (try `Ok (f ()) with Invalid_argument str -> `Error str)
            | json -> `Error (Yojson.Safe.to_string json))
      in
      get "" ["command", "returnOrderBook";
              "currencyPair", curr_str] >>| CCError.flat_map handle_err
  end
end

module Kraken (H: HTTP_CLIENT) = struct
  open H
  type 'a io = 'a H.t

  let name = "kraken"

  type 'a error_monad = {
    error: string list;
    result: 'a option [@default None];
  } [@@deriving yojson]

  let get endpoint params yojson_to_a =
    let lift_f = function
      | `Assoc [_, t] -> yojson_to_a t
      | _ -> `Error "lift_f"
    in
    let handle_err s =
      yojson_of_string s |>
      CCError.flat_map (error_monad_of_yojson lift_f) |>
      CCError.flat_map
        (function
          | { error = []; result = Some r } -> `Ok r
          | { error; result = None } -> `Error (String.concat " " error)
          | _ -> `Error "internal error")
    in
    get endpoint params >>| CCError.flat_map handle_err

  type pair = [`BTCUSD | `BTCLTC]
  let pairs = [`BTCUSD; `BTCLTC]

  let string_of_pair = function
    | `BTCUSD -> "XBTCZUSD"
    | `BTCLTC -> "XXBTXLTC"

  module Ticker = struct
    module Raw = struct
      module T = struct
        type t = {
          a: string list;
          b: string list;
          c: string list;
          v: string list;
          p: string list;
          t: int list;
          l: string list;
          h: string list;
          o: string;
        } [@@deriving yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let ticker p = get "public/Ticker" ["pair", string_of_pair p] of_yojson
    end

    include Ticker

    let of_raw t = create
        ~timestamp:(gettimeofday_int64 ())
        ~bid:(satoshis_of_string_exn @@ List.hd t.Raw.b)
        ~ask:(satoshis_of_string_exn @@ List.hd t.Raw.a)
        ~volume:(satoshis_of_string_exn @@ List.nth t.Raw.v 1)
        ~vwap:(satoshis_of_string_exn @@ List.nth t.Raw.p 1)
        ~low:(satoshis_of_string_exn @@ List.nth t.Raw.l 1)
        ~high:(satoshis_of_string_exn @@ List.nth t.Raw.h 1) ()

    let ticker p = Raw.ticker p >>| CCError.map of_raw
  end

  module OrderBook = struct
    open OrderBook

    let book p =
      let lift_f = function
        | `Assoc ["asks", `List asks; "bids", `List bids] ->
          let f () = create_book
            ~asks:(List.map (function
                  | `List [`String price; `String qty; `Int ts] ->
                    create_order
                      ~price:(satoshis_of_string_exn price)
                      ~qty:(satoshis_of_string_exn qty) ()
                  | _ -> raise Exit
                ) asks)
            ~bids:(List.map (function
                | `List [`String price; `String qty; `Int ts] ->
                  create_order
                    ~price:(satoshis_of_string_exn price)
                    ~qty:(satoshis_of_string_exn qty) ()
                | _ -> raise Exit
              ) bids) () in
          (try `Ok (f ()) with Exit -> `Error "book")
        | _ -> `Error "lift_f" in

      get "public/Depth" ["pair", string_of_pair p] lift_f
  end

  (* module Trade = struct *)
  (*   open Trade *)

  (*   module Raw = struct *)
  (*     type t =  *)
  (*   end *)
  (*   let trades p = *)
  (*     let lift_f = function *)
  (*       |  *)
  (*     get "public/Trades" ["pair", string_of_pair p] lift_f *)
  (* end *)
end

module Hitbtc (H: HTTP_CLIENT) = struct
  open H

  let get endpoint params yojson_to_a =
    let handle_err s =
      CCError.flat_map yojson_to_a @@ yojson_of_string s in
    get endpoint params >>| CCError.flat_map handle_err

  type supported_curr = [`BTC | `LTC | `DOGE]

  let string_of_curr = function
    | `BTC -> "BTC"
    | `LTC -> "LTC"
    | `DOGE -> "DOGE"

  module Ticker = struct
    module Raw = struct
      module T = struct
        type t = {
          ask: string;
          bid: string;
          last: string;
          low: string;
          high: string;
          o [@key "open"]: string;
          volume: string;
          volume_quote: string;
          timestamp: int;
        } [@@deriving yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let ticker c1 c2 =
        get ("public/" ^ string_of_curr c1 ^ string_of_curr c2 ^ "/ticker") []
          of_yojson
    end

    include Ticker

    let of_raw t = create
        ~ask:(satoshis_of_string_exn t.Raw.ask)
        ~bid:(satoshis_of_string_exn t.Raw.bid)
        ~last:(satoshis_of_string_exn t.Raw.last)
        ~low:(satoshis_of_string_exn t.Raw.low)
        ~high:(satoshis_of_string_exn t.Raw.high)
        ~volume:(satoshis_of_string_exn t.Raw.volume)
        ~timestamp:Int64.(1000L * of_int t.Raw.timestamp) ()

    let ticker c1 c2 = Raw.ticker c1 c2 >>| CCError.map of_raw
  end

  module OrderBook = struct
    module Raw = struct
      module T = struct
        type t = {
          asks: string list list;
          bids: string list list;
        } [@@deriving yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let book c1 c2 =
        get ("public/" ^ string_of_curr c1 ^ string_of_curr c2 ^ "/orderbook") []
          of_yojson
    end

    open OrderBook
    let of_raw t =
      let f t =
        create_book
          ~bids:(List.map (function
              | [price; qty] ->
                OrderBook.create_order
                  ~price:(satoshis_of_string_exn price)
                  ~qty:(satoshis_of_string_exn qty) ()
              | _ -> raise Exit
            ) t.Raw.bids)
          ~asks:(List.map (function
              | [price; qty] ->
                OrderBook.create_order
                  ~price:(satoshis_of_string_exn price)
                  ~qty:(satoshis_of_string_exn qty) ()
              | _ -> raise Exit
            ) t.Raw.asks) ()
      in try `Ok (f t) with Exit -> `Error "of_raw"

    let book c1 c2 = Raw.book c1 c2 >>| CCError.map of_raw
  end
end
