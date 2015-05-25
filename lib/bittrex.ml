module type HTTP_CLIENT = sig
  include Cohttp.S.IO

  val get : string -> (string * string) list -> string t
end

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
      | _ -> `Error "Not a json array."
  end
end

module OrderBook = struct
  type 'a book = {
    bids: 'a list;
    asks: 'a list;
  } [@@deriving show,yojson]

  type order = {
    price: float;
    qty: float;
  } [@@deriving show,yojson,create]

  type t = order book [@@deriving show,yojson]
end

module Ticker = struct
  type t = {
    last: float;
    bid: float;
    ask: float;
    high: float;
    low: float;
    volume: float;
    timestamp: float;
    vwap: float option;
  } [@@deriving show,create]
end

module Trade = struct
  type kind = [`Ask | `Bid | `Unknown] [@@deriving show]

  type t = {
    ts: float;
    price: float;
    qty: float;
    kind: kind;
  } [@@deriving show,create]
end

module type EXCHANGE = sig
  type 'a io
  type currency

  module Ticker : sig
    val ticker : currency -> currency -> (Ticker.t, string) CCError.t io
  end

  module OrderBook : sig
    val book : currency -> currency -> (OrderBook.t, string) CCError.t io
  end

  module Trade : sig
    val trades : ?since:float -> ?limit:int ->
      currency -> currency -> (Trade.t list, string) CCError.t io
  end
end

module Bitfinex (H: HTTP_CLIENT) = struct
  open H
  type 'a io = 'a H.t

  let get endpoint params yojson_to_a =
    try
      get endpoint params >>= fun s ->
      return @@ yojson_to_a @@ Yojson.Safe.from_string s
    with exn -> return @@ `Error (Printexc.to_string exn)

  type currency = [`BTC | `LTC | `USD]

  let string_of_curr = function
    | `BTC -> "BTC"
    | `LTC -> "LTC"
    | `USD -> "USD"

  module Ticker = struct
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

      let ticker c1 c2 = get
          ("pubticker/" ^ string_of_curr c1 ^ string_of_curr c2) [] of_yojson
    end

    let of_raw r =
      let vwap = float_of_string r.Raw.mid in
      let bid = float_of_string r.Raw.bid in
      let ask = float_of_string r.Raw.ask in
      let last = float_of_string r.Raw.last_price in
      let low = float_of_string r.Raw.low in
      let high = float_of_string r.Raw.high in
      let volume = float_of_string r.Raw.volume in
      let timestamp = float_of_string r.Raw.timestamp in
      Ticker.create ~bid ~ask ~high ~low ~volume ~vwap ~last ~timestamp ()

    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t -> return @@ CCError.map of_raw t
  end

  module OrderBook = struct
    open OrderBook

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

      let book c1 c2 = get ("book/" ^ string_of_curr c1 ^ string_of_curr c2)
          [] of_yojson
    end

    let of_raw r =
      {
        price = float_of_string r.Raw.price;
        qty = float_of_string r.Raw.amount;
      }

    let book c1 c2 = Raw.book c1 c2 >>= fun t ->
      return @@
      CCError.map (fun { bids; asks; } ->
          { bids = List.map of_raw bids; asks = List.map of_raw asks }) t
  end

  module Trade = struct
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

      let trades ?since ?limit c1 c2 =
        get ("trades/" ^ string_of_curr c1 ^ string_of_curr c2)
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
      | _ -> invalid_arg "kind_of_raw"

    let of_raw t = Trade.create
        ~ts:(float t.Raw.timestamp)
        ~price:(float_of_string t.Raw.price)
        ~qty:(float_of_string t.Raw.amount)
        ~kind:(kind_of_raw t.Raw.type_) ()

    let trades ?since ?limit c1 c2 = Raw.trades c1 c2 >>= fun trades ->
      return @@ CCError.map (List.map of_raw) trades

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
    get endpoint params >>= fun s ->
    Yojson.Safe.from_string s |>
    error_monad_of_yojson yojson_to_a |>
    function | `Ok { success = true; result = Some r } -> return r
             | `Ok { success = false; message } -> failwith message
             | `Error reason -> failwith reason
             | _ -> failwith "internal error"

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
      { bids = List.map (fun { Raw.price; Raw.qty; } ->
            OrderBook.create_order ~price ~qty ()) t.Raw.buy;
        asks = List.map (fun { Raw.price; Raw.qty; } ->
            OrderBook.create_order ~price ~qty ()) t.Raw.sell;
      }

    let book c1 c2 = Raw.book c1 c2 >>= fun b -> return @@ of_raw b
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
    get endpoint params >>= fun s ->
    Yojson.Safe.from_string s |>
    error_monad_of_yojson yojson_to_a |>
    function | `Ok { success = true; data = Some r } -> return r
             | `Ok { success = false; error } -> failwith error
             | `Error reason -> failwith reason
             | _ -> failwith "internal error"

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

    let currencies () = Raw.currencies () >>= fun ts -> return @@ List.map of_raw ts
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

    let markets () = Raw.markets () >>= fun ts -> return @@ List.map of_raw ts
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

    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t -> return @@ of_raw t
    let tickers () = Raw.tickers () >>= fun ts -> return @@ List.map of_raw ts
  end
end

module BTCE (H: HTTP_CLIENT) = struct
  open H
  type 'a io = 'a H.t

  let get endpoint params yojson_to_a =
    get endpoint params >>= fun s ->
    Yojson.Safe.from_string s |> function
    | `Assoc [(_, ret)] ->
      yojson_to_a ret |> (function | `Ok r -> return r
                                   | `Error reason -> failwith reason)
    | _ -> failwith s

  type currency = [`BTC | `LTC]

  let string_of_curr = function
    | `BTC -> "btc"
    | `LTC -> "ltc"

  module Ticker = struct
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

      let ticker c1 c2 = get
          ("ticker/" ^ string_of_curr c1 ^ "_" ^ string_of_curr c2) [] of_yojson
    end
    let of_raw t =
      let open Raw in
      Ticker.create
        ~bid:t.buy ~ask:t.sell ~last:t.last ~high:t.high ~low:t.low
        ~vwap:t.avg ~volume:t.vol ~timestamp:(float t.updated) ()

    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t ->
      return @@ of_raw t
  end

  module OrderBook = struct
    module Raw = struct
      type t = {
        asks: float list list;
        bids: float list list
      } [@@deriving yojson]
    end

    open OrderBook

    let book c1 c2 = get ("depth/" ^ string_of_curr c1 ^ "_" ^ string_of_curr c2)
        [] Raw.of_yojson >>= fun t ->
      return @@ {
        bids = List.map (function | [price; qty] -> { price; qty } | _ -> invalid_arg "book") t.Raw.bids;
        asks = List.map (function | [price; qty] -> { price; qty } | _ -> invalid_arg "book") t.Raw.asks;
      }
  end

  module Trade = struct
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

      let trades c1 c2 = get ("depth/" ^ string_of_curr c1 ^ "_" ^ string_of_curr c2)
          [] ts_of_json
    end

    open Trade

    let trades ?since ?limit c1 c2 = Raw.trades c1 c2 >>= fun trades ->
      return @@ List.map (fun t ->
          create
            ~ts:(float t.Raw.timestamp)
            ~price:t.Raw.price
            ~qty:t.Raw.amount
            ~kind:(match t.Raw.type_ with
                | "bid" -> `Bid
                | "ask" -> `Ask
                | _ -> `Unknown) ()
        ) trades
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
      let curr_str = string_of_curr c2 ^ "_" ^ string_of_curr c1 in
      get "" params >>= fun s ->
      Yojson.Safe.from_string s |> function
      | `Assoc l ->
        (try let t = List.assoc curr_str l in
           (* Format.printf "%s@." (Yojson.Safe.to_string t); *)
           yojson_to_a t |> (function | `Ok r -> return r
                                      | `Error reason -> failwith reason)
         with Not_found -> failwith "Unknown currency")
      | _ -> failwith s

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

    type t = {
      last: float;
      bid: float;
      ask: float;
      percent_change: float;
      base_volume: float;
      quote_volume: float;
      is_frozen: bool;
      high: float;
      low: float;
    } [@@deriving show]

    let of_raw t = {
      last = float_of_string t.Raw.last;
      bid = float_of_string t.Raw.highestBid;
      ask = float_of_string t.Raw.lowestAsk;
      percent_change = float_of_string t.Raw.percentChange;
      base_volume = float_of_string t.Raw.baseVolume;
      quote_volume = float_of_string t.Raw.quoteVolume;
      is_frozen = (match t.Raw.isFrozen with "0" -> false | _ -> true);
      high = float_of_string t.Raw.high24hr;
      low = float_of_string t.Raw.low24hr;
    }

    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t -> return @@ of_raw t
  end

  module OrderBook = struct
    open OrderBook

    let book c1 c2 =
      let curr_str = string_of_curr c2 ^ "_" ^ string_of_curr c1 in
      get "" ["command", "returnOrderBook";
              "currencyPair", curr_str] >>= fun s ->
      Yojson.Safe.from_string s |> function
      | `Assoc ["asks", `List asks; "bids", `List bids; "isFrozen", `String frz] ->
        if frz <> "0" then return { asks = []; bids = [] }
        else
          return {
            asks = List.map (
                function
                | `List [`String price; `Float qty] ->
                  { price = float_of_string price; qty }
                | `List [`String price; `Int qty] ->
                  { price = float_of_string price; qty = float qty}
                | json -> invalid_arg @@ Yojson.Safe.to_string json
              ) asks;
            bids = List.map (
                function
                | `List [`String price; `Float qty] ->
                  { price = float_of_string price; qty }
                | `List [`String price; `Int qty] ->
                  { price = float_of_string price; qty = float qty}
                | json -> invalid_arg @@ Yojson.Safe.to_string json
              ) bids;
          }
      | json -> invalid_arg @@ Yojson.Safe.to_string json
  end
end

module Kraken (H: HTTP_CLIENT) = struct
  open H

  type 'a error_monad = {
    error: string list;
    result: 'a option [@default None];
  } [@@deriving yojson]

  let get endpoint params yojson_to_a =
    let lift_f = function
      | `Assoc [_, t] -> yojson_to_a t
      | _ -> invalid_arg "lift_f"
    in
    get endpoint params >>= fun s ->
    Yojson.Safe.from_string s |>
    error_monad_of_yojson lift_f |> function
    | `Ok { error = []; result = Some r } -> return r
    | `Ok { error; result = None } -> failwith (String.concat " " error)
    | `Error reason -> failwith reason
    | _ -> failwith "internal error"

  type supported_curr = [`BTC | `LTC | `DOGE]

  let string_of_curr = function
    | `BTC -> "XXBT"
    | `LTC -> "XLTC"
    | `DOGE -> "XXDG"

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

      let ticker c1 c2 = get "public/Ticker"
          ["pair", string_of_curr c1 ^ string_of_curr c2] of_yojson
    end

    type t = {
      bid: float;
      ask: float;
      vol: float;
      vwap: float;
      nb_trades: int;
      low: float;
      high: float;
    } [@@deriving show]

    let of_raw t = {
      bid = float_of_string @@ List.hd t.Raw.b;
      ask = float_of_string @@ List.hd t.Raw.a;
      vol = float_of_string @@ List.nth t.Raw.v 1;
      vwap = float_of_string @@ List.nth t.Raw.p 1;
      nb_trades = List.nth t.Raw.t 1;
      low = float_of_string @@ List.nth t.Raw.l 1;
      high = float_of_string @@ List.nth t.Raw.h 1;
    }

    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t -> return @@ of_raw t
  end

  module OrderBook = struct
    open OrderBook

    let book c1 c2 =
      let lift_f = function
        | `Assoc ["asks", `List asks; "bids", `List bids] ->
          `Ok ({ asks =
              List.map (function
                  | `List [`String price; `String qty; `Int ts] ->
                    create_order
                      ~price:(float_of_string price)
                      ~qty:(float_of_string qty) ()
                  | _ -> invalid_arg "get_orderbook"
                ) asks;

            bids =
              List.map (function
                  | `List [`String price; `String qty; `Int ts] ->
                    create_order
                      ~price:(float_of_string price)
                      ~qty:(float_of_string qty) ()
                  | _ -> invalid_arg "get_orderbook"
                ) bids;
          })
        | _ -> `Error "lift_f" in

      get "public/Depth"
        ["pair", string_of_curr c1 ^ string_of_curr c2] lift_f
  end
end

module Hitbtc (H: HTTP_CLIENT) = struct
  open H

  let get endpoint params yojson_to_a =
    get endpoint params >>= fun s ->
    Yojson.Safe.from_string s |>
    yojson_to_a |>
    function | `Ok r -> return r
             | `Error reason -> failwith reason

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

    type t = {
      ask: float;
      bid: float;
      last: float;
      low: float;
      high: float;
      o: float;
      volume: float;
      volume_quote: float;
      timestamp: int;
    } [@@deriving show]

    let of_raw t = {
      ask = float_of_string t.Raw.ask;
      bid = float_of_string t.Raw.bid;
      last = float_of_string t.Raw.last;
      low = float_of_string t.Raw.low;
      high = float_of_string t.Raw.high;
      o = float_of_string t.Raw.o;
      volume = float_of_string t.Raw.volume;
      volume_quote = float_of_string t.Raw.volume_quote;
      timestamp = t.Raw.timestamp;
    }

    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t -> return @@ of_raw t
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
    let of_raw t = {
      bids = List.map (function
          | [price; qty] ->
            OrderBook.create_order
              ~price:(float_of_string price)
              ~qty:(float_of_string qty) ()
          | _ -> invalid_arg "of_raw"
        ) t.Raw.bids;
      asks = List.map (function
          | [price; qty] ->
            OrderBook.create_order
              ~price:(float_of_string price)
              ~qty:(float_of_string qty) ()
          | _ -> invalid_arg "of_raw"
        ) t.Raw.asks;
    }

    let book c1 c2 = Raw.book c1 c2 >>= fun t -> return @@ of_raw t
  end
end
