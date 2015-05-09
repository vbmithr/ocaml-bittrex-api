module type HTTP_CLIENT = sig
  include Cohttp.S.IO

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t

  (* val post : Credentials.t -> string -> (string * string) list -> *)
  (*   (string -> [< `Error of string | `Ok of 'a ]) -> 'a t *)
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
    let ts_of_json ts =
      try
        match ts with
        | `List ts ->
            begin
              try
                let ts = List.map
                    (fun t -> match T.of_yojson t with
                       | `Ok a -> a
                       | `Error s -> failwith s) ts
                in `Ok ts
              with Failure s -> `Error s
            end
        | _ -> `Error "Not a json array."
      with exn -> `Error (Printexc.to_string exn)
  end
end

module Bitfinex (H: HTTP_CLIENT) = struct
  open H

  type supported_curr = [`BTC | `LTC]

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

      let string_of_curr = function
        | `BTC -> "BTC"
        | `LTC -> "LTC"

      let ticker c1 c2 = get
          ("pubticker/" ^ string_of_curr c1 ^ string_of_curr c2) [] of_yojson
    end

    type t = {
      mid: float;
      bid: float;
      ask: float;
      last_price: float;
      low: float;
      high: float;
      volume: float;
      timestamp: float;
    } [@@deriving show,yojson]

    let of_raw r =
      {
        mid = float_of_string r.Raw.mid;
        bid = float_of_string r.Raw.bid;
        ask = float_of_string r.Raw.ask;
        last_price = float_of_string r.Raw.last_price;
        low = float_of_string r.Raw.low;
        high = float_of_string r.Raw.high;
        volume = float_of_string r.Raw.volume;
        timestamp = float_of_string r.Raw.timestamp;
      }
    let ticker c1 c2 = Raw.ticker c1 c2 >>= fun t -> return @@ of_raw t
  end

  module OrderBook = struct
    type 'a book = {
      bids: 'a list;
      asks: 'a list;
    } [@@deriving show,yojson]

    module Raw = struct
      module T = struct
        type order = {
          price: string;
          amount: string;
          timestamp: string;
        } [@@deriving show,yojson]

        type t = order book [@@deriving show,yojson]
      end
      include T
      include Stringable.Of_jsonable(T)

      let book pair = get ("book/" ^ pair) [] of_yojson
    end

    type order = {
      price: float;
      amount: float;
      timestamp: float;
    } [@@deriving show,yojson]

    type t = order book

    let of_raw r =
      {
        price = float_of_string r.Raw.price;
        amount = float_of_string r.Raw.amount;
        timestamp = float_of_string r.Raw.timestamp;
      }

    let book pair = Raw.book pair >>= fun { bids; asks; } ->
      return @@ { bids = List.map of_raw bids; asks = List.map of_raw asks }
  end
end

module Bittrex (H: HTTP_CLIENT) = struct
  open H

  type supported_curr = [`BTC | `LTC | `DOGE]

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

      let string_of_curr = function
        | `BTC -> "BTC"
        | `LTC -> "LTC"
        | `DOGE -> "DOGE"

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
    type order = {
      qty [@key "Quantity"] : float;
      price [@key "Rate"] : float;
    } [@@deriving show,yojson]

    type book = {
      buy: order list;
      sell: order list
    } [@@deriving show,yojson]

    let book pair = get "public/getorderbook"
        ["market", pair; "type", "both"; "depth", "50"] book_of_yojson
  end
end

module Cryptsy (H: HTTP_CLIENT) = struct
  open H

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

  type supported_curr = [`BTC | `LTC]

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

      let string_of_curr = function
        | `BTC -> "btc"
        | `LTC -> "ltc"

      let ticker c1 c2 = get
          ("ticker/" ^ string_of_curr c1 ^ "_" ^ string_of_curr c2) [] of_yojson
    end
    include Raw
  end
end

module Poloniex (H: HTTP_CLIENT) = struct
  open H

  type supported_curr = [`BTC | `LTC | `DOGE]

  module Ticker = struct
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

      let string_of_curr = function
        | `BTC -> "BTC"
        | `LTC -> "LTC"
        | `DOGE -> "DOGE"

      let ticker c1 c2 = get (string_of_curr c2 ^ "_" ^ string_of_curr c1)
          ["command", "returnTicker"] of_yojson
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
end

module Kraken (H: HTTP_CLIENT) = struct
  open H

  type supported_curr = [`BTC | `LTC | `DOGE]

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

      let string_of_curr = function
        | `BTC -> "XXBT"
        | `LTC -> "XLTC"
        | `DOGE -> "XXDG"

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
end
