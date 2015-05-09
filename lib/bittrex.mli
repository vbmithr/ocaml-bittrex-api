module type HTTP_CLIENT = sig
  include Cohttp.S.IO

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t

  (* val post : Credentials.t -> string -> (string * string) list -> *)
  (*   (string -> [< `Error of string | `Ok of 'a ]) -> 'a t *)
end

module Bitfinex (H: HTTP_CLIENT) : sig
  module Ticker : sig
    type t = {
      mid : float;
      bid : float;
      ask : float;
      last_price : float;
      low : float;
      high : float;
      volume : float;
      timestamp : float;
    } [@@deriving show,yojson]

    val ticker : string -> t H.t
    (** [ticker currency_pair] returns the ticker for the given
        [currency_pair]. *)
  end

  module OrderBook : sig
    type 'a book = {
      bids: 'a list;
      asks: 'a list;
    } [@@deriving show,yojson]

    type order = {
      price: float;
      amount: float;
      timestamp: float;
    } [@@deriving show,yojson]

    type t = order book

    val book : string -> t H.t
  end
end

module Bittrex (H: HTTP_CLIENT) : sig
  module Market : sig
    type t = {
      market_currency: string;
      base_currency: string;
      market_currency_long: string;
      base_currency_long: string;
      min_trade_size: float;
      market_name: string;
      is_active: bool;
      created: string;
      notice: string option;
      is_sponsored: bool option;
      logo_url: string option;
    } [@@deriving show,yojson]

    val markets : unit -> t list H.t
  end

  module MarketSummary : sig
    type t = {
      market_name: string;
      high: float;
      low: float;
      volume: float;
      last: float;
      base_volume: float;
      timestamp: string;
      bid: float;
      ask: float;
      open_buy_orders: int;
      open_sell_orders: int;
      prev_day: float;
      created: string;
    } [@@deriving show,yojson]

    val summaries : unit -> t list H.t
    val summary : string -> t list H.t
  end

  module Ticker : sig
    type t = {
      bid: float;
      ask: float;
      last: float;
    } [@@deriving show,yojson]

    val ticker : string -> t H.t
    (** [ticker currency_pair] returns the ticker for the given
        [currency_pair]. *)
  end

  module Currency : sig
    type t = {
      currency: string;
      currency_long: string;
      min_confirmation: int;
      tx_fee: float;
      is_active: bool;
      coin_type: string;
      base_addr: string option;
      notice: string option;
    } [@@deriving show,yojson]

    val currencies : unit -> t list H.t
  end

  module OrderBook : sig
    type order = {
      qty: float;
      price: float;
    } [@@deriving show,yojson]

    type book = {
      buy: order list;
      sell: order list
    } [@@deriving show,yojson]

    val book : string -> book H.t
  end
end

module Cryptsy (H: HTTP_CLIENT) : sig
  module Currency : sig
    type t = {
      id: int;
      name: string;
      code: string;
      maintenance: int;
    } [@@deriving show,yojson]

    val currencies : unit -> t list H.t
  end

  module Market : sig
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

    val markets : unit -> t list H.t
  end

  module Ticker : sig
    type t = {
      id: int;
      bid: float;
      ask: float;
    } [@@deriving show,yojson]

    val tickers : unit -> t list H.t
  end
end

module BTCE (H: HTTP_CLIENT): sig
  module Ticker : sig
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

    val ticker : string -> t H.t
  end
end
