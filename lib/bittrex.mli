module type HTTP_CLIENT = sig
  include Cohttp.S.IO

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t

  (* val post : Credentials.t -> string -> (string * string) list -> *)
  (*   (string -> [< `Error of string | `Ok of 'a ]) -> 'a t *)
end

module API(H: HTTP_CLIENT) : sig
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
