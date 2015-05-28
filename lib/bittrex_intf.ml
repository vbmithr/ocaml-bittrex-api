module type HTTP_CLIENT = sig
  include Cohttp.S.IO
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  val get : string -> (string * string) list -> [`Ok of string | `Error of string] t
end

module type EXCHANGE = sig
  type 'a io
  type pair

  val name : string
  val pairs : pair list

  module Ticker : sig
    type t = {
      last: int64;
      bid: int64;
      ask: int64;
      high: int64;
      low: int64;
      volume: int64;
      timestamp: int64; (* unix time in us *)
      vwap: int64 option;
    } [@@deriving show,create]

    val ticker : pair -> [`Ok of t | `Error of string] io
  end

  module OrderBook : sig
    type 'a book = {
      bids: 'a list;
      asks: 'a list;
    } [@@deriving show]

    type order = {
      price: int64;
      qty: int64;
    } [@@deriving show,create]

    type t = order book [@@deriving show]

    val book : pair -> [`Ok of t | `Error of string] io
  end

  module Trade : sig
    type kind = [`Ask | `Bid | `Unknown] [@@deriving show]

    type t = {
      ts: int64; (* unix time in us *)
      price: int64;
      qty: int64;
      kind: kind;
    } [@@deriving show,create]

    val trades : ?since:int64 -> ?limit:int -> pair -> [`Ok of t list | `Error of string] io
  end
end
