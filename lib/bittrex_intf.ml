open Rresult
open Mt

module type IO = sig
  include Cohttp.S.IO
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val all : 'a t list -> 'a list t
end

type err = [
  | `Exchange_error of string
  | `Json_error of string
  | `Internal_error of string
  | R.exn_trap
] [@@deriving show]

let exchange_error str = R.error (`Exchange_error str)
let json_error str = R.error (`Json_error str)
let internal_error str = R.error (`Internal_error str)

module type HTTP_CLIENT = sig
  include IO

  val get : string -> (string * string) list -> (string, err) result t
end

module Exchange = struct
  type t = [`Bitfinex | `BTCE | `Kraken] [@@deriving show, enum, eq, ord]

  let to_string = function
    | `Bitfinex -> "BITFINEX"
    | `BTCE -> "BTCE"
    | `Kraken -> "KRAKEN"

  let of_string s = String.lowercase s |> function
    | "bitfinex" | "`bitfinex" -> Some `Bitfinex
    | "btce" | "`btce" -> Some `BTCE
    | "kraken" | "`kraken" -> Some `Kraken
    | _ -> None
end

module Symbol = struct
  type t = [`XBTUSD | `LTCUSD | `LTCXBT | `XBTLTC] [@@deriving show, enum, eq, ord]

  let to_string = function
    | `XBTUSD -> "XBTUSD"
    | `LTCUSD -> "LTCUSD"
    | `LTCXBT -> "LTCXBT"
    | `XBTLTC -> "XBTLTC"

  let of_string s = String.lowercase s |> function
    | "xbtusd" | "`xbtusd" | "btcusd" -> Some `XBTUSD
    | "ltcusd" | "`ltcusd" -> Some `LTCUSD
    | "ltcxbt" | "`ltcxbt" | "ltcbtc" -> Some `LTCXBT
    | "xbtltc" | "`xbtltc" | "btcltc" -> Some `XBTLTC
    | _ -> None

  let descr = function
    | `XBTUSD -> "Bitcoin / US Dollar"
    | `LTCUSD -> "Litecoin / US Dollar"
    | `LTCXBT -> "Litecoin / Bitcoin"
    | `XBTLTC -> "Bitcoin / Litecoin"
end

(** Abstract exchange type. *)

module type EXCHANGE = sig
  include IO
  type symbol
  type ticker
  type book_entry
  type trade

  val kind : Exchange.t
  val accept : Symbol.t -> symbol option
  val symbols : symbol list

  val price_increment : int (** minimum increment of price in satoshis *)
  val trade_increment : int (** minimum tradable amount of satoshis *)

  val ticker : symbol -> (ticker, err) result t

  val book : symbol -> (book_entry OrderBook.t, err) result t

  val trades : ?since:int64 -> ?limit:int -> symbol ->  (trade list, err) result t
    (** [trades ?since ?limit symbol] is a thread that returns a list of
        trades contained in an error monad. If the underlying exchange
        supports it, the list will be limited to [limit] entries, and
        will not contain trades older than [since], where [since] is
        an unix timestamp in nanoseconds. *)
end

(** Concrete exchange types. *)

module type BITFINEX = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `LTCXBT]
   and type ticker = (int64, int64) Ticker.tvwap
   and type book_entry = int64 Mt.Tick.t
   and type trade = (int64, int64) Mt.Tick.tdts

module type BTCE = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `LTCXBT]
   and type ticker = (int64, int64) Ticker.tvwap
   and type book_entry = int64 Tick.t
   and type trade = (int64, int64) Mt.Tick.tdts

module type KRAKEN = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `XBTLTC]
   and type ticker = (int64, int64) Ticker.tvwap
   and type book_entry = (int64, int64) Tick.tts
   and type trade = < d : [ `Ask | `Bid | `Unset ];
                      kind : [ `Limit | `Market | `Unset ]; misc : string;
                      p : int64; ts : int64; v : int64 >

(** Generic exchange type. *)

module type GENERIC = sig
  include IO
  type ticker = (int64, int64) Ticker.tvwap
  type book_entry = int64 Mt.Tick.t
  type trade = (int64, int64) Mt.Tick.tdts

  val price_increment : [< Exchange.t] -> int
  val trade_increment : [< Exchange.t] -> int

  val symbols : [< Exchange.t] -> [> Symbol.t] list
  val ticker : symbol:[< Symbol.t] -> exchange:[< Exchange.t] ->
    (ticker, err) result t
  val book : symbol:[< Symbol.t] -> exchange:[< Exchange.t] ->
    (book_entry OrderBook.t, err) result t
  val trades : ?since:int64 -> ?limit:int -> symbol:[< Symbol.t] -> exchange:[< Exchange.t] ->
    unit -> (trade list, err) result t
end
