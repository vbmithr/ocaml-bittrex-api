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
  | `Unsupported_by_exchange
  | `Not_implemented
  | R.exn_trap
] [@@deriving show]

let exchange_error str = R.error (`Exchange_error str)
let json_error str = R.error (`Json_error str)
let internal_error str = R.error (`Internal_error str)
let unsupported = R.error `Unsupported_by_exchange
let not_implemented = R.error `Not_implemented

let satoshis_of_float_exn f =
  let s = Printf.sprintf "%.8f" f in
  let i = String.index s '.' in
  let a = Int64.of_string @@ String.sub s 0 i in
  let b = Int64.of_string @@ String.sub s (i+1) (String.length s - i - 1) in
  Int64.(add b @@ mul a 100_000_000L)

let satoshis_of_string_exn s =
  satoshis_of_float_exn @@ float_of_string s

type credentials = { key: Cstruct.t; secret: Cstruct.t }
let create_credentials ~key ~secret =
  let key = Cstruct.of_string key in
  let secret = Cstruct.of_string secret in
  { key; secret; }

module type HTTP_CLIENT = sig
  include IO

  val get : endp:string -> params:(string * string) list -> (string, err) result t
  val post : creds:credentials -> endp:string -> body:string -> (string, err) result t
end

module Exchange = struct
  type t = [
    | `Bitfinex [@name "bitfinex"]
    | `Bitstamp [@name "bitstamp"]
    | `BTCE [@name "btce"]
    | `Kraken [@name "kraken"]
    | `OKCoin [@name "okcoin"]
    | `Coinbase [@name "coinbase"]
  ] [@@deriving show, enum, eq, ord, yojson]

  let to_string = function
    | `Bitfinex -> "BITFINEX"
    | `Bitstamp -> "BITSTAMP"
    | `BTCE -> "BTCE"
    | `Kraken -> "KRAKEN"
    | `OKCoin -> "OKCOIN"
    | `Coinbase -> "COINBASE"

  let of_string s = String.lowercase s |> function
    | "bitfinex" | "`bitfinex" -> Some `Bitfinex
    | "bitstamp" | "`bitstamp" -> Some `Bitstamp
    | "btce" | "`btce" -> Some `BTCE
    | "kraken" | "`kraken" -> Some `Kraken
    | "okcoin" | "`okcoin" -> Some `OKCoin
    | "coinbase" | "`coinbase" -> Some `Coinbase
    | _ -> None

  let of_string_exn s = match of_string s with
    | None -> invalid_arg "Exchange.of_string_exn"
    | Some e -> e
end

module Config = struct
  type cred = {
    exchange: Exchange.t;
    key: string;
    secret: string
  } [@@deriving yojson]

  type t = cred list [@@deriving yojson]

  let load fn =
    CCError.map
      (List.map (fun {exchange; key; secret} ->
           exchange, Cstruct.{key = of_string key;
                              secret = of_string secret})) @@
      CCIO.with_in fn (fun ic ->
          of_yojson @@ Yojson.Safe.from_channel ic
      )
end


(** Abstract exchange type. *)

module type EXCHANGE = sig
  include IO
  type symbol
  type exchange
  type ticker
  type book_entry
  type trade
  type order_types
  type time_in_force
  type position
  type order = (int64, symbol, order_types, time_in_force) Order.t
  type order_status = (int64, symbol, exchange, order_types, time_in_force, int64) Order.status

  (* val kind : Exchange.t *)
  val accept : Symbol.t -> symbol option
  val symbols : symbol list

  val price_increment : int (** minimum increment of price in satoshis *)
  val trade_increment : int (** minimum tradable amount of satoshis *)

  val ticker : symbol -> (ticker, err) result t

  val book : symbol -> (book_entry list * book_entry list, err) result t

  val trades : ?since:int64 -> ?limit:int -> symbol ->  (trade list, err) result t
    (** [trades ?since ?limit symbol] is a thread that returns a list of
        trades contained in an error monad. If the underlying exchange
        supports it, the list will be limited to [limit] entries, and
        will not contain trades older than [since], where [since] is
        an unix timestamp in nanoseconds. *)

  val balance : credentials -> (int64 Balance.t list, err) result t
  val positions : credentials -> (position list, err) result t
  val orders : credentials -> (order_status list, err) result t
  val new_order : credentials -> order -> (int, err) result t
  val order_status : credentials -> int -> (order_status, err) result t
  val cancel_order : credentials -> int -> (unit, err) result t
end

(** Concrete exchange types. *)

module type BITFINEX = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `LTCXBT]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = int64 Tick.T.t
   and type trade = (int64, int64) Tick.TDTS.t
   and type order_types = [`Market | `Limit | `Stop]
   and type time_in_force = [ `Good_till_canceled | `Fill_or_kill]
   and type position = < id : int; p : int64; pl : int64;
                         status : [`Active | `Unset ];
                         swap : int64; symbol : [`XBTUSD | `LTCUSD | `LTCXBT];
                         ts : int64; v : int64 >

module type BITSTAMP = EXCHANGE
  with type symbol = [`XBTUSD]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = int64 Tick.T.t
   and type trade = (int64, int64) Tick.TDTS.t
   and type order_types = [`Limit]
   and type time_in_force = [`Good_till_canceled]

module type BTCE = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `XBTEUR | `LTCEUR | `LTCXBT]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = int64 Tick.T.t
   and type trade = (int64, int64) Tick.TDTS.t
   and type order_types = [`Limit]
   and type time_in_force = [`Good_till_canceled]

module type KRAKEN = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `XBTEUR | `LTCEUR | `XBTLTC]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = (int64, int64) Tick.TTS.t
   and type trade = < d : [ `Ask | `Bid | `Unset ];
                      kind : [ `Limit | `Market | `Unset ]; misc : string;
                      p : int64; ts : int64; v : int64 >
   and type order_types = [`Market | `Limit]
   and type time_in_force = [`Good_till_canceled]

(** Generic exchange type. *)

module type GENERIC = sig
  include IO
  type ticker = (int64, int64) Ticker.Tvwap.t
  type book_entry = int64 Tick.T.t
  type trade = (int64, int64) Tick.TDTS.t

  val price_increment : [< Exchange.t] -> int
  val trade_increment : [< Exchange.t] -> int

  val symbols : [< Exchange.t] -> [> Symbol.t] list
  val ticker : symbol:[< Symbol.t] -> exchange:[< Exchange.t] ->
    (ticker, err) result t
  val book : symbol:[< Symbol.t] -> exchange:[< Exchange.t] ->
    (book_entry list * book_entry list, err) result t
  val trades : ?since:int64 -> ?limit:int -> symbol:[< Symbol.t] ->
    exchange:[< Exchange.t] ->  unit -> (trade list, err) result t

  val balance : credentials ->
    exchange:Exchange.t -> (int64 Balance.t list, err) result t
end
