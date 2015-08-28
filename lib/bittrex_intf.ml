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
  type position
  type order
  type order_status
  type filled_order_status

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

  val balance : credentials -> (< currency : Currency.t;
                                  balance : int64;
                                  available : int64;
                                  reserved : int64;
                                > list, err) result t

  val positions : credentials -> (position list, err) result t
  val orders : credentials -> (order_status list, err) result t

  (* val filled_orders : ?after:int64 -> ?before:int64 -> credentials -> *)
  (*   (filled_order_status list, err) result t *)
  (* val new_order : credentials -> order -> (int, err) result t *)
  (* val order_status : credentials -> int -> (order_status, err) result t *)
  (* val cancel_order : credentials -> int -> (unit, err) result t *)
end

(** Concrete exchange types. *)

module type BITFINEX = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `LTCXBT]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = int64 Tick.T.t
   and type trade =
         < p : int64; v : int64; side : [`Buy | `Sell]; ts : int64 >
   and type position =
         < id : int; p : int64; pl : int64;
           status : [`Active | `Unset ];
           swap : int64; symbol : [`XBTUSD | `LTCUSD | `LTCXBT];
           ts : int64; v : int64 >
   and type filled_order_status =
         < fee_amount : int64; fee_currency : Mt.Currency.t;
           order_id : string; p : int64; side : [ `Buy | `Sell ];
           symbol: [`XBTUSD | `LTCUSD | `LTCXBT];
           tid : string; ts : int64; v : int64 >
   and type order_status =
         < avg_fill_price : int64;
           exchange : Exchange.t;
           exchange_order_id : string;
           expire_ts : int64;
           filled_qty : int64;
           order_qty : int64;
           order_type : [ `Limit | `Market | `Market_if_touched | `Stop | `Stop_limit ];
           p1 : int64;
           p2 : int64;
           side : [ `Buy | `Sell ];
           symbol : Symbol.t;
           time_in_force : [ `Fill_or_kill | `Good_till_canceled | `Good_till_date_time ]
         >

module type BITSTAMP = EXCHANGE
  with type symbol = [`XBTUSD]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = int64 Tick.T.t
   and type trade = < p : int64; v : int64; side : [`Buy | `Sell]; ts : int64 >


module type BTCE = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `XBTEUR | `LTCEUR | `LTCXBT]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = int64 Tick.T.t
   and type trade = < p : int64; v : int64; side : [`Buy | `Sell]; ts : int64 >

module type KRAKEN = EXCHANGE
  with type symbol = [`XBTUSD | `LTCUSD | `XBTEUR | `LTCEUR | `XBTLTC]
   and type ticker = (int64, int64) Ticker.Tvwap.t
   and type book_entry = (int64, int64) Tick.TTS.t
   and type trade =
         < side : [ `Buy | `Sell ];
           order_type : [ `Limit | `Market | `Unset ]; misc : string;
           p : int64; ts : int64; v : int64 >
   and type filled_order_status =
         < fee_amount : int64; fee_currency : Mt.Currency.t;
           order_id : string; p : int64; side : [ `Buy | `Sell ];
           symbol: [`XBTUSD | `LTCUSD | `XBTEUR | `LTCEUR | `XBTLTC];
           tid : string; ts : int64; v : int64 >
   and type order_status =
         < avg_fill_price : int64;
           exchange : Exchange.t;
           exchange_order_id : string;
           expire_ts : int64;
           filled_qty : int64;
           order_qty : int64;
           order_type : [ `Limit | `Market | `Market_if_touched | `Stop | `Stop_limit ];
           p1 : int64;
           p2 : int64;
           side : [ `Buy | `Sell ];
           symbol : Symbol.t;
           time_in_force : [ `Good_till_canceled | `Good_till_date_time ]
         >

(** Generic exchange type. *)

module type GENERIC = sig
  include IO
  type ticker = (int64, int64) Ticker.Tvwap.t
  type book_entry = int64 Tick.T.t
  type trade = < p : int64; v : int64; side : [`Buy | `Sell]; ts : int64 >

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
    exchange:Exchange.t -> (< currency : Currency.t;
                              balance : int64;
                              available : int64;
                              reserved : int64;
                            > list, err) result t

  val orders : credentials ->
    exchange:Exchange.t ->
    (< avg_fill_price : int64;
       exchange : Exchange.t;
       exchange_order_id : string;
       expire_ts : int64;
       filled_qty : int64;
       order_qty : int64;
       order_type : [ `Limit | `Market | `Market_if_touched | `Stop | `Stop_limit ];
       p1 : int64;
       p2 : int64;
       side : [ `Buy | `Sell ];
       symbol : Symbol.t;
       time_in_force : [ `Fill_or_kill | `Good_till_canceled | `Good_till_date_time ]
     > list, err) result t

  val positions : credentials ->
    exchange:Exchange.t ->
    (< id: int; p:int64; v:int64; symbol: Symbol.t > list, err) result t

  val filled_orders : ?after:int64 -> ?before:int64 -> credentials ->
    exchange:Exchange.t ->
    (< order_id : string; p : int64; side : [ `Buy | `Sell ];
       symbol: Symbol.t;
       tid : string; ts : int64; v : int64 > list, err) result t
end
