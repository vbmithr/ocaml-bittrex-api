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

type exchanges = [`Bitfinex | `BTCE | `Kraken] [@@deriving show]
type pairs = [`XBTUSD | `LTCXBT | `XBTLTC] [@@deriving show]

(** Abstract exchange type. *)

module type EXCHANGE = sig
  include IO
  type pair
  type ticker
  type book_entry
  type trade

  val name : string
  val accept :pairs -> pair option
  val pairs : pair list
  val pair_of_string : string -> pair option

  val ticker : pair -> (ticker, err) result t

  val book : pair ->    (book_entry OrderBook.t, err) result t

  val trades : ?since:int64 -> ?limit:int -> pair ->  (trade list, err) result t
    (** [trades ?since ?limit pair] is a thread that returns a list of
        trades contained in an error monad. If the underlying exchange
        supports it, the list will be limited to [limit] entries, and
        will not contain trades older than [since], where [since] is
        an unix timestamp in nanoseconds. *)
end

(** Concrete exchange types. *)

module type BITFINEX = EXCHANGE
  with type pair = [`XBTUSD | `LTCXBT]
   and type ticker = (int64, int64) Ticker.tvwap
   and type book_entry = int64 Mt.Tick.t
   and type trade = (int64, int64) Mt.Tick.tdts

module type BTCE = EXCHANGE
  with type pair = [`XBTUSD | `LTCXBT]
   and type ticker = (int64, int64) Ticker.tvwap
   and type book_entry = int64 Tick.t
   and type trade = (int64, int64) Mt.Tick.tdts

module type KRAKEN = EXCHANGE
  with type pair = [`XBTUSD | `XBTLTC]
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

  val ticker : [< pairs] -> [< exchanges] -> (ticker, err) result t
  val book : [< pairs] -> [< exchanges] -> (book_entry OrderBook.t, err) result t
  val trades : ?since:int64 -> ?limit:int -> [< pairs] -> [< exchanges] ->
    (trade list, err) result t
end
