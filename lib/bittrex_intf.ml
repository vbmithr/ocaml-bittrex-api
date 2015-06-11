open Mt

module type IO = sig
  include Cohttp.S.IO
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val all : 'a t list -> 'a list t
end

module type HTTP_CLIENT = sig
  include IO

  val get : string -> (string * string) list -> [`Ok of string | `Error of string] t
end

type exchanges = [`Bitfinex | `BTCE | `Kraken]
type pairs = [`XBTUSD | `LTCXBT | `XBTLTC]

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

  val ticker : pair ->
    [`Ok of ticker | `Error of string] t

  val book : pair ->
    [`Ok of book_entry OrderBook.t | `Error of string] t

  val trades : ?since:int64 -> ?limit:int -> pair ->
    [`Ok of trade list | `Error of string] t
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

  val ticker : pairs -> exchanges -> (ticker, string) CCError.t t
  val book : pairs -> exchanges -> (book_entry OrderBook.t, string) CCError.t t
  val trades : ?since:int64 -> ?limit:int -> pairs -> exchanges ->
    (trade list, string) CCError.t t
end
