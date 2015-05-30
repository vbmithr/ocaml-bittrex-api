open Core.Std
open Async.Std

open Bittrex_async

let log = Log.create ~level:`Info ~output:[(Log.Output.stderr ())]

let ignore_log label f =
  f () >>| function
  | `Ok _ -> Log.info log "Checked %s OK" label
  | `Error msg -> Log.info log "Checked %s ERROR: %s" label msg

type pair = [`BTCUSD]
type ticker = (int64, int64) Mt.ticker_with_vwap
type book_entry = int64 Mt.tick
type trade = (int64, int64) Mt.tick_with_direction_ts

type exchange =
  <
    name : string;
    ticker : pair -> [`Ok of ticker | `Error of string] Deferred.t;
    book : pair -> [`Ok of book_entry Mt.orderbook | `Error of string] Deferred.t;
    trades : ?since:int64 -> ?limit:int -> pair -> [`Ok of trade list | `Error of string] Deferred.t
  >

let obj_of_name = function
  | "bitfinex" -> (Bitfinex.exchange :> exchange)
  | "btce" -> (BTCE.exchange :> exchange)
  | "kraken" -> (Kraken.exchange :> exchange)
  | _ -> invalid_arg "module_of_name"

let run_tests e =
  let e = obj_of_name e in
  let pair = `BTCUSD in
  ignore_log (e#name ^ "::ticker") (fun () -> e#ticker pair) >>= fun () ->
  ignore_log (e#name ^ "::book") (fun () -> e#book pair) >>= fun () ->
  ignore_log (e#name^ "::trades") (fun () -> e#trades pair) >>= fun () ->
  Deferred.unit

let main exchanges =
  let tests = List.map exchanges ~f:run_tests in
  Deferred.all_unit tests >>= fun () ->
  Shutdown.shutdown 0;
  Deferred.unit

let _ =
  let exchanges = ref [] in
  let speclist = Arg.align [] in
  let anon_fun s = exchanges := s :: !exchanges in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ "  exchange exchanges...\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  don't_wait_for @@ main !exchanges;
  never_returns @@ Scheduler.go ()
