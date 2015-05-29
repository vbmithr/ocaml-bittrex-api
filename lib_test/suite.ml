open Core.Std
open Async.Std

open Bittrex_async

let log = Log.create ~level:`Info ~output:[(Log.Output.stderr ())]

let ignore_log label f =
  f () >>| function
  | `Ok _ -> Log.info log "Checked %s OK" label
  | `Error msg -> Log.info log "Checked %s ERROR: %s" label msg

module type MINIASYNC = Bittrex_intf.Minimum.S with type 'a io := 'a Deferred.t

let module_of_name = function
  | "bitfinex" -> (module Bitfinex : MINIASYNC)
  | "btce" -> (module BTCE : MINIASYNC)
  (* | "kraken" -> (module Kraken : MINIASYNC) *)
  | _ -> invalid_arg "module_of_name"

let run_tests exchange =
  let exchange = module_of_name exchange in
  let module E = (val exchange : MINIASYNC) in
  let pair = List.hd_exn E.pairs in
  ignore_log (E.name ^ "::ticker") (fun () -> E.ticker pair) >>= fun () ->
  ignore_log (E.name ^ "::book") (fun () -> E.book pair) >>= fun () ->
  ignore_log (E.name^ "::trades") (fun () -> E.trades pair) >>= fun () ->
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
