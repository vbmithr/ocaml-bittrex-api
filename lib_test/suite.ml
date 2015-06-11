open Core.Std
open Async.Std

open Mt
open Bittrex_async

let log = Log.create ~level:`Info ~output:[(Log.Output.stderr ())]

let ignore_log label f =
  f () >>| function
  | `Ok _ -> Log.info log "Checked %s OK" label
  | `Error msg -> Log.info log "Checked %s ERROR: %s" label msg

let run_tests e =
  let pair = `XBTUSD in
  ignore_log (e ^ "::ticker") (fun () -> Generic.ticker pair e) >>= fun () ->
  ignore_log (e ^ "::book") (fun () -> Generic.book pair e) >>= fun () ->
  ignore_log (e^ "::trades") (fun () -> Generic.trades pair e) >>= fun () ->
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
