open Rresult
open Core.Std
open Async.Std

open Mt
open Bittrex_async

let log = Log.create ~on_error:`Raise
    ~level:`Info ~output:[(Log.Output.stderr ())]

let ignore_log label f =
  f () >>| function
  | Rresult.Ok _ -> Log.info log "Checked %s OK" label
  | Rresult.Error err -> Log.info log "Checked %s: %s" label Bittrex_intf.(show_err err)

let run_tests syms e =
  Deferred.List.iter syms ~how:`Sequential ~f:(fun s ->
      let e = Option.value_exn (Exchange.of_string e) in
      let e_str = Exchange.show e in
      let s = Option.value_exn (Symbol.of_string s) in
      let s_str = Symbol.show s in
      ignore_log (e_str ^ "::" ^ s_str ^ "::ticker")
        (fun () -> Generic.ticker s e) >>= fun () ->
      ignore_log (e_str ^ "::" ^ s_str ^ "::book")
        (fun () -> Generic.book s e) >>= fun () ->
      ignore_log (e_str ^ "::" ^ s_str ^ "::trades")
        (fun () -> Generic.trades s e ())
    )

let main exchanges symbols =
  let tests = List.map exchanges ~f:(run_tests symbols) in
  Deferred.all_unit tests >>= fun () ->
  Shutdown.shutdown 0;
  Deferred.unit

let _ =
  let exchanges = ref [] in
  let symbols = ref [] in
  let speclist = Arg.(align [
      "-e", String (fun e -> exchanges := e :: !exchanges), "<string> name of an exchange to test";
      "-s", String (fun s -> symbols := s :: !symbols), "<string> name of a symbol to test";
    ]) in
  let anon_fun = Fn.ignore in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ "[options]\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  don't_wait_for @@ main !exchanges !symbols;
  never_returns @@ Scheduler.go ()
