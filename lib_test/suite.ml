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

let run_tests syms (e, cred) =
  Deferred.List.iter syms ~how:`Sequential ~f:(fun s ->
      let e_str = Exchange.show e in
      let s_str = Symbol.show s in
      ignore_log (e_str ^ "::" ^ s_str ^ "::ticker")
        (fun () -> Generic.ticker s e) >>= fun () ->
      ignore_log (e_str ^ "::" ^ s_str ^ "::book")
        (fun () -> Generic.book s e) >>= fun () ->
      ignore_log (e_str ^ "::" ^ s_str ^ "::trades")
        (fun () -> Generic.trades s e ()) >>= fun () ->
      match cred with
      | None -> Deferred.unit
      | Some cred ->
        ignore_log (e_str ^ "::" ^ s_str ^ "::balance")
          (fun () -> Generic.balance cred e)
    )

let main exchanges symbols =
  let exchanges = List.filter_map exchanges ~f:Exchange.of_string in
  let symbols = List.filter_map symbols ~f:Symbol.of_string in
  let creds = Config.load @@ Sys.getenv_exn "HOME" ^ "/.exchanges-api-keys" |> function
    | `Error str ->
      Log.error log "Unable to load credentials: %s" str;
      []
    | `Ok creds ->
      Log.info log "Succesfully loaded credentials";
      creds in
  let exchanges =
    List.map exchanges ~f:(fun e -> Config.(e, List.Assoc.find creds e)) in
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
