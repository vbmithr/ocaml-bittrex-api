open Rresult
open Core.Std
open Async.Std

open Mt
open Bittrex_async

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let main () =
  let key = Sys.getenv_exn "BFX_APIKEY" in
  let secret = Sys.getenv_exn "BFX_APISECRET" in
  let creds = create_credentials ~key ~secret in
  Log.info log "Welcome to the BFX Shell.";
  let rec read_loop () =
    Reader.(read_line Lazy.(force stdin)) >>= function
    | `Eof ->
      Log.info log "EOF received, exiting.";
      Shutdown.exit 0
    | `Ok msg ->
      let words = String.split msg ~on:' ' in
      (match List.hd_exn words with
       | "BALANCE" ->
         (Bitfinex.balance creds >>| function
           | Ok balances ->
             List.iter
               ~f:(fun b -> Log.info log "%s" @@ Balance.show Int64.pp b)
               balances
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | command ->
         Log.info log "Unsupported command: %s" command;
         read_loop ()
      )
  in
  read_loop ()

let () =
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
