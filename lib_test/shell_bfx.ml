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
       | "balance" ->
         (Bitfinex.balance creds >>| function
           | Ok balances ->
             List.iter
               ~f:(fun b -> Log.info log "%s" @@ Balance.show Int64.pp b)
               balances
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "positions" ->
         (Bitfinex.positions creds >>| function
           | Ok positions ->
             Log.error log "Not impl."
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "status" ->
         let order_id = List.nth_exn words 1 |> int_of_string in
         (Bitfinex.order_status creds order_id >>| function
           | Ok status ->
             Log.error log "Not impl."
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "buy" ->
         let symbol =
           Option.(List.nth_exn words 1 |> Symbol.of_string >>= Bitfinex.accept) in
         let symbol = Option.value_exn symbol in
         let amount = List.nth_exn words 2 |> satoshis_of_string_exn in
         let price = List.nth_exn words 3 |> satoshis_of_string_exn in
         let order = new Order.t ~price ~amount ~symbol ~client_id:""
           ~direction:`Buy ~order_type:`Limit ~time_in_force:`Good_till_canceled ()
         in
         (Bitfinex.new_order creds order >>| function
           | Ok status ->
             Log.error log "Not impl."
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "sell" ->
         let symbol =
           Option.(List.nth_exn words 1 |> Symbol.of_string >>= Bitfinex.accept) in
         let symbol = Option.value_exn symbol in
         let amount = List.nth_exn words 2 |> satoshis_of_string_exn in
         let price = List.nth_exn words 3 |> satoshis_of_string_exn in
         let order = new Order.t ~price ~amount ~symbol ~client_id:""
           ~direction:`Sell ~order_type:`Limit ~time_in_force:`Good_till_canceled ()
         in
         (Bitfinex.new_order creds order >>| function
           | Ok status ->
             Log.error log "Not impl."
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
