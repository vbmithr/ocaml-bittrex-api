open Rresult
open Core.Std
open Async.Std

open Mt
open Bittrex_async

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let ot_tif_of_string = function
  | "market" -> `Market, `Good_till_canceled
  | "limit" -> `Limit, `Good_till_canceled
  | "stop" -> `Stop, `Good_till_canceled
  | "fill-or-kill" -> `Limit, `Fill_or_kill
  | _ -> invalid_arg "ot_tif_of_string"

let new_order ~price ~amount ~symbol ~side ~ot_tif =
  let order_type, time_in_force = ot_tif_of_string ot_tif in
  let symbol =
    Option.(value_exn (Symbol.of_string symbol >>= Bitfinex.accept)) in
  let amount = satoshis_of_string_exn amount in
  let price = satoshis_of_string_exn price in
  new Order.t ~price ~amount ~symbol ~client_id:""
    ~side ~order_type ~time_in_force ()

let current_exchange = ref `Bitfinex
let current_creds = ref @@ create_credentials "" ""

let creds = CCError.get_exn @@
    Config.load @@ Sys.getenv_exn "HOME" ^ "/.exchanges-api-keys"

let main () =
  (* let key = Sys.getenv_exn "BFX_APIKEY" in *)
  (* let secret = Sys.getenv_exn "BFX_APISECRET" in *)
  (* let creds = create_credentials ~key ~secret in *)
  Log.info log "Welcome to the BFX Shell.";
  let rec read_loop () =
    Reader.(read_line Lazy.(force stdin)) >>= function
    | `Eof ->
      Log.info log "EOF received, exiting.";
      Shutdown.exit 0
    | `Ok msg ->
      let words = String.split msg ~on:' ' in
      (match List.hd_exn words with
       | "open" ->
         ((let open Option.Monad_infix in
         Exchange.of_string @@ List.nth_exn words 1 >>= fun exchange ->
         current_exchange := exchange;
         List.Assoc.find creds exchange >>| fun creds ->
         current_creds := creds; exchange) |> function
         | None ->
           Log.error log "Failure loading credentials"; read_loop ()
         | Some exchange ->
           Log.info log "Loaded credentials for %s" Exchange.(to_string exchange);
           read_loop ()
         )
       | "balance" ->
         (Generic.balance !current_creds !current_exchange >>| function
           | Ok balances ->
             List.iter
               ~f:(fun b -> Log.info log "%s: %Ld"
                      Currency.(to_string b#currency) b#available)
               balances
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "positions" ->
         (Generic.positions !current_creds !current_exchange >>| function
           | Ok positions ->
             List.iter ~f:(fun p -> Log.info log "%d" p#id) positions
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "orders" ->
         (Generic.orders !current_creds !current_exchange >>| function
           | Ok orders ->
             List.iter ~f:(fun o -> Log.info log "%d" o#exchange_order_id) orders
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       | "filled" ->
         (Generic.filled_orders !current_creds !current_exchange >>| function
           | Ok orders ->
             List.iter ~f:(fun o -> Log.info log "%Ld" o#tid) orders
           | Error err ->
             Log.error log "%s" @@ show_err err
         )
         >>= read_loop
       (* | "status" -> *)
       (*   let order_id = List.nth_exn words 1 |> int_of_string in *)
       (*   (Generic.order_status !current_creds !current_exchange order_id >>| function *)
       (*     | Ok status -> *)
       (*       Log.error log "Not impl." *)
       (*     | Error err -> *)
       (*       Log.error log "%s" @@ show_err err *)
       (*   ) *)
       (*   >>= read_loop *)
       (* | "buy" -> *)
       (*   let ot_tif = List.nth_exn words 1 in *)
       (*   let symbol = List.nth_exn words 2 in *)
       (*   let amount = List.nth_exn words 3 in *)
       (*   let price = List.nth_exn words 4 in *)
       (*   let order = new_order ~price ~amount ~symbol ~side:`Buy ~ot_tif *)
       (*   in *)
       (*   (Generic.new_order !current_creds !current_exchange order >>| function *)
       (*     | Ok status -> *)
       (*       Log.error log "Not impl." *)
       (*     | Error err -> *)
       (*       Log.error log "%s" @@ show_err err *)
       (*   ) *)
       (*   >>= read_loop *)
       (* | "sell" -> *)
       (*   let ot_tif = List.nth_exn words 1 in *)
       (*   let symbol = List.nth_exn words 2 in *)
       (*   let amount = List.nth_exn words 3 in *)
       (*   let price = List.nth_exn words 4 in *)
       (*   let order = *)
       (*     new_order ~price ~amount ~symbol ~side:`Sell ~ot_tif *)
       (*   in *)
       (*   (Generic.new_order !current_creds !current_exchange  order >>| function *)
       (*     | Ok status -> *)
       (*       Log.error log "Not impl." *)
       (*     | Error err -> *)
       (*       Log.error log "%s" @@ show_err err *)
       (*   ) *)
       (*   >>= read_loop *)
       (* | "cancel" -> *)
       (*   let order_id = List.nth_exn words 1 |> int_of_string in *)
       (*   (Generic.cancel_order !current_creds !current_exchange order_id >>| function *)
       (*     | Ok status -> *)
       (*       Log.error log "Not impl." *)
       (*     | Error err -> *)
       (*       Log.error log "%s" @@ show_err err *)
       (*   ) *)
       (*   >>= read_loop *)
       | command ->
         Log.info log "Unsupported command: %s" command;
         read_loop ()
      )
  in
  read_loop ()

let () =
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
