open Core.Std
open Async.Std

module B = Bittrex
module BA = Bittrex_async

module BFX = B.Bitfinex(BA.Bitfinex)
module Bittrex = B.Bittrex(BA.Bittrex)
module BTCE = B.BTCE(BA.BTCE)
module Poloniex = B.Poloniex(BA.Poloniex)
module Kraken = B.Kraken(BA.Kraken)
module Hitbtc = B.Hitbtc(BA.Hitbtc)

let main () =
  Format.printf "Checking BFX@.";
  BFX.Ticker.ticker `LTC `BTC >>= fun _ ->
  BFX.OrderBook.book `LTC `BTC >>= fun _ ->
  Format.printf "Checking Bittrex@.";
  Bittrex.Ticker.ticker `LTC `BTC >>= fun _ ->
  Bittrex.OrderBook.book `LTC `BTC >>= fun _ ->
  Format.printf "Checking BTCE@.";
  BTCE.Ticker.ticker `LTC `BTC >>= fun _ ->
  BTCE.OrderBook.book `LTC `BTC >>= fun _ ->
  Format.printf "Checking Poloniex@.";
  Poloniex.Ticker.ticker `LTC `BTC >>= fun _ ->
  Poloniex.OrderBook.book `LTC `BTC >>= fun _ ->
  Format.printf "Checking Kraken@.";
  Kraken.Ticker.ticker `BTC `LTC >>= fun _ ->
  Kraken.OrderBook.book `BTC `LTC >>= fun _ ->
  Format.printf "Checking Hitbtc@.";
  Hitbtc.Ticker.ticker `LTC `BTC >>= fun _ ->
  Hitbtc.OrderBook.book `LTC `BTC >>= fun _ ->
  (* Format.printf "%a@." Poloniex.Ticker.pp p; *)
  Shutdown.shutdown 0;
  Deferred.unit

let _ =
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
