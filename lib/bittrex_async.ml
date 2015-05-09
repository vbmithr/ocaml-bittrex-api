open Async.Std
open Cohttp_async
open Bittrex

module type ASYNC_HTTP_CLIENT = sig
  include Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Reader.t
     and type oc = Writer.t

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t
end

module Bitfinex = struct
  include Cohttp_async_io

  let base_uri = "https://api.bitfinex.com/v1/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  (* GET / unauthentified *)

  let get endpoint params yojson_to_a =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body >>= fun s ->
    Yojson.Safe.from_string s |>
    yojson_to_a |>
    function | `Ok r -> return r
             | `Error reason -> failwith reason
end

module BTCE = struct
  include Cohttp_async_io

  let version = "3"
  let base_uri = "https://btc-e.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  let get endpoint _ yojson_to_a =
    let uri = mk_uri endpoint in
    Client.get uri >>= fun (resp, body) ->
    Body.to_string body >>= fun s ->
    Yojson.Safe.from_string s |> function
    | `Assoc [(_, ret)] ->
      yojson_to_a ret |> (function | `Ok r -> return r
                                   | `Error reason -> failwith reason)
    | _ -> failwith s
end

module Bittrex = struct
  include Cohttp_async_io

  let version = "v1.1"
  let base_uri = "https://bittrex.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  (* GET / unauthentified *)

  type 'a error_monad = {
    success: bool;
    message: string;
    result: 'a option;
  } [@@deriving show,yojson]

  let get endpoint params yojson_to_a =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body >>= fun s ->
    Yojson.Safe.from_string s |>
    error_monad_of_yojson yojson_to_a |>
    function | `Ok { success = true; result = Some r } -> return r
             | `Ok { success = false; message } -> failwith message
             | `Error reason -> failwith reason
             | _ -> failwith "internal error"
end

module Cryptsy = struct
  include Cohttp_async_io

  let version = "v2"
  let base_uri = "https://api.cryptsy.com/api/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  (* GET / unauthentified *)

  type 'a error_monad = {
    success: bool;
    error: string [@default ""];
    data: 'a option;
  } [@@deriving show,yojson]

  let get endpoint params yojson_to_a =
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body >>= fun s ->
    Yojson.Safe.from_string s |>
    error_monad_of_yojson yojson_to_a |>
    function | `Ok { success = true; data = Some r } -> return r
             | `Ok { success = false; error } -> failwith error
             | `Error reason -> failwith reason
             | _ -> failwith "internal error"
end

module Poloniex = struct
  include Cohttp_async_io

  let base_uri = "https://poloniex.com/public"

  let get endpoint params yojson_to_a =
    Client.get Uri.(with_query' (Uri.of_string base_uri) params) >>= fun (resp, body) ->
    Body.to_string body >>= fun s ->
    Yojson.Safe.from_string s |> function
    | `Assoc l ->
      (try let t = List.assoc endpoint l in
         (* Format.printf "%s@." (Yojson.Safe.to_string t); *)
         yojson_to_a t |> (function | `Ok r -> return r
                                    | `Error reason -> failwith reason)
       with Not_found -> failwith "Unknown currency")
    | _ -> failwith s
end

module Kraken = struct
  include Cohttp_async_io

  let version = "0"
  let base_uri = "https://api.kraken.com/" ^ version ^ "/"
  let mk_uri section = Uri.of_string @@ base_uri ^ section

  type 'a error_monad = {
    error: string list;
    result: 'a option [@default None];
  } [@@deriving yojson]

  let get endpoint params yojson_to_a =
    let lift_f = function
      | `Assoc [_, t] -> yojson_to_a t
      | _ -> invalid_arg "lift_f"
    in
    let uri = mk_uri endpoint in
    Client.get Uri.(with_query' uri params) >>= fun (resp, body) ->
    Body.to_string body >>= fun s ->
    Yojson.Safe.from_string s |>
    error_monad_of_yojson lift_f |> function
    | `Ok { error = []; result = Some r } -> return r
    | `Ok { error; result = None } -> failwith (String.concat " " error)
    | `Error reason -> failwith reason
    | _ -> failwith "internal error"
end

(* POST / authentified *)

(* let post c endpoint params type_of_string = *)
(*   let uri = mk_uri endpoint in *)
(*   let nonce, sign = Credentials.Signature.make c in *)
(*   let params = *)
(*     (["key", [c.Credentials.key]; *)
(*       "signature", [sign]; *)
(*       "nonce", [nonce]] *)
(*      @ List.map (fun (k, v) -> k, [v]) params) *)
(*   in *)
(*   Client.post_form ~params uri >>= fun (resp, body) -> *)
(*   Body.to_string body >>= fun s -> *)
(*   try *)
(*     type_of_string s |> function | `Ok r -> return r *)
(*                                  | `Error reason -> failwith reason *)
(*   with exn -> raise exn *)

