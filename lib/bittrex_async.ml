open Async.Std
open Cohttp_async
open Bittrex

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

