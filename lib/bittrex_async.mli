open Async.Std

module Bittrex : sig
  include Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Reader.t
     and type oc = Writer.t

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t
end

module Cryptsy : sig
  include Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Reader.t
     and type oc = Writer.t

  val get : string -> (string * string) list ->
    (Yojson.Safe.json -> [`Error of string | `Ok of 'a ]) -> 'a t
end

(* val post : Bitstamp.Credentials.t -> string -> (string * string) list -> *)
(*   (string -> [< `Error of string | `Ok of 'a ]) -> 'a t *)
