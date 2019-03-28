(** Ofetch HTTP client library *)

(** Transport-independent HTTP client library. *)

module type Peer_S =
sig

  type t
  (** A channel state representing the equivalent of a file descriptor. *)

  type init_data
  (** Data for initializing a connection, like IP address or
      X509 authentication strategies. *)

  val init : protocol:string -> init_data -> (t, string) result
  (** [init ~protocol init_data] is a new channel or an error message.
      The channel is initialized with [init_data]
      (which could hold an IP address or similar).
      The [protocol] must currently be ["http"].
  *)

  val select : t list -> t list -> t list -> t list * t list * t list
  (** [select read write except] is the [t] elements from
      [read], [write], [except] that are ready for handling
      of read, write, and exceptional handling (respectively).

      You can proceed to pass the [read] elements to {!recv_peer}
       and [write] to {!write_peer}.

      Note that the [t] states may have changed, so you {b MUST} use the
       returned [t]s instead of the originals; example:
       [match select [a;b;c] [] []
       | [new_a] when Peer_S.equal a new_a -> (*replace [a] with [new_a]*)
       ]

      {b NB: [except] is currently ignored.}
  *)

  val recv_peer :
    t -> buf:bytes -> pos:int -> len:int -> (int * t, int) result
  (** [recv_peer state ~buf ~pos ~len] is up to [len] bytes from [state]
      blitted into [buf] at [pos].
      [len] represents the maximum size the caller is willing to read,
      and [pos + len] must be less than the size of [buf].
      The result is the amount of bytes read, and the new [state] when the
      operation was succesful. An [Error bytes_read] signals that
      [bytes_read] bytes were blitted into [buf], but that the channel is
      destroyed and no further reads are possible.
  *)

  val write_peer : t -> buf:string -> pos:int -> len:int -> int * t
  (** [write_peer state ~buf ~pos ~len] is up to [len] bytes from [buf] starting
      at offset [pos] sent to the underlying channel.
      The result is the number of bytes actually written and a new state.
      TODO should be a result type to allow error handling on broken writes.
  *)

  val equal : t -> t -> bool
  (** [equal t1 t2] is true if [t1] shares its underlying channel with [t2].
      The internal state is ignored in this comparison so that you may
      identify which returned [t]s correspond to the original [t]s.
  *)
end

val ofetch_global_debugging : bool ref
val debug : ?fd:out_channel ->
  ((('a, out_channel, unit) format -> 'a) -> unit) -> unit
type response_transfer_method =
    Content_length of int
  | Chunked of int
  | Version_1_0 of int
type download_state =
    Reading_headers
  | Reading_body of response_transfer_method
  | Done
module Results : sig
  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
end
val res_assert : string -> bool -> (unit, string) result
val substr_equal : ?off:int -> bytes -> ?len:int -> string -> bool
val index_substr : ?off:int -> bytes -> ?len:int -> string -> int option
module ChunkedLength :
  sig
    type t
    val create : unit -> t
    val add : t -> bytes -> int -> int -> (t, string) result
    val to_int : t -> (int, string) result
    val contains : t -> string -> bool
  end
type 'io data_cb_return =
    ('io request * ('io request -> 'io request) option, string) result
and 'io data_cb = 'io request -> 'io data_cb_return
and 'io request = {
  io : 'io;
  buf : Bytes.t;
  headers : Bytes.t;
  header_off : int ref;
  chunked_buf : ChunkedLength.t;
  saved_bytes : int ref;
  state : download_state;
  read_cb : 'io data_cb option;
  write_cb : 'io data_cb option;
}
val fetch_request :
  write_peer:(string -> int -> int -> int) ->
  path:string -> hostname:string -> 'handle request -> 'handle data_cb_return
type content_range = {
  first : int64;
  last : int64;
  complete : int64 option;
}
type content_range_response =
    Content_range of content_range
  | Unsatisfiable_range of int64
val pp_content_range : Format.formatter -> content_range_response -> unit
val parse_content_range : string -> (content_range_response, string) result
val parse_headers : string -> (response_transfer_method, string) result
val fetch_download :
  write_local:(Bytes.t -> int -> int -> int) ->
  recv_peer:(Bytes.t -> int -> int -> (int, int) result) ->
  'handle request -> 'handle data_cb_return
val new_request :
  connection_handle:'a ->
  buflen:int ->
  write_local:(Bytes.t -> int -> int -> int) ->
  recv_peer:(Bytes.t -> int -> int -> (int, int) result) ->
  write_peer:(string -> int -> int -> int) ->
  path:string -> hostname:string -> 'a request
val fetch_select :
  equal:('a -> 'a -> bool) ->
  requests:'a request list ->
  select:('a list -> 'a list -> 'a list -> 'a list * 'a list * 'a list) ->
  (unit, string) result
val urlparse : string -> (string * string * int * string, string) result
