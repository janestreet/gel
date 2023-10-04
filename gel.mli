open! Base

(** A shim to mark non-record fields global. "GEL" stands for "Global Even if inside a
    Local", but is kept short since we'll need this boilerplate a lot.

    For example, if you have a list:

    {[
      type t = string list
    ]}

    and want to make it local, but still keep the strings global, you can write:

    {[
      type t = string Gel.t list
    ]}

    and it will be so, but with some extra boilerplate when using it.

    This is for use with existing types that don't have the desired global_ annotation.
    If you find yourself reaching for this for a new type you are defining, you can avoid
    the boilerplate. For example:

    {[
      type t =
        { global_ foo : string
        ; bar : int
        }

      type t =
        | Foo of global_ string
        | Bar of { global_ foo : string; bar : int }
        | Baz of global_ string * int * global_ string
    ]}
*)
type 'a t = { g : 'a [@global] }
[@@unboxed] [@@deriving bin_io, compare, equal, hash, sexp]

val create : 'a -> ('a t[@local])
val g : ('a t[@local]) -> 'a
val map : ('a t[@local]) -> f:(('a -> 'b)[@local]) -> ('b t[@local])
val globalize : _ -> ('a t[@local]) -> 'a t

(** Removes a [Gel.t] from inside an option type with zero runtime cost. This is useful
    when some other function returns an [X.t Gel.t option], you know [X.t] is
    mode-crossing, and you want to drop the inner [Gel.t] without allocating another local
    option. *)
val drop_some : ('a t option[@local]) -> ('a option[@local])

(** Like [drop_some], but for the [Ok _] branch of a result. *)
val drop_ok : (('a t, 'b) Result.t[@local]) -> (('a, 'b) Result.t[@local])

(** Like [drop_some], but for the [Error _] branch of a result. *)
val drop_error : (('a, 'b t) Result.t[@local]) -> (('a, 'b) Result.t[@local])
