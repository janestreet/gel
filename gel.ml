open! Base

type 'a t = { g : 'a [@global] } [@@unboxed]

(* We want to ensure the derived operations are exactly the same as for the inner type, so
   we define them directly. *)

let[@inline] create g =  { g }
let[@inline] g ({ g } [@local]) = g
let[@inline] map ({ g } [@local]) ~f:(f [@local]) =  { g = f g }
let[@inline] compare compare_g { g = a } { g = b } = compare_g a b
let[@inline] hash_fold_t hash_fold_g hash_state { g } = hash_fold_g hash_state g
let[@inline] sexp_of_t sexp_of_g { g } = sexp_of_g g
let[@inline] t_of_sexp g_of_sexp sexp = { g = g_of_sexp sexp }
let[@inline] globalize _ ({ g } [@local]) = { g }
let[@inline] equal equal_g { g = a } { g = b } = equal_g a b

(* Proof that [extract_opt] can be safely implemented in normal ocaml. But this
   implementation allocates a second option locally. *)
let _extract_opt_proof : ('a t option[@local]) -> ('a option[@local]) =
  fun x ->
  
    (match x with
     | None -> None
     | Some { g } -> Some g)
;;

(* This version of [extract_opt] does not. *)
external drop_some : ('a t option[@local]) -> ('a option[@local]) = "%identity"

(* A similar argument applies to [drop_ok] and [drop_error]. *)
external drop_ok
  :  (('a t, 'b) Result.t[@local])
  -> (('a, 'b) Result.t[@local])
  = "%identity"

external drop_error
  :  (('a, 'b t) Result.t[@local])
  -> (('a, 'b) Result.t[@local])
  = "%identity"

(* We specifically use the "legacy" [Make_binable1_without_uuid] function, because it
   _doesn't_ change the bin_io shape, which is what we want here. *)
include
  Bin_prot.Utils.Make_binable1_without_uuid [@alert "-legacy"] [@inlined hint] (struct
  module Binable = struct
    type 'a t = 'a [@@deriving bin_io]
  end

  type nonrec 'a t = 'a t

  let[@inline] of_binable g = { g }
  let[@inline] to_binable { g } = g
end)
