(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/ExtendedVector.sig
 * Purpose:         Extended Vector signature
 * Author:          Robert John Duncan, Nov 19 1990
 * Documentation:   HELP * EXTENDED_VECTOR
 * Related Files:   C.all/pml/lib/ExtendedVector.ml
 *)


signature ExtendedVector = sig

(* Include everything from the basic Vector module *)

include Vector

(* Then the extras *)

val from_list   : 'a list -> 'a vector
val to_list     : 'a vector -> 'a list

val map         : ('a -> 'b) -> 'a vector -> 'b vector
val app         : ('a -> unit) -> 'a vector -> unit
val iterate     : ('a * int -> 'b) -> 'a vector -> 'b vector

end;    (* signature ExtendedVector *)
