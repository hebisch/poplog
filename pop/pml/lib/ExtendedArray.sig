(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/ExtendedArray.sig
 * Purpose:         Extended Array signature
 * Author:          Robert John Duncan, Nov 19 1990
 * Documentation:   HELP * EXTENDED_ARRAY
 * Related Files:   C.all/pml/lib/ExtendedArray.ml
 *)


signature ExtendedArray = sig

(* Include everything from the basic Array module *)

include Array

(* Then the extras *)

val from_list   : '_a list -> '_a array
val from_vector : '_a Vector.vector -> '_a array
val to_list     : 'a array -> 'a list
val to_vector   : 'a array -> 'a Vector.vector

val ==          : 'a array * 'a array -> bool

val map         : ('a -> '_b) -> 'a array -> '_b array
val app         : ('a -> unit) -> 'a array -> unit
val iterate     : ('a * int -> '_b) -> 'a array -> '_b array
val copy        : '_a array -> '_a array
val fill        : 'a array -> 'a -> unit

val nc_map      : ('a -> 'a) -> 'a array -> 'a array
val nc_iterate  : ('a * int -> 'a) -> 'a array -> 'a array

end;    (* signature ExtendedArray *)
