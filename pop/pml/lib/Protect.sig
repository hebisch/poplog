(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Protect.sig
 * Purpose:         Adding entry and exit code to functions (signature)
 * Author:          Robert John Duncan, Jul  6 1990
 * Related Files:   C.all/pml/lib/Protect.ml
 *)


signature Protect = sig

val protect : (unit -> 'a) * ('a -> unit) -> ('b -> 'c) -> 'b -> 'c
val guard   : 'a ref -> ('b -> 'c) -> 'b -> 'c

end;    (* signature Protect *)
