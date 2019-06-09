(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Scon.sig
 * Purpose:         Union of SML special constant types
 * Author:          Rob Duncan, Jan 15 1990
 * Documentation:   HELP * SCON
 * Related Files:   C.all/pml/lib/Scon.ml
 *)

signature Scon = sig

datatype scon =
    INT of int
|   REAL of real
|   STRING of string

end;
