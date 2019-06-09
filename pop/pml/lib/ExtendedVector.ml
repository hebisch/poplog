(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/lib/ExtendedVector.ml
 * Purpose:         Extra functions on vectors
 * Author:          Robert John Duncan, Nov 20 1990 (see revisions)
 * Documentation:   HELP * EXTENDED_VECTOR
 * Related Files:   C.all/pml/lib/ExtendedVector.sig
 *)

(* Initial definition in Pop-11 *)
pop11
ml_structure ExtendedVector = struct

ml_val to_list : 'a Vector.vector -> 'a list =
    datalist;

ml_val map : ('a -> 'b) -> 'a Vector.vector -> 'b Vector.vector =
procedure(f, v) with_props map;
    lvars f, v;
    ml_consvector(appdata(v, f), datalength(v));
endprocedure;

ml_val app : ('a -> unit) -> 'a Vector.vector -> unit =
procedure(f, v) with_props app;
    lvars f, v;
    erasenum(#| appdata(v, f) |#);
    ml_unit;
endprocedure;

ml_val iterate : ('a * int -> 'b) -> 'a Vector.vector -> 'b Vector.vector =
procedure(f, v) with_props iterate;
    lvars i, procedure f, v, n = datalength(v);
    ml_consvector(
        fast_for i to n do
            f(conspair(fast_subscrv(i, v), i fi_- 1));
        endfor,
        n);
endprocedure;

ml_endstructure; /* ExtendedVector */
ml

(* Redefinition *)
structure ExtendedVector : ExtendedVector = struct

    (* Include all the basic Vector stuff ... *)
    open Vector

    (* ... add some synonyms ... *)
    val from_list = Vector.vector

    (* ... then the extras we've just defined *)
    val to_list = ExtendedVector.to_list
    val map     = ExtendedVector.map
    val app     = ExtendedVector.app
    val iterate = ExtendedVector.iterate

end;


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Vector structure now built-in, and the extra functions defined
        explicitly here
--- Robert John Duncan, Dec  3 1990
        -sub- function no longer infix by default.
 *)
