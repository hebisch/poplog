(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/lib/ExtendedArray.ml
 * Purpose:         More functions on arrays
 * Author:          Robert John Duncan, Nov 19 1990 (see revisions)
 * Documentation:   HELP * EXTENDED_ARRAY
 * Related Files:   C.all/pml/lib/ExtendedArray.sig
 *)

(* Initial definition in Pop-11 *)
pop11
ml_structure ExtendedArray = struct

ml_val to_list : 'a Array.array -> 'a list =
    datalist;

ml_val from_vector : '_a Vector.vector -> '_a Array.array =
procedure(v) with_props from_vector;
    lvars v;
    ml_consarray(ml_destvector(v));
endprocedure;

ml_val to_vector : 'a Array.array -> 'a Vector.vector =
procedure(a) with_props to_vector;
    lvars a;
    ml_consvector(ml_destarray(a));
endprocedure;

ml_val == : 'a Array.array * 'a Array.array -> bool =
    nonop ==;

ml_val map : ('a -> '_b) -> 'a Array.array -> '_b Array.array =
procedure(f, a) with_props map;
    lvars f, a;
    ml_consarray(appdata(a, f), datalength(a));
endprocedure;

ml_val app : ('a -> unit) -> 'a Array.array -> unit =
procedure(f, a) with_props app;
    lvars f, a;
    erasenum(#| appdata(a, f) |#);
    ml_unit;
endprocedure;

ml_val iterate : ('a * int -> '_b) -> 'a Array.array -> '_b Array.array =
procedure(f, a) with_props iterate;
    lvars i, procedure f, a, n = datalength(a);
    ml_consarray(
        fast_for i to n do
            f(conspair(fast_subscrv(i, a), i fi_- 1));
        endfor,
        n);
endprocedure;

ml_val copy : '_a Array.array -> '_a Array.array =
procedure(a) with_props copy;
    lvars a;
    ml_consarray(ml_destarray(a));
endprocedure;

ml_val fill : 'a Array.array -> 'a -> unit =
procedure(a, x) with_props fill;
    lvars a, x;
    fill(fast_repeat datalength(a) times x endrepeat, a) -> ;
    ml_unit;
endprocedure;

ml_val nc_map : ('a -> 'a) -> 'a Array.array -> 'a Array.array =
procedure(f, a) with_props nc_map;
    lvars f, a;
    fill(appdata(a, f), a);
endprocedure;

ml_val nc_iterate : ('a * int -> 'a) -> 'a Array.array -> 'a Array.array =
procedure(f, a) with_props nc_iterate;
    lvars i, procedure f, a, n = datalength(a);
    fill(
        fast_for i to n do
            f(conspair(fast_subscrv(i, a), i fi_- 1));
        endfor,
        a);
endprocedure;

ml_endstructure; /* ExtendedArray */
ml

(* Redefinition *)
structure ExtendedArray : ExtendedArray = struct

    (* Include all the basic Array stuff ... *)
    open Array

    (* ... add some synonyms ... *)
    val from_list       = Array.arrayoflist

    (* ... then the extras we've just defined *)
    val to_list         = ExtendedArray.to_list
    val from_vector     = ExtendedArray.from_vector
    val to_vector       = ExtendedArray.to_vector

    val op ==           = ExtendedArray.==

    val map             = ExtendedArray.map
    val app             = ExtendedArray.app
    val iterate         = ExtendedArray.iterate
    val copy            = ExtendedArray.copy
    val fill            = ExtendedArray.fill

    val nc_map          = ExtendedArray.nc_map
    val nc_iterate      = ExtendedArray.nc_iterate

end; (* structure ExtendedArray *)

infix 4 ==;


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Array structure now built-in, and the extra functions defined
        explicitly here
--- Robert John Duncan, Dec  3 1990
        -sub- function no longer infix by default.
 *)
