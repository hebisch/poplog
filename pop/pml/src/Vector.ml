(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/Vector.ml
 * Purpose:         PML: the Vector module
 * Author:          Robert John Duncan, Nov 16 1994 (see revisions)
 * Documentation:   HELP * VECTOR
 *)

signature Vector = sig

    eqtype 'a vector

    exception Size
    exception Subscript

    val vector      : 'a list -> 'a vector
    val tabulate    : int * (int -> 'a) -> 'a vector
    val length      : 'a vector -> int
    val sub         : 'a vector * int -> 'a

end (* signature Vector *)

(* Synonym for compatibility with other compilers *)

signature VECTOR = Vector;

pop11

section $-ml;

ml_structure Vector : Vector = struct

ml_eqtype 'a vector;

ml_exception Size;
ml_exception Subscript;

lconstant SizeExn = exception("Size");
lconstant SubscriptExn = exception("Subscript");

ml_val vector : 'a list -> 'a vector =
procedure(list) with_props vector;
    lvars list;
    returnif(list == [])(ml_vector0);
    consvector(#| until list == [] do Destpair(list) -> list enduntil |#);
endprocedure;

ml_val tabulate : int * (int -> 'a) -> 'a vector =
procedure(n, f) with_props tabulate;
    lvars i, n, procedure f;
    if isinteger(n) and n fi_> 0 then
        consvector(For i from 0 to n fi_- 1 do f(i) endfor, n);
    elseif n == 0 then
        ml_vector0;
    else
        raise(SizeExn);
    endif;
endprocedure;

ml_val length : 'a vector -> int =
    datalength;

ml_val sub : 'a vector * int -> 'a =
procedure(v, i) with_props sub;
    lvars v, i;
    if i >= 0 and i < datalength(v) then
        Subv0(i, v);
    else
        raise(SubscriptExn);
    endif;
endprocedure;

ml_endstructure; /* Vector */

endsection; /* $-ml */

ml


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
 *)
