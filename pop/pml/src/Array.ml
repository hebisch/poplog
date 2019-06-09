(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/Array.ml
 * Purpose:         PML: the Array module
 * Author:          Robert John Duncan, Nov 16 1994 (see revisions)
 * Documentation:   HELP * ARRAY
 * Related Files:
 *)

signature Array = sig

    eqtype 'a array

    exception Size
    exception Subscript

    val array       : int * '_a -> '_a array
    val arrayoflist : '_a list -> '_a array
    val tabulate    : int * (int -> '_a) -> '_a array
    val length      : 'a array -> int
    val sub         : 'a array * int -> 'a
    val update      : 'a array * int * 'a -> unit

end;    (* signature Array *)

(* Synonym for compatibility with other compilers *)

signature ARRAY = Array;

pop11

section $-ml;

ml_structure Array : Array = struct

ml_eqtype 'a array;

ml_exception Size;
ml_exception Subscript;

lconstant SizeExn = exception("Size");
lconstant SubscriptExn = exception("Subscript");

ml_val array : int * '_a -> '_a array =
procedure(n, init) with_props array;
    lvars n, init;
    if isinteger(n) and n fi_> 0 then
        consmlarray(Repeat n times init endrepeat, n);
    elseif n == 0 then
        ml_array0;
    else
        raise(SizeExn);
    endif;
endprocedure;

ml_val arrayoflist : '_a list -> '_a array =
procedure(list) with_props array;
    lvars list;
    returnif(list == [])(ml_array0);
    consmlarray(#| until list == [] do Destpair(list) -> list enduntil |#);
endprocedure;

ml_val tabulate : int * (int -> '_a) -> '_a array =
procedure(n, f) with_props tabulate;
    lvars i, n, procedure f;
    if isinteger(n) and n fi_> 0 then
        consmlarray(For i from 0 to n fi_- 1 do f(i) endfor, n);
    elseif n == 0 then
        ml_array0;
    else
        raise(SizeExn);
    endif;
endprocedure;

ml_val length : 'a array -> int =
    datalength;

ml_val sub : 'a array * int -> 'a =
procedure(a, i) with_props sub;
    lvars a, i;
    if i >= 0 and i < datalength(a) then
        Subv0(i, a);
    else
        raise(SubscriptExn);
    endif;
endprocedure;

ml_val update : 'a array * int * 'a -> unit =
procedure(a, i, x) with_props update;
    lvars a, i, x;
    if i >= 0 and i < datalength(a) then
        x -> Subv0(i, a);
    else
        raise(SubscriptExn);
    endif;
    ml_unit;
endprocedure;

ml_endstructure; /* Array */

;;; patch the equality attribute of the type constructor to be pointer
;;; equality -- like refs
"ref" -> typename_equality(tycon_function(lookup_tycon(idpath('Array.array'))));

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
 *)
