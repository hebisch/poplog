(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/FastInt.ml
 * Purpose:         Fast integer operations in ML
 * Author:          Robert John Duncan, Jul  9 1990 (see revisions)
 * Documentation:   HELP * FASTINT
 * Related Files:   C.all/pml/lib/FastInt.sig
 *)


external structure FastInt : FastInt = struct

#_IF not(DEF pop_max_int)
section;
loadlib('int_parameters.p');
endsection;
#_ENDIF

ml_eqtype fastint;

ml_val maxint : fastint = pop_max_int;
ml_val minint : fastint = pop_min_int;
ml_val zero   : fastint = 0;
ml_val one    : fastint = 1;

ml_val fastint : int -> fastint =
procedure(n) with_props fastint;
    lvars n;
    if isinteger(n) then
        n;
    elseif n > 0 then
        pop_max_int;
    else
        pop_min_int;
    endif;
endprocedure;

ml_val int  : fastint -> int = identfn;

ml_val sign : fastint -> fastint;

ml_val +    : fastint * fastint -> fastint = nonop fi_+;
ml_val -    : fastint * fastint -> fastint = nonop fi_-;
ml_val *    : fastint * fastint -> fastint = nonop fi_*;

define lconstant divmod(i, d) -> q -> r;
    lvars i, d, q, r;
    i fi_// d -> q -> r;
    if d fi_> 0 then
        unless r fi_>= 0 then
            q fi_- 1 -> q;
            r fi_+ d -> r;
        endunless;
    else
        unless r fi_<= 0 then
            q fi_- 1 -> q;
            r fi_+ d -> r;
        endunless;
    endif;
enddefine;

ml_val div : fastint * fastint -> fastint =
procedure() -> q with_props div with_nargs 2;
    lvars q;
    divmod() -> q -> ;
endprocedure;

ml_val mod : fastint * fastint -> fastint =
procedure() -> r with_props mod with_nargs 2;
    lvars r;
    divmod() -> -> r;
endprocedure;

ml_val quot : fastint * fastint -> fastint = nonop fi_div;
ml_val rem  : fastint * fastint -> fastint = nonop fi_rem;

ml_val ==   : fastint * fastint -> bool = nonop ==;

ml_val <    : fastint * fastint -> bool = nonop fi_<;
ml_val <=   : fastint * fastint -> bool = nonop fi_<=;
ml_val >    : fastint * fastint -> bool = nonop fi_>;
ml_val >=   : fastint * fastint -> bool = nonop fi_>=;

ml_val max  : fastint -> fastint -> fastint = fi_max;
ml_val min  : fastint -> fastint -> fastint = fi_min;

ml_val ~~   : fastint -> fastint = nonop fi_~~;

ml_val ||   : fastint * fastint -> fastint = nonop fi_||;
ml_val &&   : fastint * fastint -> fastint = nonop fi_&&;
ml_val &~   : fastint * fastint -> fastint = nonop fi_&&~~;
ml_val &?   : fastint * fastint -> bool    = nonop &&/=_0;

ml_val >>   : fastint * fastint -> fastint = nonop fi_>>;
ml_val <<   : fastint * fastint -> fastint = nonop fi_<<;

ml_val ~ : fastint -> fastint =
procedure(n) with_props ~;
    lvars n;
    0 fi_- n;
endprocedure;

ml_val abs : fastint -> fastint =
procedure(n) with_props abs;
    lvars n;
    if n fi_>= 0 then
        n;
    else
        0 fi_- n;
    endif;
endprocedure;

end;    (* structure FastInt *)

infix 7 && &~ &?
infix 6 ||
infix 5 << >>
infix 4 ==


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 30 1990
        Added -quot- and -rem-
--- Robert John Duncan, Aug 10 1990
        Fixed -div- & -mod- to work like the standard integer versions.
        Fixed result type of -sign-.
--- Robert John Duncan, Jul 16 1990
        Added -abs-, -sign- and bitwise operators.
 *)
