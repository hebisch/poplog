(* --- Copyright University of Sussex 1994.  All rights reserved. ---------
 * File:            C.all/pml/src/Int.ml
 * Purpose:         PML: Functions on integers
 * Author:          Robert Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * INT
 *)


signature Int = sig

    exception Power

    val quot    : int * int -> int
    val rem     : int * int -> int

    val sign    : int -> int
    val max     : int -> int -> int
    val min     : int -> int -> int
    val power   : int -> int -> int
    val gcd     : int -> int -> int
    val lcm     : int -> int -> int

    val **      : int * int -> int

end;

infix 8 **;
infix 7 quot rem;


pop11


section $-ml;

ml_structure Int : Int = struct

ml_exception Power;

ml_val quot : int * int -> int = nonop div;

ml_val rem : int * int -> int;

ml_val sign : int -> int;

ml_val max : int -> int -> int;

ml_val min : int -> int -> int;

ml_val power : int -> int -> int =
procedure(n, m) with_props power;
    lconstant PowerExn = exception("Power");
    lvars n, m;
    if m < 0 then
        raise(PowerExn);
    else
        n ** m;
    endif;
endprocedure;

ml_val ** : int * int -> int = ml_valof("power");

ml_val gcd : int -> int -> int =
procedure with_props gcd with_nargs 2;
    gcd_n(2);
endprocedure;

ml_val lcm : int -> int -> int =
procedure with_props lcm with_nargs 2;
    lcm_n(2);
endprocedure;

ml_endstructure;

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 30 1990
        Added -quot- and -rem-
--- Robert John Duncan, Aug  9 1990
        Added infix power operator "**".
--- Rob Duncan, Sep  4 1989
        New file created from integer part of old "Numbers.ml" with the
        addition of the -power- function.
 *)
