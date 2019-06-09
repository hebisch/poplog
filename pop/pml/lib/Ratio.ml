(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Ratio.ml
 * Purpose:         Rational numbers
 * Author:          Robert John Duncan, Jul 13 1990 (see revisions)
 * Documentation:   HELP * RATIO
 * Related Files:   C.all/pml/lib/Ratio.sig
 *)


external structure Ratio : Ratio = struct

ml_eqtype ratio;

ml_exception Ratio;

/* Constants */

ml_val zero : ratio = 0;
ml_val one  : ratio = 1;

/* Construction and destruction */

ml_val ratio : int * int -> ratio =
procedure(n, d) with_props ratio;
    lconstant Ratio = ml_valof("Ratio");
    lvars n, d;
    if d == 0 then
        ml_raise(Ratio);
    else
        n / d;
    endif;
endprocedure;

ml_val unratio : ratio -> int * int =
procedure(r) with_props unratio;
    lvars r;
    ml_constuple(destratio(r), 2);
endprocedure;

ml_val numerator   : ratio -> int;
ml_val denominator : ratio -> int;

/* Conversion to other number types */

ml_val floor  : ratio -> int = ml_valof('StdValues.floor');
ml_val round  : ratio -> int;
ml_val intof  : ratio -> int;
ml_val fracof : ratio -> ratio;
ml_val real   : ratio -> real = ml_valof('StdValues.real');

/* Arithmetic */

ml_val ~ : ratio -> ratio = negate;

ml_val + : ratio * ratio -> ratio;
ml_val - : ratio * ratio -> ratio;
ml_val * : ratio * ratio -> ratio;
ml_val / : ratio * ratio -> ratio;

ml_val abs  : ratio -> ratio;
ml_val sign : ratio -> ratio;

/* Comparisons */

ml_val <  : ratio * ratio -> bool;
ml_val <= : ratio * ratio -> bool;
ml_val >  : ratio * ratio -> bool;
ml_val >= : ratio * ratio -> bool;

ml_val max : ratio -> ratio -> ratio;
ml_val min : ratio -> ratio -> ratio;

end;    (* structure Ratio *)

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 10 1990
        Added -fracof- and fixed the result type of -sign-.
 *)
