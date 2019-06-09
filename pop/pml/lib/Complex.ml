(* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.all/pml/lib/Complex.ml
 * Purpose:         Complex numbers
 * Author:          Robert John Duncan, Aug 10 1990 (see revisions)
 * Documentation:   HELP * COMPLEX
 * Related Files:   C.all/pml/lib/Complex.sig
 *)


external structure Complex : Complex = struct

/* Complex Type */

ml_eqtype complex;

/* Construction and Decomposition */

ml_val complex : real * real -> complex = nonop +:;

ml_val uncomplex : complex -> real * real =
procedure(n) with_props uncomplex;
    lvars n;
    ml_constuple(destcomplex(n), 2);
endprocedure;

ml_val realpart : complex -> real;
ml_val imagpart : complex -> real;


/* Useful Constants */

ml_val zero : complex = 0.0d0;
ml_val one  : complex = 1.0d0;
ml_val i    : complex = 0.0d0_+:1.0d0;
ml_val pi   : complex;


/* Basic Arithmetic */

ml_val ~ : complex -> complex = negate;
ml_val + : complex * complex -> complex;
ml_val - : complex * complex -> complex;
ml_val * : complex * complex -> complex;
ml_val / : complex * complex -> complex;

ml_val conjugate : complex -> complex;

ml_val abs : complex -> complex;

ml_val sign : complex -> complex;


/* Complex Functions */

section $-ml;
/* Declare exceptions in section ML to share with others of the same name */
ml_exception Overflow;
ml_exception Ln;
ml_exception Log10;
ml_exception Power;
endsection;

ml_val sqrt : complex -> complex;

ml_val ln : complex -> complex =
procedure() with_props ln with_nargs 1;
    define dlocal pop_exception_handler(n, msg, idstring, severity);
        returnunless(severity == `E` or severity == `R`)(false);
        erasenum(n);
        lconstant Ln = ml_valof("Ln");
        ml_raise(Ln);
    enddefine;
    log();
endprocedure;

ml_val log10 : complex -> complex =
procedure() with_props log10 with_nargs 1;
    define dlocal pop_exception_handler(n, msg, idstring, severity);
        returnunless(severity == `E` or severity == `R`)(false);
        erasenum(n);
        lconstant Log10 = ml_valof("Log10");
        ml_raise(Log10);
    enddefine;
    log10();
endprocedure;

ml_val exp : complex -> complex;

ml_val power : complex -> complex -> complex =
procedure with_props power with_nargs 2;
    define dlocal pop_exception_handler(n, msg, idstring, severity);
        returnunless(severity == `E` or severity == `R`)(false);
        erasenum(n);
        lconstant Power = ml_valof("Power"), Overflow = ml_valof("Overflow");
        ml_raise(
            if isendstring(':arith-fltovf', 1, idstring) then
                Overflow
            else
                Power
            endif);
    enddefine;
    () ** ();
endprocedure;

ml_val ** : complex * complex -> complex = ml_valof("power");


/* Trigonometric Functions */

ml_val phase : complex -> complex;

ml_val sin : complex -> complex;
ml_val cos : complex -> complex;
ml_val tan : complex -> complex;

ml_val arcsin : complex -> complex;
ml_val arccos : complex -> complex;
ml_val arctan : complex -> complex;

ml_val sinh : complex -> complex;
ml_val cosh : complex -> complex;
ml_val tanh : complex -> complex;

ml_val arcsinh : complex -> complex;
ml_val arccosh : complex -> complex;
ml_val arctanh : complex -> complex;


end;    (* structure Complex *)

infix 8 **;


(* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
 *)
