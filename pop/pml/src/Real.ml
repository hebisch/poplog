(* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 * File:            C.all/pml/src/Real.ml
 * Purpose:         PML: Functions on real numbers
 * Author:          Robert Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * REAL
 *)


signature Real = sig

    (* Basic Functions *)

    exception Log10
    exception Power

    val round           : real -> int
    val intof           : real -> int
    val fracof          : real -> real
    val sign            : real -> real
    val max             : real -> real -> real
    val min             : real -> real -> real
    val log10           : real -> real
    val power           : real -> real -> real

    val **              : real * real -> real

    (* Trigonometric Functions *)

    exception ArcSin
    exception ArcCos
    exception ArcCosh
    exception ArcTanh

    val tan             : real -> real
    val arcsin          : real -> real
    val arccos          : real -> real
    val sinh            : real -> real
    val cosh            : real -> real
    val tanh            : real -> real
    val arcsinh         : real -> real
    val arccosh         : real -> real
    val arctanh         : real -> real

    val arctan2         : real -> real -> real

    (* Real Constants *)

    val pi              : real
    val most_positive   : real
    val least_positive  : real
    val most_negative   : real
    val least_negative  : real
    val add_epsilon     : real
    val sub_epsilon     : real

end;

infix 8 **;

pop11

section $-ml;

;;; Define floating-point parameters
#_IF not(DEF pop_most_positive_ddecimal)
section;
loadlib('float_parameters.p');
endsection;
#_ENDIF

;;; Check a result to be an ML real number (= pop ddecimal)
;;; Raise the given exception if not

define lconstant CheckReal(r, exn) -> r;
    lvars r, exn;
    unless isddecimal(r) then
        raise(exn);
    endunless;
enddefine;

ml_structure Real : Real = struct

ml_exception Power;
ml_exception Log10;

/* Basic Functions */

ml_val round : real -> int;

ml_val intof : real -> int;

ml_val fracof : real -> real;

ml_val sign : real -> real;

ml_val max : real -> real -> real;

ml_val min : real -> real -> real;

ml_val log10 : real -> real =
procedure(n) with_props log10;
    lconstant Log10 = exception("Log10");
    lvars n;
    if n > 0.0d0 then log10(n) else raise(Log10) endif;
endprocedure;

ml_val power : real -> real -> real =
procedure with_nargs 2 with_props power;
    lconstant Power = ml_valof("Power"), Overflow = ml_valof("Overflow");
    define dlocal pop_exception_handler(n, msg, idstring, severity);
        returnunless(severity == `E` or severity == `R`)(false);
        erasenum(n);
        raise(
            if isendstring(':arith-fltovf', idstring) then
                Overflow
            else
                Power
            endif);
    enddefine;
    CheckReal(**, Power);
endprocedure;

ml_val ** : real * real -> real = ml_valof("power");

/* Trigonometric Functions */

ml_exception ArcSin;
ml_exception ArcCos;
ml_exception ArcCosh;
ml_exception ArcTanh;

ml_val tan : real -> real;

ml_val arcsin : real -> real =
procedure with_nargs 1 with_props arcsin;
    lconstant ArcSin = exception("ArcSin");
    CheckReal(arcsin(), ArcSin);
endprocedure;

ml_val arccos : real -> real =
procedure with_nargs 1 with_props arccos;
    lconstant ArcCos = exception("ArcCos");
    CheckReal(arccos(), ArcCos);
endprocedure;

ml_val arctan2 : real -> real -> real;

ml_val sinh : real -> real;
ml_val cosh : real -> real;
ml_val tanh : real -> real;

ml_val arcsinh : real -> real;

ml_val arccosh : real -> real =
procedure with_nargs 1 with_props arccosh;
    lconstant ArcCosh = exception("ArcCosh");
    CheckReal(arccosh(), ArcCosh);
endprocedure;

ml_val arctanh : real -> real =
procedure with_nargs 1 with_props arctan;
    lconstant ArcTanh = exception("ArcTanh");
    CheckReal(arctanh(), ArcTanh);
endprocedure;

/* Real Constants */

ml_val most_positive  : real = pop_most_positive_ddecimal;
ml_val least_positive : real = pop_least_positive_ddecimal;
ml_val most_negative  : real = pop_most_negative_ddecimal;
ml_val least_negative : real = pop_least_negative_ddecimal;

ml_val add_epsilon : real = pop_plus_epsilon_ddecimal;
ml_val sub_epsilon : real = pop_minus_epsilon_ddecimal;

ml_val pi : real = pi;

ml_endstructure;

endsection; /* $-ml */

ml


(* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Aug  9 1990
        Added complete range of trig functions and infix power operator.
        Revised for new maths exceptions.
--- Rob Duncan, Sep  4 1989
        New file created from real part of old "Numbers.ml" with some
        renaming and addition of the -power- function.
 *)
