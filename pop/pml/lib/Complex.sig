(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Complex.sig
 * Purpose:         Complex numbers (signature)
 * Author:          Robert John Duncan, Aug 10 1990
 * Documentation:   HELP * COMPLEX
 * Related Files:   C.all/pml/lib/Complex.ml
 *)


signature Complex = sig

    (* The Complex Type *)

    eqtype complex

    (* Construction and Decomposition *)

    val complex     : real * real -> complex
    val uncomplex   : complex -> real * real

    val imagpart    : complex -> real
    val realpart    : complex -> real

    (* Useful Constants *)

    val zero        : complex
    val one         : complex
    val i           : complex
    val pi          : complex

    (* Basic Arithmetic *)

    val ~           : complex -> complex
    val +           : complex * complex -> complex
    val -           : complex * complex -> complex
    val *           : complex * complex -> complex
    val /           : complex * complex -> complex

    val abs         : complex -> complex
    val sign        : complex -> complex

    val conjugate   : complex -> complex

    (* Basic Functions *)

    exception Ln
    exception Log10
    exception Power

    val sqrt        : complex -> complex
    val exp         : complex -> complex
    val ln          : complex -> complex
    val log10       : complex -> complex
    val power       : complex -> complex -> complex

    val **          : complex * complex -> complex

    (* Trigonometric Functions *)

    val phase       : complex -> complex

    val sin         : complex -> complex
    val cos         : complex -> complex
    val tan         : complex -> complex

    val arcsin      : complex -> complex
    val arccos      : complex -> complex
    val arctan      : complex -> complex

    val arcsinh     : complex -> complex
    val arccosh     : complex -> complex
    val arctanh     : complex -> complex

    val sinh        : complex -> complex
    val cosh        : complex -> complex
    val tanh        : complex -> complex

end;    (* signature Complex *)
