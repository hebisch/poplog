(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Ratio.sig
 * Purpose:         Rational numbers (signature)
 * Author:          Robert John Duncan, Jul 13 1990 (see revisions)
 * Documentation:   HELP * RATIO
 * Related Files:   C.all/pml/lib/Ratio.ml
 *)


signature Ratio = sig

    eqtype ratio

    exception Ratio

    val ratio       : int * int -> ratio
    val unratio     : ratio -> int * int

    val numerator   : ratio -> int
    val denominator : ratio -> int

    val zero        : ratio
    val one         : ratio

    val floor       : ratio -> int
    val round       : ratio -> int
    val intof       : ratio -> int
    val fracof      : ratio -> ratio
    val real        : ratio -> real

    val ~           : ratio -> ratio
    val +           : ratio * ratio -> ratio
    val -           : ratio * ratio -> ratio
    val *           : ratio * ratio -> ratio
    val /           : ratio * ratio -> ratio

    val abs         : ratio -> ratio
    val sign        : ratio -> ratio

    val <           : ratio * ratio -> bool
    val <=          : ratio * ratio -> bool
    val >           : ratio * ratio -> bool
    val >=          : ratio * ratio -> bool

    val max         : ratio -> ratio -> ratio
    val min         : ratio -> ratio -> ratio

end;    (* signature Ratio *)


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 10 1990
        Added -fracof- and fixed the result type of -sign-.
 *)
