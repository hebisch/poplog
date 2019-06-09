(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/FastInt.sig
 * Purpose:         Fast integer operations in ML (signature)
 * Author:          Robert John Duncan, Jul  9 1990 (see revisions)
 * Documentation:   HELP * FASTINT
 * Related Files:   C.all/pml/lib/FastInt.ml
 *)


signature FastInt = sig

    eqtype fastint

    val maxint  : fastint
    val minint  : fastint
    val zero    : fastint
    val one     : fastint

    val fastint : int -> fastint
    val int     : fastint -> int

    val sign    : fastint -> fastint

    val ~       : fastint -> fastint
    val +       : fastint * fastint -> fastint
    val -       : fastint * fastint -> fastint
    val *       : fastint * fastint -> fastint
    val div     : fastint * fastint -> fastint
    val mod     : fastint * fastint -> fastint
    val quot    : fastint * fastint -> fastint
    val rem     : fastint * fastint -> fastint
    val abs     : fastint -> fastint

    val ==      : fastint * fastint -> bool
    val <       : fastint * fastint -> bool
    val <=      : fastint * fastint -> bool
    val >       : fastint * fastint -> bool
    val >=      : fastint * fastint -> bool

    val max     : fastint -> fastint -> fastint
    val min     : fastint -> fastint -> fastint

    val ~~      : fastint -> fastint
    val ||      : fastint * fastint -> fastint
    val &&      : fastint * fastint -> fastint
    val &~      : fastint * fastint -> fastint
    val &?      : fastint * fastint -> bool

    val >>      : fastint * fastint -> fastint
    val <<      : fastint * fastint -> fastint

end;    (* signature FastInt *)


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 30 1990
        Added -quot- and -rem-
--- Robert John Duncan, Aug 10 1990
        Fixed result type of -sign-.
--- Robert John Duncan, Jul 16 1990
        Added -abs-, -sign- and bitwise operators.
 *)
