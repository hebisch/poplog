(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Bit.sig
 * Purpose:         Bitwise operations on integers (signature)
 * Author:          Robert John Duncan, Jul 12 1990
 * Documentation:   HELP * BIT
 * Related Files:   C.all/pml/lib/Bit.ml
 *)


signature Bit = sig

    exception BitNumber

    val ||          : int * int -> int
    val &&          : int * int -> int
    val &~          : int * int -> int
    val &?          : int * int -> bool
    val ~~          : int -> int

    val set         : int -> int -> int
    val mask        : int -> int -> int
    val clear       : int -> int -> int
    val test        : int -> int -> bool
    val complement  : int -> int

    val <<          : int * int -> int
    val >>          : int * int -> int

    val shiftl      : int -> int -> int
    val shiftr      : int -> int -> int

    val setbit      : int -> int -> int
    val clearbit    : int -> int -> int
    val testbit     : int -> int -> bool

    val size        : int -> int
    val bitcount    : int -> int

end;
