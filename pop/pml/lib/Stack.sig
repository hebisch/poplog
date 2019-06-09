(* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 * File:           C.all/pml/lib/Stack.sig
 * Purpose:        ML - Signature Stack.
 * Author:         Robert Duncan & Simon Nichols, Sept. 1987 (see revisions)
 *)

signature Stack = sig

    datatype 'a stack =
        NIL
    |   PUSH of 'a * 'a stack

    exception Pop
    exception Top

    val nilstack : 'a stack
    val push : 'a * 'a stack -> 'a stack
    val pop : 'a stack -> 'a * 'a stack
    val top : 'a stack -> 'a
    val isempty : 'a stack -> bool

end;    (* signature Stack *)

(* --- Revision History ---------------------------------------------------
--- Rob Duncan, Apr 20 1990
        Upper-case constructors
--- Rob Duncan, Nov  8 1989
        Modified for PML Version 2
 *)
