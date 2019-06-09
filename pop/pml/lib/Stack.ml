(* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 * File:           C.all/pml/lib/Stack.ml
 * Purpose:        ML - Structure Stack: a trivial implementation of stacks.
 * Author:         Robert Duncan & Simon Nichols, Sept. 1987 (see revisions)
 *)

structure Stack : Stack = struct

datatype 'a stack =
    NIL
|   PUSH of 'a * 'a stack

exception Pop
exception Top

val nilstack = NIL

val push = PUSH

fun pop (PUSH s) = s
|   pop _ = raise Pop

fun top (PUSH(x, _)) = x
|   top _ = raise Top

fun isempty NIL = true
|   isempty _ = false

end;    (* structure Stack *)

(* --- Revision History ---------------------------------------------------
--- Rob Duncan, Apr 20 1990
        Upper-case constructors
--- Rob Duncan, Nov  8 1989
        Modified for PML Version 2
 *)
