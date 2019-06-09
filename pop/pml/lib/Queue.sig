(* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 * File:           C.all/pml/lib/Queue.sig
 * Purpose:        ML - Signature Queue.
 * Author:         Robert Duncan & Simon Nichols, Sept. 1987 (see revisions)
 *)

signature Queue = sig

    type 'a queue

    exception Dequeue
    exception Front

    val nilqueue : 'a queue
    val enqueue : 'a * 'a queue -> 'a queue
    val dequeue : 'a queue -> 'a * 'a queue
    val front : 'a queue -> 'a
    val isempty : 'a queue -> bool

end;

(* --- Revision History ---------------------------------------------------
--- Rob Duncan, Nov  8 1989
        Modified for PML Version 2
 *)
