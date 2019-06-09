(* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 * File:           C.all/pml/lib/Queue.ml
 * Purpose:        ML - Structure Queue: an implementation of queues.
 * Author:         Robert Duncan & Simon Nichols, Sept. 1987 (see revisions)
 *)

structure Queue : Queue = struct

(* Use an abstype to prevent equality *)
abstype 'a queue =
    QUEUE of 'a list * 'a list
with

exception Dequeue
exception Front

val nilqueue = QUEUE([], [])

fun enqueue (item, QUEUE(head, tail)) =
        QUEUE(head, item :: tail)

fun dequeue (QUEUE(item :: head, tail)) =
        (item, QUEUE(head, tail))
|   dequeue (QUEUE([], [])) =
        raise Dequeue
|   dequeue (QUEUE([], tail)) =
        dequeue (QUEUE(rev(tail), []))

fun front (QUEUE(item :: head, tail)) =
        item
|   front (QUEUE([], [])) =
        raise Front
|   front (QUEUE([], tail)) =
        front (QUEUE(rev(tail), []))

fun isempty (QUEUE([], [])) = true
|   isempty _ = false

end     (* abstype queue *)
end;    (* structure Queue *)

(* --- Revision History ---------------------------------------------------
--- Rob Duncan, Apr 20 1990
        Upper-case constructors
--- Rob Duncan, Nov  8 1989
        Modified for PML Version 2
 *)
