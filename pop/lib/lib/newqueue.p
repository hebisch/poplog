/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/lib/newqueue.p
 >  Purpose:        Provide a queue mechanism.
 >  Author:         Jonathan Cunningham, March 1984 (see revisions)
 >  Documentation:  HELP *NEWQUEUE
 */
compile_mode :pop11 +strict;

section;

uses newpr;

lconstant qneeded = 'QUEUE NEEDED';

define vars isqueue(queue);
    lvars queue;
    queue.isclosure and pdprops(queue) == "queue"
enddefine;

;;; make a queue into a list - leaves queue unchanged but doesn't copy list
define vars queuelist(queue);
    lvars queue;
    unless queue.isqueue then
        mishap(queue, 1, qneeded)
    endunless;
    back(frozval(1,queue))
enddefine;
;;;
;;; replace the contents of an existing queue
define updaterof queuelist(item, queue);
    lvars item, queue, pair;
    unless queue.isqueue then
        mishap(queue, 1, qneeded)
    endunless;
    frozval(1,queue) -> pair;
    item -> back(pair);
    lastpair(pair) -> front(pair)
enddefine;

;;; length of the queue
define vars queuelength(queue);
    lvars queue;
    length(queuelist(queue))
enddefine;

;;; queue printing procedure
define vars prqueue(queue);
    lvars queue;
    unless queue.isqueue then
        mishap(queue, 1, qneeded)
    endunless;
    pr('<queue: ');
    pr(queuelist(queue));
    pr('>')
enddefine;

;;; make an initial list into a queue procedure
define vars newqueue(initial) -> queue;
    lvars initial, queue, pair;

    define lconstant qp(queue) -> item;
        lvars queue, item;
        if back(queue) == [] then
            termin -> item
        else
            front(back(queue)) -> item;
            back(back(queue)) -> back(queue);
            if back(queue) == [] then queue -> front(queue) endif
        endif
    enddefine;
    ;;;
    define updaterof qp(item,queue);
        lvars queue, item;
        conspair(item,[]) ->> back(front(queue)) -> front(queue)
    enddefine;

    if initial == [] then
        conspair([],[]) -> pair;
        pair -> front(pair)
    else
        conspair(lastpair(initial),initial) -> pair
    endif;

    qp(% pair %) -> queue;
    "queue" -> pdprops(queue);

    ;;; change pr to print queues
    if pr == syspr then newpr(isqueue, prqueue) endif;
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 24 1993
        Made lib newqueue -- old one to lib/obsolete/old_newqueue.p
--- John Gibson, Nov 11 1992
        Cleaned up and made lib new_new*queue so as to rename procedures
        qlist->queuelist and qlength->queuelength
 */
