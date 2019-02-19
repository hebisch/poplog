/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/sig_queue.p
 > Purpose:         routines for accessing the queue of pending signals
 > Author:          Roger Evans, Apr 18 1988 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   signals.p sigtab.p signals.ph sig_*.p
 */

#_INCLUDE 'declare.ph';
#_INCLUDE 'signals.ph';

;;; --------------------------------------------------------------------

section $-Sys => pop_ast_queue;

    ;;; access to signal queue

    ;;; Two complications:
    ;;;     1) Sys_ast_queue points to a dummy pair on the front of
    ;;;        the queue, so we don't want to return that
    ;;;     2) sys_raise_ast assumes it can return pairs in the queue
    ;;;        to the free list (to avoid creating garbage in normal use)
    ;;;        so these routines return/update COPIES of the list to
    ;;;        ensure no user pointers to the component pairs exist

define active pop_ast_queue;
    copylist(fast_back(Sys_ast_queue));
enddefine;

define updaterof active pop_ast_queue(q);
    lvars p, qend, qcopy, q;
    if q == [] then
        ;;; clears all queued ASTs/signals
        chain(true, Clear_ast_queue)
    endif;

    ;;; not clearing - check and copy pairs and remember the last
    ;;; pair created
    q -> p;
    conspair(false,[]) ->> qcopy -> qend;  ;;; dummy pair at head
    while ispair(p) do
        _CHECKINTERRUPT;
        conspair(fast_front(p),[]) ->> fast_back(qend) -> qend;
        fast_back(p) -> p;
    endwhile;
    unless p == [] then
        mishap(q,1,'(NON-DYNAMIC) LIST NEEDED FOR pop_ast_queue');
    endunless;
    fast_back(qcopy) -> q;  ;;; strip off dummy pair

    sys_grbg_list(fast_back(Sys_ast_queue));  ;;; throw away old list
    q -> fast_back(Sys_ast_queue);
    qend -> Sys_ast_queue_end;
    ;;; clear flag to check queue for suspended asts/signals
    false -> _ignore_ast_queue;
enddefine;


endsection; ;;; $-Sys



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 22 1994
        sys*_signal_queue -> pop_ast_queue
--- John Gibson, May 16 1990
        Made updater of -pop_ast_queue- call -Clear_ast_queue- when
        assigning [].
--- John Gibson, Aug 24 1989
        Removed #_IF for S*IGNALS
--- Roger Evans, Apr 18 1988 wrapped in #_IF DEF SIGNALS
 */
