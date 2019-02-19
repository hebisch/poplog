/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/src/readitem.p
 > Purpose:
 > Author:          John Gibson, Mar  8 1988 (see revisions)
 > Documentation:   REF *PROGLIST
 */

;;; ---------------- READING ITEMS FROM PROGLIST --------------------------

#_INCLUDE 'declare.ph'

global constant
        procedure (isincharitem, charin)
    ;

global vars
        pop_#_include_stack
    ;

section Sys$-Prglst;

constant
        procedure Include_pop
    ;

vars
        #_if_stack
    ;

endsection;


;;; ------------------------------------------------------------------------

section $-Sys$-Prglst => proglist, poplastitem, readitem, nextreaditem;

vars
    proglist        = [],
    poplastitem     = false,
    _next_code      = _0,
    ;


    ;;; Called when -proglist- is null
define Unstack();
    if testdef #_if_stack and weakref #_if_stack /== [] then
        mishap(0, 'UNTERMINATED #_IF')
    elseif testdef pop_#_include_stack then
        weakref[pop_#_include_stack] Include_pop()
    else
        false
    endif
enddefine;

    ;;; Reset proglist when null and reading from the terminal
define Reset();
    lvars p;
    if testdef isincharitem
    and (weakref isincharitem(readitem) ->> p)
    and cont(fast_frozval(1, p)) == weakref charin then
        true -> fast_front(proglist);   ;;; revive it
        _0
    else
        _:M_ID_TERMIN
    endif -> _next_code
enddefine;

define readitem();
    lvars pl = proglist;
    ;;; The next rather strange test is because calling -null- on -proglist-
    ;;; can actually change it (due to recursive calls of itemread,
    ;;; readitem etc inside xved inside the dynamic list procedure)
    if null(pl) or (proglist /== pl and null(proglist)) then
        if Unstack() then chain(readitem) endif;
        Reset();
        termin
    else
        fast_destpair(proglist) -> proglist;
        _0 -> _next_code
    endif ->> poplastitem
enddefine;

define nextreaditem();
    lvars pl = proglist;
    if null(pl) or (proglist /== pl and null(proglist)) then
        if Unstack() then chain(nextreaditem) endif;
        _:M_ID_TERMIN -> _next_code;
        termin
    else
        fast_front(proglist);
        _0 -> _next_code
    endif
enddefine;

endsection;     /* $-Sys$-Prglst */

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  4 1991
        Fixed horrible problem with null(proglist) -- see comment in code.
--- John Gibson, Aug 23 1991
        Made readitem & nextreaditem set _next_code to _:M_ID_TERMIN when
        proglist is null
 */
