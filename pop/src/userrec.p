/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/userrec.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;------------------- USER RECORD PROCEDURES -----------------------------

#_INCLUDE 'declare.ph'

;;; -----------------------------------------------------------------------

section $-Sys;

define Checkr_record(item, _key) -> item;
    lvars item, _key;
    unless iscompound(item) and item!KEY == _key then
        Record_needed(item, _key)
    endunless
enddefine;

define Eq__Fullrec(item, fullrec) with_props '(Sys$-Eq__Fullrec)';
    lvars item, fullrec, _work, _lim;
    if item == fullrec then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _work) == fullrec!KEY then
        _checkall();            ;;; check for interrupt/recursion overflow
        returnunless(EQ(item!FIRST, fullrec!FIRST)) (false);
        @@SECOND -> _work;
        @@POPBASE{fullrec!KEY!K_RECSIZE_R} -> _lim;
        repeat
            returnif(_work _sgreq _lim) (true);
            returnunless(EQ( item!(w){_work}, fullrec!(w){_work} )) (false);
            @@(w){_work}++ -> _work;
            _CHECKINTERRUPT
        endrepeat
    elseif _work!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(fullrec, item, _work!K_SYS_EQUALS)
    else
        false
    endif
enddefine;      /* Eq__Fullrec */

define Eq__Record(item, rec) with_props '(Sys$-Eq__Record)';
    lvars access_vec, item, procedure accp, rec, _work, _lim;
    if item == rec then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _work) == rec!KEY then
        item!KEY!K_ACCESS_R -> access_vec;
        @@V_WORDS[_0] -> _work;
        @@V_WORDS[access_vec!V_LENGTH] -> _lim;
        repeat
            returnif(_work _greq _lim) (true);
            access_vec!(w){_work} -> accp;
            returnunless(EQ(accp(item), accp(rec))) (false);
            @@(w){_work}++ -> _work;
            _CHECKINTERRUPT
        endrepeat
    elseif _work!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(rec, item, _work!K_SYS_EQUALS)
    else
        false
    endif
enddefine;      /* Eq__Record */


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 17 1996
        Added pdprops strings to Eq__Fullrec and Eq__Record
--- John Gibson, Jan  4 1996
        Changed to use EQ macro
--- John Gibson, Mar 20 1990
        Removed Check_record
--- John Gibson, Dec 20 1989
        Changes for new pop pointers
--- John Gibson, Mar 15 1989
        Created from run-time procedures from conskey.p
 */
