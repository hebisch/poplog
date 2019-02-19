/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/rec_generic.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---------------- GENERIC RECORD PROCEDURES ---------------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Key_hash
    ;

;;; --------------------------------------------------------------------

section $-Sys;

;;; --- HASHING PROCEDURES --------------------------------------------

define Unique_hash() with_nargs 1;
    _pint()
enddefine;

define Fullrec1_hash(item);
    lvars item;
    Key_hash(item!KEY) fi_+ syshash(item!FIRST)
enddefine;

define Record_hash(item) with_props '(Sys$-Record_hash)';
    lvars item, pdrvec, _len;
    item!KEY!K_ACCESS_R -> pdrvec;
    Key_hash(item!KEY);
    unless _zero(pdrvec!V_LENGTH ->> _len) do
            fi_+ syshash(fast_apply(item, pdrvec!V_WORDS[_len _sub _1]))
    endunless
enddefine;


;;; --- GETSIZE PROCEDURES ----------------------------------------------

define Rec1_getsize() with_nargs 1;
    -> ; @@(struct POPREC1)++
enddefine;

define Rec2_getsize() with_nargs 1;
    -> ; @@(struct POPREC2)++
enddefine;

define Record_getsize(rec);         ;;; any record
    lvars rec, _key;
    rec!KEY -> _key;
    if issimple(_key) then _mkcompound(_key) -> _key endif;
    _key!K_RECSIZE_R
enddefine;


;;; --- EQUALS PROCEDURES ----------------------------------------------

define Eq__Fullrec1(item, fullrec1);
    lvars item, fullrec1, _key;
    if item == fullrec1 then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _key) == fullrec1!KEY then
        _CHECKINTERRUPT;
        CHAIN_EQ(item!FIRST, fullrec1!FIRST)
    elseif _key!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(fullrec1, item, _key!K_SYS_EQUALS)
    else
        false
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  4 1996
        Uses CHAIN_EQ macro
--- John Gibson, Apr 25 1995
        Full*rec1/2_getsize -> Rec1/2_getsize
--- John Gibson, Dec 20 1989
        Changes for new pop pointers
--- John Gibson, Apr 14 1988
        From various files
 */
