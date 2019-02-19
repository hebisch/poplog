/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/numbers.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;; ------------- MISCELLANEOUS NUMBER PROCEDURES ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (Sys$-Ratio_clawback, Sys$-Complex_clawback)
    ;

;;; -------------------------------------------------------------------------

section $-Sys => isreal isnumber;

define isreal(item);
    lvars item;
    issimple(item) or item!KEY!K_NUMBER_TYPE _lt _:NUMTYPE_COMPLEX
enddefine;

define isnumber(item);
    lvars item;
    if issimple(item) or item!KEY!K_NUMBER_TYPE /== _:NUMTYPE_NON_NUMBER then
        true
    else
        false
    endif
enddefine;

define Checkr_num_class(item);
    lvars item, _ntype;
    if issimple(item) then
        if isinteger(item) then
            _:NUMTYPE_RATIO
        else
            _:NUMTYPE_DECIMAL
        endif
    else
        item!KEY!K_NUMBER_TYPE -> _ntype;
        if _ntype == _:NUMTYPE_NON_NUMBER then
            mishap(item, 1, 'NUMBER NEEDED')
        elseif _ntype _lt _:NUMTYPE_RATIO then
            _:NUMTYPE_RATIO
        else
            _ntype
        endif
    endif
enddefine;


;;; --- CLAWBACK PROCEDURES FOR NUMBERS -------------------------------------

lconstant
    clawbacks = {%repeat 7 times Clawback endrepeat%};

RTWEAK Ratio_clawback   -> subscrv(NUMTYPE_RATIO, clawbacks);
CXWEAK Complex_clawback -> subscrv(NUMTYPE_COMPLEX, clawbacks);


define Clawback_num(n);
    lvars n;
    if iscompound(n) then
        fast_apply(n, fast_subscrv(_pint(n!KEY!K_NUMBER_TYPE), clawbacks))
    else
        Clawback(n)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
 */
