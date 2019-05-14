/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/last.p
 > Purpose:         Return/update last element of a datastructure
 > Author:          Aaron Sloman, Nov  3 1978 (see revisions)
 > Documentation:   HELP * LAST
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

lconstant procedure
    Last_err = mishap(% 1, 'NON-EMPTY LIST OR VECTOR-TYPE STRUCTURE NEEDED' %);

define global last(x);
    lvars x l key;
    if ispair(x) and not(null(x)) then
        until null(fast_back(x) ->> l) do l -> x enduntil;
        fast_front(x)
    elseif (isvectorclass(x) ->> key) and (datalength(x) ->> l) fi_> 0 then
        class_fast_subscr(key)(l, x)
    elseif isword(x) then
        subscrw(datalength(x), x)
    else
        Last_err(x)
    endif
enddefine;
;;;
define updaterof last(x) with_nargs 2;
    lvars x l key;
    if ispair(x) and not(null(x)) then
        until null(fast_back(x) ->> l) do l -> x enduntil;
        -> fast_front(x)
    elseif (isvectorclass(x) ->> key) and (datalength(x) ->> l) fi_> 0 then
        ;;; fast subscr procedure DOES check new field value
        -> class_fast_subscr(key)(l, x)
    else
        Last_err(x)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992 Cleaned up
*/
