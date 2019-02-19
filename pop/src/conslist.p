/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/conslist.p
 > Purpose:
 > Author:          John Williams (see revisions)
 > Documentation:   REF *LISTS
 */

;;; --------------------- LISTS AS VECTORS -------------------------------

#_INCLUDE 'declare.ph'

;;; ---------------------------------------------------------------------

section $-Sys => conslist destlist initl;

define conslist(len) -> list;
    lvars len, list = [];
    Check_integer(len, 0);
    until len == 0 do
        conspair(list) -> list;
        len fi_- 1 -> len
    enduntil
enddefine;

define destlist(list) -> _len;
    lvars _len = stacklength(), list;
    until null(list) do
        fast_destpair(list) -> list;
    enduntil;
    stacklength() fi_- _len -> _len;
enddefine;

define initl(_len) -> list;
    lvars _len, list = [];
    Check_integer(_len, 0);
    until _len == 0 do
        conspair([], list) -> list;
        _len fi_- 1 -> _len
    enduntil
enddefine;

endsection;     /* $- Sys */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jul 14 1988
    Made destlist use stacklength instead of counting each time.
--- John Gibson, Apr  7 1988
        Moved out of lists.p
 */
