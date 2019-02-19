/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/isclosed.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ---------------------- ISCLOSED ----------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

constant
        procedure (isdevice, Sys$-Stringin)
    ;

;;; ------------------------------------------------------------------------

define isclosed(item);
    lvars item, froz, _mshp = true;
    if isboolean(item) then
        item -> _mshp -> item
    endif;
    if isdevice(item) then
        return(item!D_FLAGS _bitst _M_D_CLOSED)
    elseif isclosure(item) and datalength(item) fi_> 0 then
        fast_frozval(1, item) -> froz;
        if isdevice(froz) then
            chain(froz, isclosed)
        elseif testdef Sys$-Stringin
        and pdpart(item) == weakref Sys$-Stringin then
            return(back(froz) > datalength(front(froz)))
        endif
    elseunless _mshp then
        return("undef")
    endif;
    mishap(item, 1, 'BAD ARGUMENT FOR ISCLOSED')
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Mar 14 1988
        Weakref'ed -Stringin- (now Sys$-Stringin)
 */
