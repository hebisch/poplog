/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/pdr_util.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
 */

;;; ----------------- ACCESSING PROCEDURE FIELDS ------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Check_closure, Sys$-Check_user_procedure)
    ;

;;; --------------------------------------------------------------------

section $-Sys => updater pdprops pdpart pdnargs;

define updater(pdr);
    lvars pdr;
    Check_procedure(pdr);
    pdr!PD_UPDATER
enddefine;
;;;
define updaterof updater(newupd, pdr);
    lvars pdr, newupd;
    Check_user_procedure(pdr);
    if newupd then Check_procedure(newupd) endif;
    if pdr!PD_FLAGS _bitst _:M_PD_CLOSURE then
        if pdr!PD_CLOS_PDPART == weakref systrace then
            fast_frozval(1, pdr) -> pdr
        endif
    elseif pdr!PD_FLAGS _bitst _:M_PD_ARRAY then
        mishap(pdr, 1, 'CAN\'T CHANGE UPDATER OF ARRAY PROCEDURE')
    endif;
    newupd -> pdr!PD_UPDATER
enddefine;

define pdprops(pdr);
    lvars pdr;
    Check_procedure(pdr);
    pdr!PD_PROPS
enddefine;
;;;
define updaterof pdprops(newprops, pdr);
    lvars pdr, newprops;
    Check_user_procedure(pdr);
    newprops -> pdr!PD_PROPS
enddefine;

define pdpart(pdr);
    lvars pdr;
    Check_procedure(pdr);
    if isclosure(pdr) then
        pdr!PD_CLOS_PDPART
    else
        false
    endif
enddefine;
;;;
define updaterof pdpart(newpdpart, pdr);
    lvars pdr, newpdpart;
    Check_closure(pdr);
    if pdr!PD_FLAGS _bitst _:M_PD_CLOS_PROTECT then
        mishap(pdr, 1, 'CAN\'T ALTER PDPART')
    endif;
    Check_procedure(newpdpart);
    newpdpart -> pdr!PD_CLOS_PDPART
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1988
        Moved out of procedure.p
 */
