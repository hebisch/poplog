/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/isdlocal.p
 > Purpose:
 > Author:          John Williams (see revisions)
 > Documentation:   REF *IDENT
 */

;;; --------- TEST IF IDENTIFIER IS DYNAMIC LOCAL OF PROCEDURE -------------

#_INCLUDE 'declare.ph'

constant
        procedure (isident, Sys$-Get_closure_procedure)
    ;

;;; ----------------------------------------------------------------------

section $-Sys => isdlocal;

define isdlocal(id, pdr);
    lvars id, pdr, _nonactive = false;
    Check_procedure(pdr);
    if ispair(id) and fast_back(id) == "nonactive" then
        true -> _nonactive;
        fast_front(id) -> id
    endif;
    unless isident(id) then Get_perm_ident(id, false) -> id endunless;
    _nonzero(Dlocal_frame_offset(id, Get_closure_procedure(pdr), _nonactive))
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 27 1995
        Allowed to take 'nonactive' pair (since it now works with active
        vars)
--- John Gibson, Mar 13 1988
        Moved out of control.p
 */
