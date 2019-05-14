/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/DEFV.p
 > Purpose:         DEFV: macro for testing version numbers (for #_IF etc)
 > Author:          Rob Duncan, Apr  4 1989 (see revisions)
 > Documentation:   REF * PROGLIST
 > Related Files:   LIB * DEF
 */


section;

;;; DEFV:
;;;     like DEF, but additionally tests the value of the identifier
;;;     against a given number, using one of the operators: = /= < <= > >=

define global macro DEFV;
    lconstant ops = [= /= < <= > >=];
    lvars id, op, num, val;
    sys_read_path(readitem(), false, false) -> id;
    unless lmember(readitem() ->> op, ops) then
        mishap(op, 1, 'ILLEGAL OPERATOR NAME');
    elseunless isreal(readitem() ->> num) then
        mishap(num, 1, 'REAL NUMBER NEEDED');
    endunless;
    if (sys_current_ident(id) ->> id) and (fast_idval(id) ->> val) then
        valof(op)(round(val*1000), round(num*1000));
    else
        false;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 13 1992
        Made it work with section pathnames (like DEF)
 */
