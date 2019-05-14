/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/DEF.p
 > Purpose:         Extend functionality of "#_IF"
 > Author:          Aled Morris, Dec  8 1987 (see revisions)
 > Documentation:   HELP *DEF
 > Related Files:   src/rditem.p
 */

section;

define global macro DEF;
    lvars id = sys_current_ident(sys_read_path(readitem(), false, false));
    if id and isdefined(id) and fast_idval(id) then
        true
    else
        false
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Made it return true only for a strongly-declared perm ident.
--- Jonathan Meyer, Sep  6 1991
        Made it work with section pathnames.
--- John Gibson, Sep  1 1988
        Made it return true instead of idval(id) (because if idval is a
        word, goes into proglist and will get declared).
--- John Gibson, Apr  5 1988
        Changed to use -sys_current_ident-
 */
