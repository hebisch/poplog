/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/auto/sysxsqcomp.p
 > Purpose:        Old form of -pop11_comp_expr/stmnt_seq-
 > Author:         John Gibson, Jan 26 1987
 > Documentation:  REF * POPCOMPILE
 > Related Files:
 */

section;

define global constant procedure sysxsqcomp(allow_sc);
    lvars allow_sc;
    if allow_sc then
        pop11_comp_stmnt_seq()
    else
        pop11_comp_expr_seq()
    endif
enddefine;

endsection;
