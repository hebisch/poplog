/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:           C.all/lib/auto/exprread.p
 > Purpose:        procedure for reading an expression
 > Author:         Allan Ramsay,  1983 (see revisions)
 > Documentation:  HELP * FORMS,
 > Related Files:  LIB * IMPREAD   LIB * IMPSEQREAD
 */
compile_mode:pop11 +strict;

section;

define global exprread = proglist_read_by(%pop11_comp_expr%) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  3 1991
        Changed to use new -proglist_read_by-
--- John Gibson, Nov  5 1990
        Tidied up.
 */
