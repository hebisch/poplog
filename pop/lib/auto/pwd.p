/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/auto/pwd.p
 >  Purpose:        Print working directory
 >  Author:         John Gibson, Nov 1983 (see revisions)
 */

section;

define global constant syntax pwd;
    sysPUSH("current_directory");
    sysPUSH("pop_=>_flag");
    sysPUSHQ('%S%S');
    sysCALL("nprintf");
    ";" :: proglist -> proglist
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 25 1993
        Now uses new %S option to printf
--- John Williams, Jan 22 1993
        Sets pop_pr_quotes false.
--- John Gibson, Mar  8 1988
        Made a syntax word
--- John Williams, Sep 29 1987
        Changed to plant code into -proglist-
--- Aaron Sloman, Nov  9 1986
        Now uses current_directory
*/
