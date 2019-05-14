/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/newprwarning.p
 > Purpose:         Version of prwarning that prints file and line number
 > Author:          John Williams, Nov 16 1990 (see revisions)
 > Documentation:   REF * MISHAPS
 > Related Files:   SRC * PERM_IDENT.P
 */
compile_mode :pop11 +strict;

section;

define newprwarning(word);
    dlocal pop_message_min_detail = max(4, pop_message_min_detail);
    sysprwarning(word)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1996
        Changed to use sysprwarning with new pop_message_min_detail set to 4
--- John Gibson, Feb 17 1996
        Changed to use sys_pr_message

 */
