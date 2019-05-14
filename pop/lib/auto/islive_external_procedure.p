/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/islive_external_procedure.p
 > Purpose:         Test live external procedure
 > Author:          John Gibson, Apr 27 1990 (see revisions)
 > Documentation:   REF *EXTERNAL_DATA
 */

compile_mode:pop11 +strict;

section;

weak constant procedure isexternal_procedure;

define global islive_external_procedure(expdr);
    lvars expdr;
    if isexternal_procedure(expdr) then
        is_valid_external_ptr(expdr)
    else
        mishap(expdr, 1, 'EXTERNAL PROCEDURE NEEDED')
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, May 24 1990
        Now declared as global.
 */
