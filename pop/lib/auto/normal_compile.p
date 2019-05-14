/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/normal_compile.p
 > Purpose:         Escape from VM 'pas' mode (e.g. when running popc)
 > Author:          John Gibson, Jan 29 1989 (see revisions)
 > Documentation:
 */
compile_mode :pop11 +strict;

section;

constant syntax end_normal_compile = pop_undef;
;;;
define syntax normal_compile;
    dlocal pop_pas_mode = false;
    pop11_exec_stmnt_seq_to("end_normal_compile") ->
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
 */
