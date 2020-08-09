/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/output_locals.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => output_locals;

;;; Relies on the dlocalised variable OutLocals being available.

define global vars syntax output_locals;
    unless OutLocals then
        mishap('CANNOT FIND OUTPUT LOCALS (not inside method?)', []);
    elseif pop_expr_update then
        revapplist(OutLocals, local_name <> sysPOP);
        null_instruction -> pop_expr_inst;
    else
        applist(OutLocals, local_name <> sysPUSH);
    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  1 1995
        Different format for OutLocals
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
