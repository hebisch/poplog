/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/input_locals.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => input_locals;

;;; Relies on the dlocalised permanent variable InLocals being available.

define global vars syntax input_locals;
    unless InLocals do
        mishap( 'CANNOT FIND INPUT LOCALS (not inside method?)', [] )
    elseif pop_expr_update then
        applist( InLocals, local_name <> sysPOP );
        null_instruction -> pop_expr_inst
    else
        revapplist( InLocals, local_name <> sysPUSH )
    endunless
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  1 1995
        Different format for InLocals
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
