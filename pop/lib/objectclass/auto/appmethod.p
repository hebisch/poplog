/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/appmethod.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; Iterate over all the (sorted) paths and actions.

section $-objectclass => appmethod;

sysunprotect( "appmethod" );

define global vars procedure appmethod( method, proc );
    lvars method, procedure proc;
    lvars t = method.method_table;
    lockMethodTable( t );
    lvars e;
    for e in cEntriesMethodTable( method.method_table ) do
        proc(
            [% e.classesEntry.explode_tmpvec %],
            e.actionEntry
        )
    endfor;
    unlockMethodTable( t );
enddefine;

sysprotect( "appmethod" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
