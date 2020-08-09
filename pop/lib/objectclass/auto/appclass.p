/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/appclass.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; appclass(CLASS, P) - applies P to every slot method of the class.

section $-objectclass => appclass;

sysunprotect( "appclass" );

define global vars procedure appclass(key, p); lvars key, p;
    applist( [% destclass(key) %], p );
enddefine;

sysprotect( "appclass" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
