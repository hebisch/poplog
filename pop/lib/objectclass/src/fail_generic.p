/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/fail_generic.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Handle the failure of a method ---------------------------------

global vars procedure fail_generic;

section $-objectclass;

define :method vars fail_generic( I, G ); lvars I, G;
    report_failure( I, G, false )
enddefine;

define :method updaterof fail_generic( I, G ); lvars I, G;
    report_failure( I, G, true )
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct  4 1995
        Popc changes (report_failure now in runtime library)
 */
