/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/class_infs.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; -- infs_of_objectclass --------------------------------------------------

section $-objectclass => class_infs;

define global vars procedure class_infs( c ); lvars c;
    [% dump_infs_of_class( c ) %]
enddefine;

sysprotect( "class_infs" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
