/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_singleton.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => define_singleton;

sysunprotect( "define_singleton" );

define :define_form global singleton;
    objectclass_form( "singletonclass", ";", "enddefine" )
enddefine;

sysprotect( "define_singleton" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
