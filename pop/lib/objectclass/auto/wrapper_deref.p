/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/wrapper_deref.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section;

sysunprotect( "wrapper_deref" );

define global procedure wrapper_deref( p );
    p.isclosure and pdprops( p ) or p
enddefine;

sysprotect( "wrapper_deref" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Steve Leach, 19th Sept 2002
        Obsoleted in favour of -wrapper_kernel-.
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
