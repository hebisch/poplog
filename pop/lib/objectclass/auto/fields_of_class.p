/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/fields_of_class.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section;

sysunprotect( "fields_of_class" );

lconstant procedure foc = $-objectclass$-fields_of_class;

define global vars procedure fields_of_class( c ); lvars c;
    maplist( c.foc, copy )
enddefine;

sysprotect( "fields_of_class" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
