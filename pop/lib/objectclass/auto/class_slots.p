/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/class_slots.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => class_slots;

sysunprotect( "class_slots" );

define global class_slots( c ); lvars c;
    if c.isclass then
        [%
            lvars f;
            for f in fields_of_class( c ) do
                xslot_identity( f )
            endfor;
        %]
    else
        mishap( 'Objectclass needed', [^c] )
    endif;
enddefine;

sysprotect( "class_slots" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
