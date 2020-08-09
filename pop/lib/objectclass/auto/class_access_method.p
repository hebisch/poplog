/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/class_access_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; class_access_method(N, KEY) -> SLOT_P - returns Nth slot access procedure

section $-objectclass => class_access_method;

sysunprotect( "class_access_method" );

define global procedure class_access_method( n, key ); lvars n, key;
    unless key.isclass then
        mishap( 'CLASS NEEDED', [^key] );
    endunless;
    lvars slot = subscrl( n, fields_of_class( key ) );
    xslot_identity( slot );
enddefine;

sysprotect( "class_access_method" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
