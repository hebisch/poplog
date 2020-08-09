/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/destclass.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; destclass(CLASS) -> (SLOT1_P, SLOT2_P, ...) - returns slots of the class

section $-objectclass => destclass;

sysunprotect( "destclass" );

define global vars procedure destclass(key); lvars key;
    unless key.isclass then
        mishap( 'CLASS NEEDED', [^key] );
    endunless;
    lvars L = fields_of_class( key );
    applist( L, xslot_identity );
enddefine;

sysprotect( "destclass" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
