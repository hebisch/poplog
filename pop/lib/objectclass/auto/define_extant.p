/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_extant.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => define_extant;

sysunprotect( "define_extant" );

;;; This tries to use an existing key and coerce it into a
;;; class.  It also checks that this is safe.
define :define_form global extant;
    objectclass_form( "extantclass", ";", "enddefine" );
enddefine;

sysprotect( "define_extant" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
