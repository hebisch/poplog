/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_shared_slot.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; -- define :shared_slot ------------------------------------------------

section $-objectclass => define_shared_slot;

define :define_form global shared_slot();
    method_form( "SHARED_SLOT", "enddefine" )
enddefine;

sysprotect( "define_shared_slot" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
