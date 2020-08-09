/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_mixin.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => define_mixin;

sysunprotect( "define_mixin" );

;;; This is the way of introducing new mixins (classes that cannot be instantiated)
define :define_form global mixin;
    objectclass_form( "mixinclass", ";", "enddefine" )
enddefine;

sysprotect( "define_mixin" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
