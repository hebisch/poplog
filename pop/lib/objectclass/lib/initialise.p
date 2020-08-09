/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/lib/initialise.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section;

global vars initialise;
define :generic initialise( item );
    ;;; Nothing by default.  User should override this for their class.
enddefine;

define lconstant procedure do_initialise();
    lvars item = ()();
    initialise( item );
    item;
enddefine;

define :mixin initialise;
    on new do do_initialise;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
