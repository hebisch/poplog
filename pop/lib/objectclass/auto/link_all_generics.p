/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/link_all_generics.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => link_all_generics;

define link_all_generics();
    procedure( m ); lvars m;
        if m.isunlinked_method then
            m.relink_method
        endif
    endprocedure.app_all_methods
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
