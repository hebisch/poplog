/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_class.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => define_class;

sysunprotect( "define_class" );

define :define_form global class;
    objectclass_form(
        "objectclass",
        ";",            ;;; the keyword that starts the field list
        "enddefine"     ;;; the keyword that ends the field list
    );
enddefine;

sysprotect( "define_class" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
