/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/lib/number_classes.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section;

define :mixin number;
enddefine;

define :extant complex;
    isa number;
enddefine;

define :mixin real;
    isa number;
enddefine;

define :mixin rational;
    isa real;
enddefine;

define :extant ratio;
    isa rational;
enddefine;

define :mixin integral;
    isa rational;
enddefine;

define :extant integer;
    isa integral;
enddefine;

define :extant biginteger;
    isa integral;
enddefine;

define :mixin float;
    isa real;
enddefine;

define :extant decimal;
    isa float;
enddefine;

define :extant ddecimal;
    isa float;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
