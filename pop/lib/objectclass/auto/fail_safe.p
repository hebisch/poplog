/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/fail_safe.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => fail_safe;

lvars procedure prev_method_failure;

define global procedure fail_safe( m ); lvars m;

    unless m.isgeneric then
        mishap( 'GENERIC PROCEDURE NEEDED', [^m] )
    endunless;

    dlvars succeeded = true;
    dlocal prev_method_failure = fail_generic;

    define dlocal fail_generic( m ); lvars m;
        if caller( 2 ) == fail_safe then
            false -> succeeded;
            erasenum( m.pdnargs );
        else
            chain( m, prev_method_failure )
        endif
    enddefine;

    m();
    succeeded;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
