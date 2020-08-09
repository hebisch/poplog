/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/review_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => review_method;

sysunprotect( "review_method" );

define global review_method( m ); lvars m;
    lvars name = m.pdprops;
    lvars mt = m.method_table;
    lvars c = mt.cEntriesMethodTable;
    lvars u = mt.uEntriesMethodTable;

    nprintf( 'pdprops:\t%p', [^name] );

    nprintf( 'arity:\t%p', [% mt.cArityMethodTable %] );
    nprintf( 'pdnargs:\t%p', [% m.pdnargs %] );
    nprintf( 'number of parts:\t%p', [% c.length %] );

    nprintf( 'updater-arity:\t%p', [% mt.uArityMethodTable %] );
    nprintf( 'updater-pdnargs:\t%p', [% if m.updater then m.updater.pdnargs else false endif %] );
    nprintf( 'number of updater-parts:\t%p', [% u.length %] );
enddefine;

sysprotect( "review_method" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
