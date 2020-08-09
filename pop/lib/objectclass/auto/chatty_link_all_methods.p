/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/chatty_link_all_methods.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => chatty_link_all_methods;

define chatty_link_all_methods();
    dlocal popgctrace = popgctrace or true;
    lvars gct = popgctime;
    nl(1);
    npr( 'Linking all methods' );
    npr( '-------------------' );
    lvars t = systime();
    lvars ( M, N ) = ( 0, 0 );
    procedure( m ); lvars m;
        if m.isunlinked_method then
            timediff().erase;
            m.relink_method;
            lvars t = timediff();
            nprintf( '  %p secs for method %p', [^t ^(m.pdprops)] );
            N + 1 -> N;
        else
            M + 1 -> M;
        endif
    endprocedure.app_all_methods;
    ( systime() - t ) / 100.0 -> t;
    ( popgctime - gct ) / 100.0 -> gct;
    nprintf( 'CPU time for linking %p methods was %p secs', [^(M+N) ^t] );
    nprintf( 'GC time was %p', [^gct] );
    nprintf( '%p methods were already linked', [^M] );
    nprintf( '%p methods needed relinking', [^N] );
    nl(1);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
