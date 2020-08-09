/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/consmethodclosure.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => consmethodclosure;

define global procedure consmethodclosure( n ) -> c; lvars n, c;
    newindirect_n( n ) -> c;
    lvars m = pdpart( c );

    define lconstant unlinker( C, M ); lvars C, M;
        procedure();
            if M.isunlinked_method then
                relink_method( M )
            endif;
            chain( M )
        endprocedure -> pdpart( C )
    enddefine;

    define lconstant relinker( C, M ); lvars C, M;
        pdpart( M ) -> pdpart( C )
    enddefine;

    unless m.isgeneric do
        mishap( 'CONSMETHODCLOSURE: method needed', [^m] )
    endunless;
    add_dependent( unlinker, relinker, [^c], m );
    unlinker( c, m );
enddefine;

sysprotect( "consmethodclosure" );

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
