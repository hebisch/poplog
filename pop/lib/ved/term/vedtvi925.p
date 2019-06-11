/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedtvi925.p
 >  Purpose:        FOR TELEVIDEO VDU MODEL TVI 925 (Non emulating)
 >  Author:         Jim Hunter, April 1983 (see revisions)
 >  Documentation:  HELP * VED, VEDKEYS
 >  Related Files:  LIB *TVI *TVI925KEYS *TVI925SCREEN
 */
compile_mode :pop11 +strict;

/*
    This package is provided for user convenience. It is not supported,
    and it may change.
*/

uses-by_name vedtvi925keys, vedtvi925screen;

section;

define vars vedtvi925();
    veduseterm("tvi925") -> ;
    identfn -> vedtvi925;
enddefine;

if iscaller(vedsetup) then vedtvi925() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Jul 12 1989
        Separated into "vedtvi925screen.p" and "vedtvi925keys.p" files.
*/
