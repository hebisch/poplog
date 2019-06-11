/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_plogshowlib.p
 > Purpose:        Viewing Prolog library files
 > Author:         Kathryn Seifert, Aug 27 1986 (see revisions)
 > Documentation:  HELP * PLOGSHOWLIB
 > Related Files:  LIB * VED_PROLOG
 */
compile_mode :pop11 +strict;

section;

uses ved_prolog;

define vars ved_plogshowlib();
    veddo('prolog showlib ' <> vedargument)
enddefine;

"ved_plogshowlib" -> vedgetsysfilepdr("PLOGSHOWLIB");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 28 1995
        Changed to use veddo
--- Robert John Duncan, Jul 14 1993
        Changed to be a synonym for <ENTER> prolog showlib
--- Aaron Sloman, Jan  8 1989 called vedsetup
--- Aaron Sloman, Nov  5 1986 made to work with <ESC> h
*/
