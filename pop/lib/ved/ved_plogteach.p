/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_plogteach.p
 > Purpose:        looks for Prolog teach files
 > Author:         Kathryn Seifert, Aug 29 1986 (see revisions)
 > Documentation:  HELP * PLOGTEACH
 > Related Files:  LIB * PLOGHELP, HELP * PLOGHELP, LIB * PLOGTEACH
 */
compile_mode :pop11 +strict;

section;

uses-by_name (prolog_compile);

vars plogteachlist = [
 ['$poplocal/local/plog/teach/' teach prolog_compile]
 ['$poplocal/local/plog/help/'  help  prolog_compile]
 ['$usepop/pop/plog/teach/'     teach prolog_compile]
 ['$usepop/pop/plog/help/'      help  prolog_compile]
 ^(ident vedteachlist)
];

define vars ved_plogteach();
    vedsetup(); ;;; ensure vedinit.p compiled
    vedsysfile("vedteachname", plogteachlist,
                procedure();
                    "teach" -> vedfileprops;
                    vedhelpdefaults();
                endprocedure)
enddefine;

"ved_plogteach" -> vedgetsysfilepdr("PLOGTEACH");

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  2 1992
        Uses ident vedteachlist not list itself, and name of prolog_compile
        not procedure.
--- Aaron Sloman, Jan  8 1989 called vedsetup
--- John Williams, Apr 24 1987 - changed $usepop/pop to $poplocal
--- Aaron Sloman, Nov  5 1986 made to work with <ESC> h
*/
