/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_src.p
 >  Purpose:        Examining system source files
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * SRC, HELP * SRCFILES, HELP * VEDSYSFILE
 >  Related Files:  LIB * SRC
 */
compile_mode :pop11 +strict;

section;

vars
    ;;; (vedsrclist is declared incremental in popc_declare.ph)
    ;;; (vedsrcname initialised to 'srcfiles' in core system)

    ;;; N.B Should not contain $usepop/pop/x/src -- this is added
    ;;; by lib popxlib.

    vedsrclist =
        [[^('$popsrc/' dir_>< nullstring)               src]
         [^('$usepop/pop/ved/src/' dir_>< nullstring)   src]
         [^('$usepop/pop/lisp/src/' dir_>< nullstring)  src]
         [^('$usepop/pop/pml/src/' dir_>< nullstring)   src]
         [^('$usepop/pop/plog/src/' dir_>< nullstring)  src]
        ],
;

define vars ved_src();
    vedsysfile("vedsrcname", vedsrclist, false)
enddefine;

"ved_src" -> vedgetsysfilepdr("SRC");

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Dec 19 1993
        Removed $usepop/pop/x/src because it was being duplicated by
        lib popxlib.
--- John Gibson, Jul 30 1993
        Moved intialisation of vedsrcname into core Ved system
--- John Gibson, Apr  8 1992
        Added $usepop/pop/ved/src
--- John Williams, Jan  3 1991
        Added $usepop/pop/lisp/src/ and $usepop/pop/pml/src/
--- Simon Nichols, Oct  5 1990
        Changed $Xpop to $usepop/pop/x/
--- Ian Rogers, Sep  4 1990
        Added $Xpop/src/
--- John Williams, Feb  7 1990
        Removed redundant call to -vedsetup-
--- John Williams, Feb 13 1989
        Added assignment to -vedgetsysfilepdr-
 */
