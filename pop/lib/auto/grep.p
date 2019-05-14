/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/auto/grep.p
 >  Purpose:        search a set of files for a string
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

include sysdefs.ph;

lconstant cmnd = #_IF DEF UNIX 'grep ' #_ELSE 'search ' #_ENDIF;

define vars macro grep;
    lvars x = 0, char;
    until (nextchar(itemread) ->> char) == termin or char == `\n` do
        char; x + 1 -> x
    enduntil;
    consstring(x) -> x;
    pr(newline);    ;;; in case used in VED.
    sysobey(cmnd <> x)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Dec  6 1993
        Commoned between Unix and VMS
--- Mark Rubinstein, Aug  5 1985 - sectioned, lvarsed and made to work on UNIX
 */
