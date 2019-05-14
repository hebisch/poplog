/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/poparg1.p
 > Purpose:         Return first argument (after % and + args) to POPLOG
 > Author:          John Williams, Apr 23 1992 (see revisions)
 > Documentation:   REF * poparg1
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include sysdefs.ph;

lvars POPARG1 = 0;

define active poparg1;
    lvars l;

    returnif (iscompound(POPARG1)) (POPARG1);

    define lconstant skip_args(l, char) -> l;
        lvars l, char, arg;
        while not(null(l)) and isstring(hd(l)->>arg) and datalength(arg) /== 0
        and fast_subscrs(1,arg) == char do
            tl(l) -> l
        endwhile
    enddefine;

    ;;; skip saved images (old style char)
    skip_args(tl(poparglist0), #_IF DEF VMS `/` #_ELSE `-` #_ENDIF) -> l;
    ;;; skip saved images
    skip_args(l, `+`) -> l;
    ;;; skip special flags
    skip_args(l, `%`) -> l;

    (not(null(l)) and hd(l)) ->> POPARG1;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  5 1996
        Caches result in file lvar POPARG1.
--- John Gibson, Aug 17 1995
        Further simplified
--- John Gibson, Aug 11 1995
        Completely rewritten again -- now works properly in Unix and VMS
--- John Williams, Jan  5 1995
        Completely re-written because original version didn't work!
 */
