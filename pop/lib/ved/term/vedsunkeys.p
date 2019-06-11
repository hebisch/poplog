/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/ved/term/vedsunkeys.p
 > Purpose:        Set up VED key bindings for Sun keyboard (except under X)
 > Author:         Ben Rubinstein, July 16 1986 (see revisions)
 > Documentation:  HELP * VEDSUN
 > Related Files:  LIB * vedsunkeys_SUNVIEW, LIB * vedsunkeys_NOSUNVIEW
 */
compile_mode :pop11 +strict;

/*
There are two possible keymaps for the Sun keyboard, one for use with
SunViewKeys ON (the default) and one for use with SunViewKeys OFF.

See HELP * VEDSUN for further information.
 */

section;

;;; N.B. This doesn't load either until required
uses-by_name vedsunkeys_NOSUNVIEW, vedsunkeys_SUNVIEW;

vars vednosunviewkeys;
if isundef(vednosunviewkeys) then
    false -> vednosunviewkeys
endif;

define vars vedsunkeys();
    valof(  if vednosunviewkeys then
                "vedsunkeys_NOSUNVIEW"
            else
                "vedsunkeys_SUNVIEW"
            endif) ()
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 15 1992
        Changed to just use valof on the appropriate name.
--- John Williams, Oct 17 1990
        Now uses -vednosunviewkeys-
--- Rob Duncan, Oct 25 1989
        Moved files from POPSUNLIB to POPVEDLIB/term
--- Rob Duncan, Oct 13 1989
        Simplified the comment.
--- Jason Handby, Jul 13 1989
        Removed screen setup and placed it in LIB * VEDSUNSCREEN.
*/
