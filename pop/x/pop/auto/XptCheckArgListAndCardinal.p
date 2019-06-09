/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckArgListAndCardinal.p
 > Purpose:         Check for ArgList & Cardinal
 > Author:          John Gibson, May  2 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

;;; Check for arglist/cardinal pair, coercing list/vector to shadowclass form
define XptCheckArgListAndCardinal(arglist);
    lvars arglist cardinal;
    if islist(arglist) or isvector(arglist) then
        XptArgList(arglist);
    else
        arglist -> cardinal;
        -> arglist;
        XptCheckArgList(arglist);
        XptCheckUnsignedInt(cardinal);
    endif;
enddefine;

endsection;
