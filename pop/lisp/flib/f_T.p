/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_T.p
 > Purpose:         LIB * FORMAT_PRINT ~T directive
 > Author:          John Williams, Dec  9 1985 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

procedure(colnum, colinc);
    defaults colnum 1, colinc 1;
    check_positive(colnum);
    check_positive(colinc);
    if f_at then
        repeat colnum times
            cucharout(`\s`)
        endrepeat;
        until (f_charout_col() fi_rem colinc) == 0 do
            cucharout(`\s`)
        enduntil
    elseif colinc == 0 then
        while f_charout_col() fi_< colnum do
            cucharout(`\s`)
        endwhile
    else
        repeat
            repeat colinc times cucharout(`\s`) endrepeat;
            quitunless(f_charout_col() fi_< colnum)
        endrepeat
    endif
endprocedure -> f_proc(`T`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  7 1988
        Fixed SFR 4210 (colinc = 0)
 */
