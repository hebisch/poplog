/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_escape.p
 > Purpose:         LIB * FORMAT_PRINT ~^ directive
 > Author:          John Williams, Nov  2 1994 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

f_proc(`{`) ->;         ;;; Ensure f_loop and fl_colon are defined


define f_escape();
    if fl_colon == false then
        ;;; inside ~{ .. ~} (with NO initial colon)
        exitfrom(f_loop)
    else
        exitfrom(do_fpr_string)
    endif
enddefine;


define f_escape_needed(x, y, z);
    if x then
        if y then
            if z then
                (z > y) and (y > x)
            else
                x = y
            endif
        else
            x = 0
        endif
    else
        f_arg_index fi_>= f_arg_len
    endif
enddefine;


procedure() with_nargs 3;
    if f_escape_needed() then
        fast_chain(f_escape)
    endif
endprocedure -> f_proc(`^`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 */
