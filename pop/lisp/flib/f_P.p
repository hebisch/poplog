/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_P.p
 > Purpose:         LIB * FORMAT_PRINT ~P directive
 > Author:          John Williams, Dec 9 1985 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

procedure();
    lvars arg;
    if f_colon then
        f_arg_index fi_- 1 -> f_arg_index
    endif;
    next_f_arg() -> arg;
    if f_at then
        cuch_string(if arg == 1 then 'y' else 'ies' endif)
    elseunless arg == 1 then
        cucharout(`s`)
    endif
endprocedure -> f_proc(`P`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Dec 16 1994
        Uses cuch_string instead of sys_syspr.
 */
