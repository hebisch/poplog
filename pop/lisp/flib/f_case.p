/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_case.p
 > Purpose:         LIB * FORMAT_PRINT ~( ... ~) directive
 > Author:          John Williams, Nov  2 1994 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

#_IF not(is_subsystem_loaded("lisp"))
global vars active $-lisp$-print_case;
#_ENDIF


define f_case(string, i, j, colon, at);
    dlvars First = true,
            procedure (Convert_case, Saved_cucharout = cucharout);
    dlocal print_case = nil;

    define dlocal cucharout() with_nargs 1;
        Saved_cucharout(Convert_case())
    enddefine;

    if colon and at then
        /* All uppercase output */
        lowertoupper

    elseif at then
        /* Capitalise first word */

        procedure(c) -> c;
            lvars c;
            if isalphacode(c) then
                lowertoupper(c) -> c;
                uppertolower -> Convert_case
            endif
        endprocedure

    elseif colon then
        /* Capitalise each word */

        dlvars procedure Convert_case_p = lowertoupper;

        procedure(c);
            lvars c;
            Convert_case_p(c);
            if isalphacode(c)
            or isnumbercode(c)
            or strmember(c, '\'_-') then
                uppertolower
            else
                lowertoupper
            endif -> Convert_case_p
        endprocedure

    else
        /* All lowercase output */
        uppertolower

    endif -> Convert_case;

    do_fpr_string(string, i, j)
enddefine;


procedure();
    lvars i, j;
    fmatch_bracket(`(`, `)`) -> (i, j, , );
    f_case(f_string, i, j, f_colon, f_at)
endprocedure -> f_proc(`(`);


misplaced_directive(% `)` %) -> f_proc(`)`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar  6 1996
        Uses `is_subsystem_loaded("lisp")' to decide whether to declare
        Lisp variables.
--- John Williams, Apr  3 1995
        $-lisp$-print_case now an active variable.
--- John Williams, Dec 16 1994
        Major re-write of f_cconvert.p (which this file replaces).
 */
