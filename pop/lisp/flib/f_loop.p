/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_loop.p
 > Purpose:         LIB * FORMAT_PRINT ~{ ... ~} directive
 > Author:          John Williams, Oct 16 1985 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;


vars fl_colon = pop_undef;  /* Records whether : was present in ~{ directive */


define f_loop(f_string, i, j, n, at_least1, fl_colon);
    dlocal f_string, fl_colon;
    if i == j then
        next_f_arg() -> f_string;
        1 -> i;
        false -> j
    endif;
    defaults n (f_arg_len fi_- f_arg_index);
    if at_least1 then
        if fl_colon then
            do_fpr_args(f_string, next_f_arg(), i, j) ->
        else
            do_fpr_string(f_string, i, j)
        endif;
        n fi_- 1 -> n
    endif;
    fast_repeat n times
        quitif(f_arg_index fi_>= f_arg_len);
        if fl_colon then
            do_fpr_args(f_string, next_f_arg(), i, j) ->
        else
            do_fpr_string(f_string, i, j)
        endif
    endrepeat
enddefine;


define f_loop_args(string, i, j, n, at_least1, fl_colon, f_args);
    dlocal fl_colon, f_args, f_arg_len, f_arg_index;
    if islist(f_args) then
        consvector(destlist(f_args)) -> f_args
    elseunless isvector(f_args) do
        mishap(f_args, 1, 'List or Vector Needed')
    endif;
    0 -> f_arg_index;
    datalength(f_args) -> f_arg_len;
    f_loop(string, i, j, n, at_least1, fl_colon)
enddefine;


procedure(n);
    lvars i, j, n, at_least1;
    dlocal fl_colon = f_colon;
    fmatch_bracket(`{`, `}`) -> (i, j, at_least1, );
    returnif(n == 0);
    if f_at then
        f_loop(f_string, i, j, n, at_least1, fl_colon)
    else
        f_loop_args(f_string, i, j, n, at_least1, fl_colon, next_f_arg())
    endif
endprocedure -> f_proc(`{`);


misplaced_directive(% `}` %) -> f_proc(`}`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 25 1995
        Tidied up variable declarations (parameters are now lvars by default).
 */
