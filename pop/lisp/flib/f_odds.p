/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_odds.p
 > Purpose:         LIB * FORMAT_PRINT ~&, ~<newline>, ~* and ~? directives
 > Author:          John Williams, Nov  2 1994 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

;;; ~&

procedure(n);
    defaults n 1;
    if f_charout_col() == 0 then
        n - 1 -> n
    endif;
    cuch_chars(n, `\n`)
endprocedure -> f_proc(`&`);


;;; ~<newline>

procedure();
    if f_at then
        cucharout(`\n`)
    endif;
    unless f_colon do
        /* Gobble white space */
        until (next_f_char(), f_char fi_> `\s`) do enduntil;
        f_str_index fi_- 1 -> f_str_index
    endunless
endprocedure -> f_proc(`\n`);


;;; Resetting the current argument  ~*

procedure(n);
    defaults n 1;
    if f_at then               ;;; absolute "goto"
        n - 1
    else                       ;;; relative goto
        if f_colon then
            f_arg_index - n        ;;; backing up F_ARGS
        else
            f_arg_index + n
        endif
    endif -> f_arg_index
endprocedure -> f_proc(`*`);


;;; Recursive formatted printing ~?

procedure();
    if f_at then
        do_fpr_string(next_f_arg(), 1, false)
    else
        do_fpr_args(next_f_arg(), next_f_arg(), 1, false) ->
    endif
endprocedure -> f_proc(`?`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Dec 16 1994
        pr_n_chars renamed cuch_chars and now defined in format_print.p
 */
