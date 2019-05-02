/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_cond.p
 > Purpose:         LIB * FORMAT_PRINT ~[ ... ~] directive
 > Author:          John Williams, Nov 28 1995
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

section $-lisp$-fpr;

procedure(arg);
    lvars colon = f_colon, at = f_at, i, j, clauses, last_clause_colon, n;
    defaults arg next_f_arg();
    if at then
        fmatch_bracket(`[`, `]`) -> (i, j, , );
        if f_subsystem == "lisp" then pop_true(arg) -> arg endif;
        if arg then
            f_arg_index fi_- 1 -> f_arg_index;
            do_fpr_string(f_string, i, j)
        endif
    else
        read_clauses(`[`, `]`) -> (clauses, , last_clause_colon);
        datalength(clauses) -> n;
        if colon then
            ;;; check length(clauses) == 2
            if f_subsystem == "lisp" then pop_true(arg) -> arg endif;
            if arg then
                2
            else
                1
            endif
        else
            returnunless(isinteger(arg));
            if arg fi_>= 0 and arg fi_< n then
                arg fi_+ 1
            elseif last_clause_colon == true then
                n
            else
                return
            endif
        endif -> i;
        do_fpr_string(fast_subscrv(i, clauses), 1, false)
    endif
endprocedure -> f_proc(`[`);


misplaced_directive(% `]` %) -> f_proc(`]`);


endsection;
