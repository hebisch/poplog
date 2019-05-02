/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_justify.p
 > Purpose:         LIB * FORMAT_PRINT ~< ... ~> directive
 > Author:          John Williams, Nov 28 1995
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;
include ved_declare.ph;

section $-lisp;

global constant procedure
    (is_pp_stream, make_standard_output, pprint_logical_block, pps_charout);

global vars procedure (lisp_pr), active (standard_output), pp_current_state;


section fpr;

unless isdefined("fbuffer") do
    syslibcompile("fbuffer", f_liblist) ->
endunless;


define f_lb_prefixes(clauses, first_clause_args, colon)
                        -> (prefix, suffix, line_prefix, body);
    lvars n;
    datalength(clauses) -> n;
    if n == 0 or n fi_> 3 then
        mishap('Too many/few clauses in ~< ... ~:> directive', [^clauses])
    endif;
    colon and '(' -> prefix;
    colon and ')' -> suffix;
    false -> line_prefix;
    explode(clauses);
    if n == 3 then
        -> suffix -> body -> prefix
    elseif n == 2 then
        -> body -> prefix
    elseif n == 1 then
        -> body;
    endif;
    if first_clause_args
    and strmember(`@`, first_clause_args) then
        prefix -> line_prefix;
        false -> prefix
    endif
enddefine;


define f_lb(prefix, suffix, line_prefix, body, at);
    dlvars prefix, suffix, line_prefix, body, at, args;

    if at then
        [% fast_for f_arg_index from (f_arg_index fi_+ 1) to f_arg_len do
            fast_subscrv(f_arg_index, f_args)
        endfor %]
    else
        next_f_arg()
    endif -> args;

    define lconstant Do_lb();
        if ispair(args) and fast_back(lastpair(args)) /== [] then
            lisp_pr(args)
        else
            pprint_logical_block(
                standard_output, args, prefix, suffix, line_prefix,
                do_fpr_args(% body, args, 1, false %))
        endif
    enddefine;

    dlocal standard_output;

    if is_pp_stream(standard_output) then
        procedure();
            dlocal % pps_charout(pp_current_state) %
                        = cucharout,
                ;
            Do_lb()
        endprocedure()
    else
        make_standard_output(cucharout);
        Do_lb()
    endif
enddefine;


define lconstant Linemax();
    if testdef vedprocess and weakref[vedprocess] vedinvedprocess then
        weakref[vedprocess] vedlinemax
    else
        poplinemax
    endif
enddefine;


f_proc(`;`) ->;         /* Ensure f_clause is defined */


define f_justify(w, colinc, minpad, padchar,
                clauses, first_clause_args, colon, at);

    lvars clause, nchars = 0, nclauses, nspaces, ngaps, nbig, nsmall,
            leftsp, clauses, big_gap, small_gap;

    dlvars Fj_1st_clause = false, Fj_linemax;

    defaults w 0, colinc 1, minpad 0, padchar `\s`;
    check_positive(w);
    check_positive(colinc);
    check_positive(minpad);

    [% appdata(
        clauses,
        procedure(clause);
            dlocal fbuffer_i, cucharout = fbuffer_charout;
            lvars i = fbuffer_i;
            do_fpr_string(clause, 1, false);
            substring(i fi_+ 1, fbuffer_i fi_- i, fbuffer);
            datalength(dup()) fi_+ nchars -> nchars
        endprocedure)
    %] -> clauses;

    if first_clause_args
    and fast_subscrs(2, first_clause_args) /== `;` then
        procedure();
            define dlocal f_clause(x, y);
                if f_colon then
                    fast_destpair(clauses) -> (Fj_1st_clause, clauses);
                    defaults x 0;
                    defaults y (Linemax());
                    y - x -> Fj_linemax
                endif
            enddefine;

            do_fpr_string(first_clause_args, 1, false)
        endprocedure();
    endif;

    listlength(clauses) -> nclauses;
    nclauses fi_- 1 -> ngaps;
    if nclauses == 1 and not(at) then
        true -> colon
    endif;
    if colon then
        ngaps fi_+ 1 -> ngaps
    endif;
    if at then
        ngaps fi_+ 1 -> ngaps
    endif;
    round(max(w fi_- (ngaps fi_* minpad) fi_- nchars, 0) / colinc) -> nspaces;

    if Fj_1st_clause
    and (Fj_linemax fi_- f_charout_col()) fi_< (nchars fi_+ (nspaces * colinc))
    then
        cuch_string(Fj_1st_clause)
    endif;

    nspaces fi_// ngaps -> small_gap -> nbig;
    ngaps fi_- nbig -> nsmall;
    minpad fi_+ (small_gap fi_* colinc) -> small_gap;
    small_gap fi_+ colinc -> big_gap;
    if colon then
        if nbig == 0 then
            nsmall fi_- 1 -> nsmall, small_gap
        else
            nbig fi_- 1 -> nbig, big_gap
        endif
    else
        0
    endif -> leftsp;

    cuch_chars(leftsp, padchar);
    fast_repeat nbig times
        cuch_string(fast_destpair(clauses) -> clauses);
        cuch_chars(big_gap, padchar)
    endrepeat;
    fast_repeat nsmall times
        cuch_string(fast_destpair(clauses) -> clauses);
        cuch_chars(small_gap, padchar)
    endrepeat;
    unless clauses == nil do
        cuch_string(fast_front(clauses))
    endunless
enddefine;


procedure(w, colinc, minpad, padchar);
    lvars colon = f_colon, at = f_at, clauses, first_clause_args, clause;

    read_clauses(`<`, `>`) -> (clauses, first_clause_args, );

    if f_colon and f_subsystem == "lisp" then           ;;; ended with ~:>
        f_lb(f_lb_prefixes(clauses, first_clause_args, colon), at)
    else
        f_justify(w, colinc, minpad, padchar,
                  clauses, first_clause_args, colon, at)
    endif;
endprocedure -> f_proc(`<`);


misplaced_directive(% `>` %) -> f_proc(`>`);


endsection;         ;;; fpr

endsection;         ;;; lisp
