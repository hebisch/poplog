/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_has_clauses.p
 > Purpose:         Prolog: test whether a predicate has clauses
 > Author:          Robert John Duncan, Jul  7 1993
 > Documentation:
 > Related Files:
 */

section prolog;

constant
    procedure ( predicate_isdefined, is_dynamic, proc_clauses, );

;;; ========================================================================

define predicate_has_clauses(fn, arity) -> val;
    lvars fn, arity, val;
    if predicate_isdefined(fn, arity) ->> val then
        lvars clauselist;
        if is_dynamic(val) ->> clauselist then
            Back(clauselist)
        else
            proc_clauses(val)
        endif /== [] -> val;
    endif;
enddefine;

endsection;     /* prolog */
