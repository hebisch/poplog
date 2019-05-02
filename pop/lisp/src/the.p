/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/the.p
 > Purpose:         Common Lisp THE construct
 > Author:          John Williams, Dec 18 1986 (see revisions)
 > Documentation:   CLtL, ch 9.3
 > Related Files:   C.all/lisp/src/typep.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair;


define lconstant Signal_the_error(val, type);
    fast_chain(false, val, type, type_cerror)
enddefine;


define lconstant Check_the_values(i, types);
    lvars val, type;

    dlocal % list_type_predicate(@FUNCTION) %
            = (procedure(item, type);
                   functionp(item)
               endprocedure);

    /* i is stack index of first value to be tested */

    until endp(types) do
        if i == 0 then
            lisp_cerror(
                'Carry on regardless',
                'Excess type specifier(s) in THE special form',
                types);
            return
        endif;
        subscr_stack(i) -> val;
        i fi_- 1 -> i;
        destpair(types) -> (type, types);
        unless typep(val, type) do
            Signal_the_error(val, type)
        endunless
    enduntil
enddefine;


define The(form, nresults);
    lvars types, nvals;
    unless islistlength(form, 2) do
        program_error('Malformed THE expression', [])
    endunless;
    destlist(form) -> (types, form, );
    unless Safety > Speed do
        chain(form, nresults, compile_form)
    endunless;
    if types starts_with @VALUES then
        back(types)
    else
        [^types]
    endif -> types;
    if isinteger(nresults) then
        nresults -> nvals
    elseif atom(form) then
        1 -> nvals
    else
        sysNEW_LVAR() -> nvals;
        sysCALLQ(stacklength);
        sysPOP(nvals)
    endif;
    if Space <= Speed
    and nvals == 1
    and islistlength(types, 1)
    and recursive_front(types) /== @FUNCTION
    then
        [^@QUOTE ^(front(types))] -> types;
        if (ispair(form) and front(form) /== @QUOTE)
        or (issymbol(form) and symbol_macro(form)) then
            compile_form(form, 1);
            gensymbol('T') -> form;
            lispLOCAL_POP(form)
        endif;
        compile_form(
            [^@IF [^@TYPEP ^form ^types]
                    ^form
                    [^Signal_the_error ^form ^types]],
            nresults)
    else
        compile_form(form, nresults);
        if isinteger(nvals) then
            sysPUSHQ(nvals)
        else
            sysCALLQ(stacklength);
            sysPUSH(nvals);
            sysCALLQ(nonop fi_-);
            conspair(nvals, pop_new_lvar_list) -> pop_new_lvar_list
        endif;
        sysPUSHQ(types);
        sysCALLQ(Check_the_values)
    endif
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
 */
