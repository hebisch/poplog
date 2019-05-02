/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/set.p
 > Purpose:         SETQ and PSETQ special-forms
 > Author:          John Williams, Nov  7 1985 (see revisions)
 > Documentation:   CLtL, p91
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;


define Setq(forms, nresults);
    lvars sym, val;
    nil -> sym;
    until endp(forms) do
        fast_destpair(forms) -> (sym, forms);
        if endp(forms) then
            program_error('No value for ~S in SETQ form', [^sym])
        else
            fast_destpair(forms) -> (val, forms);
            if symbol_macro(sym) then
                compile_form([^@SETF ^sym ^val], 0)
            else
                compile_form(val, 1);
                lispPOP(sym)
            endif
        endif
    enduntil;
    lispPUSHN(sym, nresults)
enddefine;


define Psetq(forms, nresults);
    lvars list, syms, vals, sym;
    dlocal Stackoffset;
    forms -> list;
    [] -> syms;
    [] -> vals;
    until endp(list) do
        fast_destpair(list) -> (sym, list);
        if endp(list) then
            program_error('No value for ~S in PSETQ form', [^sym])
        elseif symbol_macro(sym) then
            chain(conspair(@PSETF, forms), nresults, compile_form)
        else
            conspair(sym, syms) -> syms;
            conspair(fast_destpair(list) -> list, vals) -> vals
        endif
    enduntil;
    compile_args(fast_ncrev(vals)) ->;
    applist(syms, lispPOP);
    sys_grbg_list(vals);
    sys_grbg_list(syms);
    lispPUSHNQ(nil, nresults)
enddefine;


define set(sym, value) -> value;
    value -> symbol_value(sym)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Apr 26 1994
        Changes for symbol macros.
 */
