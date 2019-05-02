/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/umacs.p
 > Purpose:         User-defined Common Lisp read macros
 > Author:          John Williams, Feb 25 1986 (see revisions)
 > Documentation:   CLtL, p360-364
 > Related Files:   C.all/lisp/src/readutils.p
 */

lisp_compile_mode;

section $-lisp;


define user_char_mac() with_nargs 1;
    isfunction_info(pdprops())
enddefine;


define apply_user_char_mac(pdr, nargs);
    lvars sl;
    stacklength() fi_- nargs -> sl;
    lisp_apply(pdr, nargs, false);
    stacklength() fi_- sl -> sl;
    str_input(standard_input) -> cucharin;
    unless sl == 1 do
        if sl == 0 then
            chainfrom(lispreaditem, lispreaditem)
        else
            erasenum(sl fi_- 1)
        endif
    endunless
enddefine;


define :inline lconstant SET_READTABLE_DEFAULT(rt);
    unless pop_true(rt) do
        ;;; defaults
        readtable -> rt
    endunless
enddefine;


define set_macro_char(char, pdr, nt, rt);
    SET_READTABLE_DEFAULT(rt);
    checkr_function(pdr) -> pdr;
    if pop_true(nt) then
        ntmac(% pdr %)
    else
        tmac(% pdr %)
    endif -> readtable_pdr(char, rt);
    true
enddefine;


define get_macro_char(char, rt);
    lvars pdr;
    SET_READTABLE_DEFAULT(rt);
    readtable_pdr(char, rt) -> pdr;
    if is_char_mac(pdr) then
        fast_frozval(1, pdr);
        lisp_true(pdpart(pdr) == ntmac)
    else
        nil, nil
    endif
enddefine;


/* Dispatch macros */

constant procedure (dmac exec_non_dmac);


define make_dmac_char(char, nt, rt);
    lvars dt;
    SET_READTABLE_DEFAULT(rt);
    initvectorclass(256, exec_non_dmac, vector_key) -> dt;
    if pop_true(nt) then
        ntmac(% dmac(% dt %) %)
    else
        tmac(% dmac(% dt %) %)
    endif -> readtable_pdr(char, rt);
    true
enddefine;


define lconstant Get_dmac_table(char, rt);
    lvars pdr;
    readtable_pdr(char, rt) -> pdr;
    if is_char_mac(pdr)
    and pdpart(fast_frozval(1, pdr) ->> pdr) == dmac then
        fast_frozval(1, pdr)
    else
        mishap(char, 1, 'Dispatch macro character needed')
    endif
enddefine;


define get_dmac_char(char, subchar, rt) -> pdr;
    SET_READTABLE_DEFAULT(rt);
    lowertoupper(char_code(subchar)) -> subchar;
    fast_subscrv0(subchar, Get_dmac_table(char, rt)) -> pdr;
    if pdr == exec_non_dmac then
        nil -> pdr
    endif
enddefine;


define set_dmac_char(char, subchar, pdr, rt);
    SET_READTABLE_DEFAULT(rt);
    if pdr == nil then
        exec_non_dmac
    else
        checkr_function(pdr)
    endif -> pdr;
    lowertoupper(char_code(subchar)) -> subchar;
    if isnumbercode(subchar) then
        mishap(CHARACTER subchar, 1,
                'Dispatch macro "sub-char" must not be a decimal digit')
    endif;
    pdr -> fast_subscrv0(subchar, Get_dmac_table(char, rt));
    true
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Apr  3 1995
        Changes for CLtL 2 ch 22 (Input/Output).
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
 */
