/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/export.p
 > Purpose:         Export to Lisp core functions defined in POP11
 > Author:          John Williams, May 29 1987 (see revisions)
 */

lisp_compile_mode;

section $-lisp;

define lconstant Compile_optional_export(fmin, fmax, pdr);
    lvars labs, lab;
;;; This procedure should use 'compile_optim_roargs' in LAMLISTS.P
    sysPROCEDURE(false, fmin);
    [% fast_repeat (fmax + 1) times sysNEW_LABEL() endfast_repeat %] -> labs;
    sysNEW_LABEL() -> lab;
    lispNARGS();
    lispGO_ON(labs, lab);
    fast_repeat fmin times
        sysLABEL(fast_destpair(labs) -> labs)
    endfast_repeat;
    sysLABEL(lab);
    sysCALLQ(runtime_nargs_error);
    fast_repeat (fmax - fmin) times
        sysLABEL(fast_destpair(labs) -> labs);
        sysPUSHQ(false);
    endfast_repeat;
    sysLABEL(fast_front(labs));
    sysCALLQ(pdr);
    sysENDPROCEDURE()
enddefine;


define lconstant Compile_rest_export(fmin, pdr);
    sysPROCEDURE(false, fmin);
    lispCHECK_NARGS(fmin, true);
    compile_rest(fmin, false);
    sysCALLQ(pdr);
    sysENDPROCEDURE()
enddefine;


define lconstant Compile_NV_export(fmin, V, pdr);
    sysPROCEDURE(false, fmin);
    lispCHECK_NARGS(fmin, true);
    lispNARGS();
    if V then
        unless fmin == 0 do
            sysPUSHQ(fmin);
            sysCALLQ(nonop fi_-)
        endunless;
        sysCALLQ(consvector)
    endif;
    sysCALLQ(pdr);
    sysENDPROCEDURE()
enddefine;


define lisp_export(pdr, sym, flag);
    lvars fni, fmin, fmax, fresults, updr, usym, ufni;
    dlocal constant_functions = true;

    unless issymbol(sym) do
        string_to_sym(sym) -> sym
    endunless;

    if flag == "special" then
        pdr -> special_form_p(sym);
        return
    endif;

    new_function_info(sym) -> fni;

    if flag == "boolean" then
        goto ZERO
    else
        go_on destlist(flag) + 1 to ZERO ONE TWO THREE;
ZERO:
        pdnargs(pdr);
ONE:
        pdnargs(pdr);
TWO:
        1;
THREE:
    endif -> (fmin, fmax, fresults);

    if fresults == "?" then
        false -> fresults
    endif;

    if (updater(pdr) ->> updr) then
        sysintern('UPDATE-' <> symbol_string(sym), system_package) -> usym;
        new_function_info(usym) -> ufni
    endif;

    if flag == "boolean" then
        fni -> function_info(pdr);
        lisp_true_apply(% pdr %) -> pdr
    endif;

    if fmax == fmin then
        if updr then
            ufni -> function_info(updr);
            check_fmin_apply(% updr, fmin + 1 %)
        else
            false
        endif;
        fni -> function_info(pdr);
        check_fmin_apply(% pdr, fmin %)
    elseif fmax == "?" then
        false -> fmax;
        updr and Compile_rest_export(fmin + 1, updr);
        Compile_rest_export(fmin, pdr)
    elseif fmax == "N" then
        false -> fmax;
        updr and Compile_NV_export(fmin + 1, false, updr);
        Compile_NV_export(fmin, false, pdr)
    elseif fmax == "V" then
        false -> fmax;
        updr and Compile_NV_export(fmin + 1, true, updr);
        Compile_NV_export(fmin, true, pdr)
    else
        updr and Compile_optional_export(fmin + 1, fmax + 1, updr);
        Compile_optional_export(fmin, fmax, pdr)
    endif -> pdr -> updr;

    if updr then
        fill_function_info(ufni, fmin + 1, fmax and fmax + 1, 0);
        ufni -> function_info(updr);
        updr -> symbol_function(usym);
        sf_token(usym) -> setf_method(sym);
    endif;

    fill_function_info(fni, fmin, fmax, fresults);
    fni -> function_info(pdr);
    pdr -> symbol_function(sym);

enddefine;


lblock;
    lvars exports, line, sym, pdr, info;
    if (syssearchpath(lispsrclist, 'exports') ->> exports) then
        recursive_front(exports) -> exports;
        nprintf('Exporting from %S', [^exports]);
        for line from_repeater line_repeater(exports, inits(255)) do
            [% pop11_compile(stringin(line)) %] -> line;
            nextif(null(line));         ;;; comment or blank
            dl(line) -> (sym, pdr, info);
            if pdr == "LISP" then
                ;;; Defined in Lisp - ignore function_info spec
            else
                lisp_export(pdr, sym, info)
            endif
        endfor
    endif
endlblock;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  3 1995
        Added V option.
--- John Williams, Apr 26 1994
        Changes for function names.
--- John Williams, Aug 27 1993
        Compile_N_export now calls lispCHECK_NARGS.
--- John Williams, Jul 13 1993
        lisp_export now sets constant_functions true.
--- John Williams, Jul 12 1993
        No longer uses popdefineconstant.
--- John Williams, Jan 25 1993
        Now uses new %S option to printf.
--- John Gibson, Jan 21 1993
        Changed to use pop11_compile.
 */
