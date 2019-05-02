/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/progv.p
 > Purpose:         Common Lisp PROGV and UNWIND-PROTECT special-forms
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:   CLtl, p112-113
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

fastprocs destpair;

vars Progv_save = [];


define Progv(forms, nresults);
    lvars nresults, syms, vals;

    if endp(forms)
    or (destpair(forms) -> (syms, forms), endp(forms)) then
        program_error('Malformed PROGV construct', [])
    endif;
    destpair(forms) -> (vals, forms);

    define lconstant Progv_bind(syms, vals);
        lvars sym, id;
        for sym in_cl_list syms do
            identof(sv_token(sym)) -> id;
            acons(idval(id), id, Progv_save) -> Progv_save;
            if endp(vals) then
                consundef(sym)
            else
                destpair(vals) -> vals
            endif -> idval(id)
        endfor;
        consvector(destlist(syms)) -> f_specials(caller(1))
    enddefine;

    define lconstant Progv_unbind() with_props 0;
        sysPUSH("dlocal_context");
        sysCALLQ(
            procedure() with_nargs 1;
                if (/* context */) == 1 then
                    [] -> Progv_save
                endif
            endprocedure)
    enddefine;

    define updaterof lconstant Progv_unbind() with_props 0;
        sysPUSH("dlocal_context");
        sysCALLQ(
            procedure() with_nargs 1;
                if (/* context */) <= 2 then
                    until Progv_save == [] do
                        (destpair(Progv_save) -> Progv_save)
                            -> idval(destpair(Progv_save) -> Progv_save)
                    enduntil
                endif
            endprocedure)
    enddefine;

    compile_args([^syms ^vals]) ->;

    sysPROCEDURE('Progv', 2);
    sysLOCAL("ident Progv_save");
    sysLOCAL(Progv_unbind);
    sysCALLQ(Progv_bind);
    Progn(forms, nresults);
    sysENDPROCEDURE();
    sysCALLQ()
enddefine;


/* UNWIND-PROTECT */

define Unwind_protect(forms, nresults);
    lvars form;
    dlvars forms;
    dlocal Tail_recursion = 0;

    unless ispair(forms) do
        program_error('UNWIND-PROTECT expects at least one argument', [])
    endunless;
    destpair(forms) -> (form, forms);

    define lconstant Uw() with_props 1;
        sysPUSH("pop_enable_interrupts")
    enddefine;

    define updaterof lconstant Uw() with_props 1;
        lvars lab;
        sysPOP("pop_enable_interrupts");
        sysPUSH("dlocal_context");
        sysPUSHQ(2);
        sysCALLQ(nonop fi_>);
        sysIFSO(sysNEW_LABEL() ->> lab);
        Prog0(forms);                       ;;; clean-up forms
        sysLABEL(lab)
    enddefine;

    sysPROCEDURE('Unwind-protect', 0);
    compile_form(form, nresults);           ;;; protected form
    sysLOCAL(Uw);
    sysENDPROCEDURE();
    sysCALLQ()
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Jan 19 1995
        Unwind_protect no longer executes "clean-up forms" on process
        suspend/resume.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
 */
