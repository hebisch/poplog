/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/catch.p
 > Purpose:         Common Lisp CATCH and THROW
 > Author:          John Williams, Dec 13 1986 (see revisions)
 > Documentation:   CLtL, ch 7.11
 > Related Files:   C.all/src/lispcore.p
 */

lisp_compile_mode;

section $-lisp;


define Catch(forms, nresults);
    lvars tag;
    if endp(forms) then
        program_error('Missing tag in CATCH form', [])
    endif;
    fast_destpair(forms) -> (tag, forms);
    lispSET_STKLEN(nresults, false) -> nresults;
    compile_form(tag, 1);
    sysPUSHQ_DISCOUNT_LEX(
        (sysPROCEDURE(false, 0);
         Progn(forms, false);
         sysENDPROCEDURE()
        ));
    sysCALLQ(catch);
    /* throw comes out here */
    lispRESET_STKLEN(nresults)
enddefine;


define Throw(forms, nresults);
    lvars tag;
    unless destlist(forms) == 2 do
        program_error('THROW expects two arguments (a tag and a result form)', [])
    endunless;
    -> (tag, forms);
    compile_form(tag, 1);
    if (atom(forms) and not(symbol_macro(forms)))
    or forms starts_with @QUOTE then
        compile_form(forms, 1);
        sysPUSHQ(false)
    else
        sysPUSHQ_DISCOUNT_LEX(
            (sysPROCEDURE(false, 0);
             compile_form(forms, false);
             sysENDPROCEDURE()
            ));
        sysPUSHQ(true)
    endif;
    sysCALLQ(throw)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  6 1995
        XPUSHQ renamed sysPUSHQ_DISCOUNT_LEX.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Jul 28 1994
        Fixed throw for symbol macros.
 */
