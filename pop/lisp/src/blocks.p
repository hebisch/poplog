/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/blocks.p
 > Purpose:         Common Lisp BLOCK construct
 > Author:          John Williams, Feb 25 1986 (see revisions)
 > Documentation:   CLtL, p119-120
 */

lisp_compile_mode;

section $-lisp;

/* Information about blocks is kept in the association list Blocks
    which maps block names (symbols) to blkinfo records (3-element
    vectors). Blocks is initialied in src/plant.p.
*/


define lispBLOCK(name, nresults);
    lvars stkinfo;
    lispSET_STKLEN(0, false) -> stkinfo;
    acons(name,
            {% symbol_name(name), stkinfo, nresults %},
            Blocks) -> Blocks
enddefine;


define lispENDBLOCK();
    lvars blkinfo, b = Blocks;
    unless listlength_>=(Blocks, 2)
    and issymbol(fast_destpair(Blocks) -> Blocks)
    and isvector(fast_destpair(Blocks) -> Blocks ->> blkinfo)
    do
        lisp_error('System error: lispENDBLOCK called out of context', [^b])
    endunless;
    sysLABEL(blkinfo(1))
enddefine;


define Return(forms, nresults);
    lvars name, l, lab, stkinfo;
    dlocal Blocks;

    checkr_name_from_list(forms, @BLOCK) -> name;
    fast_back(forms) -> forms;

    if (lmember(name, Blocks) ->> l) then
        l -> Blocks;            ;;; make intervening blocks inaccessible
        destvector(cadr(l)) -> (lab, stkinfo, nresults, /* 3 */)
    else
        program_error('Cannot RETURN-FROM non-enclosing block ~S', [^name])
    endif;

    lispRESET_STKLEN_IF_NEEDED(stkinfo);
    if ispair(forms) then
        unless fast_back(forms) == nil do
            program_error(
                'Too many result forms in RETURN-FROM expression', [])
        endunless;
        compile_form(fast_front(forms), nresults)
    else
        lispPUSHNQ(nil, nresults)
    endif;
    sysGOTO(lab)
enddefine;


define lconstant Compile_block(name, forms, nresults);
    dlocal Blocks;
    lispBLOCK(name, nresults);
    Progn(forms, nresults);
    lispENDBLOCK()
enddefine;


define Block(forms, nresults);
    Compile_block(checkr_name_from_list(forms, @BLOCK),
                    fast_back(forms),
                    nresults)
enddefine;


define compile_body(name, forms, nresults);
    if name then
        Compile_block(name, forms, nresults)
    else
        Progn(forms, nresults)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Jul 26 1994
        Return disestablishes blocks intervening between the call to
        RETURN-FROM and the target block (cf. Steele 1990 189-92).
--- John Williams, Jul 11 1994
        Tidied up. lispENDBLOCK no longer takes an argument.
 */
