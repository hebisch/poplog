/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/from_repeater.p
 > Purpose:
 > Author:          Ian Rogers, Dec 12 1988
 > Documentation:
 > Related Files:
 */

section;

define :for_extension global from_repeater(varlist, isfast);
lvars   varlist isfast var pdr
        start fin
    ;

    if back(varlist) /== [] then
        mishap('Only one data variable for this construct', varlist);
    endif;
    hd(varlist) -> var;

    pop11_loop_start(sysNEW_LABEL() ->> start);
    pop11_loop_end(sysNEW_LABEL() ->> fin);

    itemread() -> pdr;

    unless pop11_try_nextitem([do then]) then
        ;;; a full expression instead of just the name
        ;;; of the repeater
        pdr :: proglist -> proglist;
        sysNEW_LVAR() -> pdr;
        pop11_comp_expr_to([do then]) -> ;
        sysPOP(pdr)
    endunless;

    sysLABEL(start);

    ;;; fetch the next item
    sysCALL(pdr);
    sysPOP(var);
    sysPUSH(var);
    sysPUSHQ(termin);
    sysCALL("==");
    sysIFSO(fin);

    ;;; do the loop body
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    ;;; and loop again
    sysGOTO(start);

    sysLABEL(fin);
enddefine;

endsection;
