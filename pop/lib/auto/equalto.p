/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/equalto.p
 > Purpose:         Iterate over list items equal to pattern
 > Author:          John Gibson, Dec 28 1995
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * allequalto
 */
compile_mode :pop11 +strict;

section;

;;;  for var equalto pattern [in list] do actions endfor
;;;
;;; where list defaults to database.


define :for_extension equalto(varlist, isfast);
    lvars Patt, List, Lab, Endlab, Item, x;
    dlocal pop_new_lvar_list;
    if listlength(varlist) /== 1 then
        mishap('for ... equalto NOT PERMITTED WITH MULTIPLE VARIABLES',varlist)
    endif;
    hd(varlist) -> Item;
    sysNEW_LVAR() -> Patt;
    sysNEW_LVAR() -> List;
    pop11_loop_start(sysNEW_LABEL() ->> Lab);
    pop11_loop_end(sysNEW_LABEL() ->> Endlab);
    (pop11_comp_expr_to([do then in]) -> x) -> sysPUSH(Patt);
    if x == "in" then
        pop11_comp_expr_to([do then]) ->
    else
        sysPUSH("database")
    endif -> sysPUSH(List);

    sysLABEL(Lab);
        sysPUSH(List);
        if isfast then
            sysCALL(sysPUSH("nil"), "==")
        else
            sysCALL("null")
        endif;
        sysIFSO(Endlab);

        sysCALL(sysPUSH(List), "fast_destpair")
                                -> (sysPUSH(Item), sysPUSH(List));
        sysCALL(sysPUSH(Item), sysPUSH(Patt), "="), sysIFNOT(Lab);
        pop11_comp_stmnt_seq_to(popclosebracket) -> ;
        sysGOTO(Lab);

    sysLABEL(Endlab);
enddefine;

endsection;
