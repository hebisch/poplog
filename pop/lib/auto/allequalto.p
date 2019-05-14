/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/allequalto.p
 > Purpose:         Takes a list of patterns and tries to satisfy all
 > Author:          John Gibson, Dec 28 1995
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * equalto
 */
compile_mode :pop11 +strict;

;;; for var allequalto ... is to database_allpresent as
;;; for var equalto ... is to database_present.
;;;
;;;     for var allequalto pattern_list [in list] do actions endfor
;;;
;;; becomes, roughly:
;;;
;;;  lvars tmp = consproc(pattern_list, list, 2, db_try_all_matches);
;;;  while runproc(tmp, 0) do actions endwhile;
;;;
;;; where list defaults to database.


section;

uses-by_name $-lib$-db_try_all_matches;

define :for_extension allequalto(varlist, isfast);
    lvars Endlab, Lab, Proc, Item, x;
    dlocal pop_new_lvar_list;
    if listlength(varlist) /== 1 then
        mishap('for ... allequalto NOT PERMITTED WITH MULTIPLE VARIABLES',varlist)
    endif;
    if (hd(varlist) ->> Item) == "_" then false -> Item endif;
    pop11_loop_start(sysNEW_LABEL() ->> Lab);
    pop11_loop_end(sysNEW_LABEL() ->> Endlab);
    sysNEW_LVAR() -> Proc;
    pop11_comp_expr_to([do then in]) -> x;
    if x == "in" then
        pop11_comp_expr_to([do then]) ->
    else
        sysPUSH("database")
    endif;
    sysPUSH(if Item then "true" else "false" endif);
    sysPUSHQ(3);
    sysPUSH("ident $-lib$-db_try_all_matches");
    sysCALL("consproc") -> sysPUSH(Proc);

    sysLABEL(Lab);
        sysCALL(sysPUSHQ(0), sysPUSH(Proc), "runproc"), sysIFNOT(Endlab);
        if Item then -> sysPUSH(Item) endif;    ;;; instance left on stack
        pop11_comp_stmnt_seq_to(popclosebracket) -> ;
        sysGOTO(Lab);
    sysLABEL(Endlab)
enddefine;

endsection;
