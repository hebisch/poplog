/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/database/foreach.p
 >  Purpose:        performs action on all items in database matching pattern.
 >  Author:         A.Sloman S.Hardy 1982 (see revisions)
 >  Documentation:  HELP * FOREACH
 >  Related Files:  LIB * FOREVERY, * DATABASE
 */

section ;

;;;  TRYNEXT takes a possibilities list as argument
;;;  It finds the next matching element and returns it, updating
;;;  the list. If none is found it returns FALSE.

define lconstant procedure trynext(PL);
    lvars P, L, PL;
    fast_front(PL) -> P;
    fast_back(PL) -> L;
    until null(L) do
        if fast_front(L) matches P then
            fast_front(L) -> it;
            fast_back(L) -> fast_back(PL);
            return(true)
        endif;
        fast_back(L) -> L;
    enduntil;
    return(false);
enddefine;

;;;  FOREACH pattern DO actions ENDFOREACH
;;;
;;;      VARS %X;
;;;      FETCH(pattern) -> %X;
;;;      WHILE TRYNEXT(%X) DO actions ENDWHILE
;;;
global vars syntax endforeach;


lconstant _temp=popconstruct;
true -> popconstruct;       ;;; make lists compile as constants


define global syntax foreach;
    lvars Var Lab Endlab _x;
    dlocal pop_new_lvar_list;
    sysNEW_LVAR() -> Var;
    sysNEW_LABEL() -> Lab;  pop11_loop_start(Lab);
    sysNEW_LABEL() -> Endlab;   pop11_loop_end(Endlab);
    pop11_comp_expr_to([do then in]) -> _x;
    if _x == "in" then
        erase(pop11_comp_expr_to([do then]));
        sysCALLQ(nonop ::)
    else
        sysPUSH("database");
        sysCALLQ(nonop ::)
    endif;
    sysPOP(Var);
    sysLABEL(Lab);
    sysPUSH(Var);
    sysCALLQ(trynext);
    sysIFNOT(Endlab);
    erase(pop11_comp_stmnt_seq_to([endforeach {close}]));
    sysGOTO(Lab);
    sysLABEL(Endlab);
enddefine;

_temp -> popconstruct;

endsection;

/* --- Revision History ---------------------------------------------------
--- Poplog Documentor, Dec 16 1987 (Ian Rogers)
    updated compiler proc names.
--- Aaron Sloman, Nov  7 1986 lvarsed, desectionised.
*/
