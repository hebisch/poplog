/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/class_subscr_loop.p
 > Purpose:
 > Author:          Ian Rogers, Nov  9 1988 (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * IN_VECTOR * IN_SUBSCRIPTED
 */
compile_mode:pop11 +strict;

define global class_subscr_loop(varlist, isfast, index, class_pdr);
lvars   var dat scr varlist datavars scrpdrs
        len index isfast class_pdr
        start fin procedure(checkpdr)
        first_one = true,
    ;
    unless index then
        sysNEW_LVAR() -> index;
    endunless;
    sysNEW_LVAR() -> len;
    [% for var in varlist do sysNEW_LVAR() endfor %] -> datavars;
    [% for var in varlist do sysNEW_LVAR() endfor %] -> scrpdrs;
    pop11_loop_start(sysNEW_LABEL() ->> start);
    pop11_loop_end(sysNEW_LABEL() ->> fin);

    ;;; get data keeping a note of the shortest.
    pop11_comp_stmnt_seq_to([do then]) -> ;

    for dat,scr in datavars,scrpdrs do
        sysPOP(dat);
        sysPUSH(dat);
        sysCALL("datakey");
        sysCALLQ(class_pdr);
        sysPOP(scr);
        if first_one or not(isfast) then
            sysPUSH(dat);
            sysCALL("length");
            if first_one then
                false -> first_one
            else
                sysPUSH(len);
                sysCALL("min");
            endif;
            sysPOP(len);
        endif;
        pop11_try_nextitem(",") -> ;
    endfor;
    ;;; as we just pulled the data off backwards...
    ncrev(varlist) -> varlist;


    sysPUSHQ(0);
    sysPOP(index);

    sysLABEL(start);

    ;;; increment index
    sysPUSH(index);
    sysPUSHQ(1);
    sysCALLQ(nonop fi_+);
    sysPOP(index);

    ;;; check that we have some data
    sysPUSH(index);
    sysPUSH(len);
    sysCALLQ(nonop fi_>);
    sysIFSO(fin);

    ;;; set user variables with data.
    for var,dat,scr in varlist,datavars,scrpdrs do
        sysPUSH(index);
        sysPUSH(dat);
        sysCALL(scr);
        sysPOP(var);
    endfor;

    ;;; do the loop body
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    ;;; and loop again
    sysGOTO(start);

    sysLABEL(fin);
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  5 1992
        Made -class_subscr_loop- global
--- Ian Rogers, Mar 26 1990
        Added -with_index- hook.
 */
