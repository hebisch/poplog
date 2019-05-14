/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/subscr_loop.p
 > Purpose:
 > Author:          Ian Rogers, Nov  9 1988 (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:   LIB * IN_VECTOR
 */
compile_mode:pop11 +strict;

define subscr_loop(varlist, isfast, index, accpdr, checkpdr);
    lvars   varlist isfast index procedure(accpdr, checkpdr) ;;; args
            start fin                   ;;; labels
            datavars var dat len        ;;; misc
            first_one = true,           ;;; misc
        ;
    dlocal pop_new_lvar_list;

    unless index then
        sysNEW_LVAR() -> index;
    endunless;

    sysNEW_LVAR() -> len;
    [% for var in varlist do sysNEW_LVAR() endfor %] -> datavars;
    pop11_loop_start(sysNEW_LABEL() ->> start);
    pop11_loop_end(sysNEW_LABEL() ->> fin);

    pop11_comp_stmnt_seq_to([do then]) -> ;
    ;;; get data keeping a note of the shortest.
    for dat in datavars do
        sysPOP(dat);
        unless isfast do
            sysPUSH(dat);
            sysCALLQ(checkpdr);
        endunless;
        if not(isfast) or first_one then
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
    endfor;
    ;;; as we just pulled the data off backwards...
    ncrev(datavars) -> datavars;

    sysPUSHQ(0);
    sysPOP(index);

    sysLABEL(start);

    ;;; increment the index
    sysPUSH(index);
    sysPUSHQ(1);
    sysCALL("fi_+");
    sysPOP(index);

    ;;; check that we have some data left
    sysPUSH(index);
    sysPUSH(len);
    sysCALL("fi_>");
    sysIFSO(fin);

    ;;; set user variables with data.
    for var,dat in varlist,datavars do
        sysPUSH(index);
        sysPUSH(dat);
        sysCALLQ(accpdr);
        sysPOP(var);
    endfor;

    ;;; do the loop body
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    ;;; and loop again
    sysGOTO(start);

    sysLABEL(fin);
enddefine;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 19 1990
        Made -pop_new_lvar_list- dlocal
--- Ian Rogers, Mar 26 1990
    Added hook for -with_index-
--- Ian Rogers, Aug 31 1989
    fixed -nextloop- bug (cf. jamesg.4)
--- Ian Rogers, Jul 17 1989 - Changed repeated calls to -pop11_comp_expr- to
    -pop11_comp_stmnt_seq_to-.

 */
