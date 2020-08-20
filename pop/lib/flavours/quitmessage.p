/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/lib/flavours/quitmessage.p
 > Purpose:        Quit from a message but restore dynamic environment etc.
 > Author:         Mark Rubinstein, Jun 10 1986 (see revisions)
 > Documentation:  HELP * FLAVOUR_LIBRARY /quitmessage
 > Related Files:
 */

section $-flavour => quitmessage;

compile_mode:pop11 +defcon;

define global syntax quitmessage;
lvars
        in_message_label = sysNEW_LABEL(),
        not_updating     = sysNEW_LABEL(),
    ;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;

    sysPUSH("self");
    sysOR(in_message_label);
        sysPUSHQ(0);
        sysPUSHQ('QUITMESSAGE EXECUTED OUTSIDE OF A MESSAGE');
        sysCALLQ(mishap);
    sysLABEL(in_message_label);
    sysCALLQ(i_frec);
    sysCALLQ(f_runmethod);
    sysPUSHS(false);
    sysCALL("iscaller");
    sysIFSO(not_updating);  ;;; If the method pdr isn't in the call stack
        sysCALL("updater"); ;;; then we must be in the updater
    sysLABEL(not_updating);

    exitfrom -> pop_expr_item; sysCALLQ -> pop_expr_inst;
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Sep 21 1990
        Fixed jamesg.10
--- John Gibson, Aug 13 1989
        Replaced sys- compiler procedure with pop11_FLUSHED
 */
