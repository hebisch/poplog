/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/in_property.p
 > Purpose:         (fast_)for <key>,<val> in_property <prop> do ... endfor
 > Author:          Ian Rogers, Nov 10 1988 (see revisions)
 > Documentation:   HELP * FOR_FORM/in_property
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

define lconstant fast_propit(varlist, is_in);
    lvars   varlist, is_in, key, val, start, fin, state, do_entry;
    dlocal  pop_new_lvar_list;

    pop11_loop_start(sysNEW_LABEL() ->> start);
    pop11_loop_end(sysNEW_LABEL() ->> fin);
    sysNEW_LABEL() -> do_entry;
    sysNEW_LVAR() -> state;

    /* Get users variables */
    if is_in then
        unless length(varlist) == 2 then
            mishap('Need two variables for in_property', varlist);
        endunless;
        varlist(2) -> val;
    else
        unless length(varlist) == 1 then
            mishap('Need one variable for on_property', varlist);
        endunless;
    endif;
    hd(varlist) -> key;

    /* Get 'state' for property (a pair) */
    pop11_comp_expr_to([do then]) -> ;
    sysCALL("Prop_entry_state_init");
    sysPOP(state);
    sysGOTO(start);

    /* entry on stack, so update users variables */
    sysLABEL(do_entry);
    if is_in then
        sysCALL("fast_destpair");
        sysPOP(val);
        sysPOP(key);
    else
        sysPOP(key);
    endif;

    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    sysLABEL(start);
    sysPUSH(state);
    sysCALL("Prop_entry_state_next");       ;;; returns next entry or false
    sysOR(do_entry);                        ;;; loop for more else erase false

    sysLABEL(fin);
    sysPUSH(state);
    sysCALL("sys_grbg_list")
enddefine;

define lconstant propit(varlist, is_in);
lvars   varlist is_in
        key val list start fin prop
    ;
    dlocal pop_new_lvar_list;

    unless is_in then
        if pop_vm_flags &&/=_0 VM_NO_FAST then
            ;;; assume we've got the slow version because of VM_NO_FAST
            chain(varlist, is_in, fast_propit)
        else
            mishap("'on_property not allowed with slow for'", varlist)
        endif
    endunless;

    pop11_loop_start(sysNEW_LABEL() ->> start);
    pop11_loop_end(sysNEW_LABEL() ->> fin);
    sysNEW_LVAR() -> list;
    sysNEW_LVAR() -> prop;

    /* Get users variables */
    unless length(varlist) == 2 then
        mishap('Need two variables for a property iteration', varlist);
    endunless;
    varlist(1) -> key;
    varlist(2) -> val;

    /* Get property and convert its contents into a list*/
    pop11_comp_expr_to([do then]) -> ;
    sysPOP(prop);   ;;; need to do this incase the above expression leaves, or
                    ;;; depends on, some mess on the stack

    sysCALL("stacklength");
    sysPOP(list);

    sysPUSH(prop);
    sysPUSHQ(identfn);
    sysCALL("fast_appproperty");

    sysPUSH(list);
    sysCALL(sysPUSH("conslist"), "sysanyvecons");
    sysPOP(list);

    sysLABEL(start);

    /* Finish loop if no values left */
    sysPUSH(list);
    sysPUSHQ([]);
    sysCALL("==");
    sysIFSO(fin);

    /* Otherwise, set users variables */
    sysPUSH(list);
    sysCALL("sys_grbg_destpair");
    sysCALL("sys_grbg_destpair");
    sysPOP(list);
    sysPOP(val);
    sysPOP(key);

    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    sysGOTO(start);
    sysLABEL(fin);

    /* Reclaim anything that may be left of the value list */
    sysPUSH(list);
    sysCALL("sys_grbg_list");
enddefine;

define :for_extension global in_property with_nargs 2;
    if () then fast_propit(true) else propit(true) endif;
enddefine;

define :for_extension global on_property with_nargs 2;
    if () then fast_propit(false) else propit(false) endif;
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Added VM_NO_FAST test in propit.
--- John Gibson, Nov 17 1992
        Added call to sys_grbg_list at the end of fast_propit
--- Ian Rogers, Dec 18 1989
        Added call to -sys_grbg_list- at the end of -propit-
--- John Gibson, May 25 1989
        Rewrote -fast_propit- to use -Prop_entry_state_init- and
        -Prop_entry_state_next- (-property_vector- withdrawn).
 */
