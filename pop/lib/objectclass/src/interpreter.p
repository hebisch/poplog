/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/interpreter.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */

section $-objectclass;

#_IF DEF OBJECTCLASS_IN_POPC

;;; Use a compiling version of the ``interpreter'' so that Popc doesn't
;;; have to generate tree structures for run-time execution

constant procedure comp_tree;

define lconstant comp_next(L, level, call);

    ;;; This check ensures we don't POP from an empty stack!
    returnif( L == [] );

    dlocal pop_new_lvar_list;
    lvars k = sysNEW_LVAR();
    sysPUSHQ(level), sysCALL("subscr_stack"), sysCALL("datakey"), sysPOP(k);
    lvars b, t, lab;
    for b in L do
        lvars keys = keysBranch(b);
        sysPUSH(k);
        if back(keys) == [] then
            sysPUSHQ(front(keys)), sysCALL("==");
        else
            sysPUSHQ(copylist(keys)), sysCALL("fast_lmember");
        endif;
        sysIFNOT(sysNEW_LABEL() ->> lab);
        for t in treesBranch(b) do
            comp_tree(t, level, call)
        endfor;
        sysLABEL(lab);
    endfor;
enddefine;

define comp_tree(T, level, call);
    level + 1 -> level;
    lvars d = T.defaultTree;
    lvars a = T.actionTree;
    lvars n = T.nextTree;
    comp_next(n, level, call);
    if d then
        comp_tree(d, level, call);
    endif;
    if a then
        sysPUSHQ(a), sysCALL(call);
    endif;
enddefine;

define relink_interpreter(Interpreter, Generic);
    lvars MT = Generic.method_table;

    lockMethodTable( MT );

    lvars centries = cEntriesMethodTable( MT );
    lvars uentries = uEntriesMethodTable( MT );

    lvars need_updater = not( null( uentries ) );

    lvars tree  = net_to_tree( centries );
    lvars utree = need_updater and net_to_tree( uentries );

    unlockMethodTable( MT );

    lvars carity = cArityMethodTable( MT );
    lvars uarity = need_updater and uArityMethodTable( MT );

    if tree then
        instantiate( tree, false )
    endif;
    if need_updater and utree then
        instantiate( utree, false )
    endif;

    define Comp(T, mode);
        sysPROCEDURE(false, 0);
            if T then comp_tree(T, 0, "ident call_method_part") endif;
            apply_mode(sysCALL, mode)("ident call_method_failure");
        sysENDPROCEDURE();
    enddefine;

    lvars proc = Comp(tree, CALL_MODE);
    set_pdnargs_mode( carity, proc, CALL_MODE );
    if need_updater then
        Comp(utree, UCALL_MODE) -> updater(proc);
        set_pdnargs_mode( uarity, proc, UCALL_MODE );
    endif;

    proc -> Interpreter.indirect_pdpart;
    need_updater and updater(proc) -> Interpreter.indirect_updater;
enddefine;

define unlink_interpreter(Int, Gen);
    procedure();
        relink_interpreter(Int, Gen);
        chain(Int);
    endprocedure -> Int.indirect_pdpart;
    procedure();
        relink_interpreter(Int, Gen);
        if Int.updater then
            chain(Int);
        else
            () -> Int();      ;;; Generate the right error message.
        endif
    endprocedure -> Int.indirect_updater;
enddefine;

define new_interpreter(Gen) -> Int;
    newindirect(identfn) -> Int;
    unlink_interpreter(Int, Gen);
enddefine;

#_ELSE

;;; -- Executing a Tree -----------------------------------------------------

;;; It is important that using this dosn't build up lots of unneeded
;;; activation records.  As a consequence, we chain whenever possible.
;;; This won't save much, though.

constant procedure run_tree;

define run_next( L, level, call ); lvars level, L, call;

    ;;; This check ensures we don't POP from an empty stack!
    returnif( L == [] );

    lvars k = datakey( subscr_stack( level ) );
    repeat
        lvars b = destpair( L ) -> L;           ;;; fast_destpair OK.
        if lmember( k, keysBranch( b ) ) then   ;;; fast_lmember OK.
            lvars TS = treesBranch( b );
            until TS == [] do
                lvars t = TS.destpair -> TS;    ;;; fast_destpair OK.
                if L == [] and TS == [] then
                    fast_chain( t, level, call, run_tree )
                else
                    run_tree( t, level, call )
                endif
            enduntil;
        endif;
        quitif( L == [] );
    endrepeat;
enddefine;

define run_tree( T, level, call ); lvars T, level, call;
    level + 1 -> level;                         ;;; fi_+ OK
    lvars d = T.defaultTree;
    lvars a = T.actionTree;
    lvars n = T.nextTree;

    if a then
        run_next( n, level, call );
        if d then
            run_tree( d, level, call )
        endif;
        chain( a, call )                        ;;; fast_chain OK.
    elseif d then
        run_next( n, level, call );
        if d then
            fast_chain( d, level, call, run_tree )
        endif
    else
        fast_chain( n, level, call, run_next )
    endif;
enddefine;


;;; -- Creating, Linking and Unlinking the interpreter ----------------------

define c_interpret( T ); lvars T;
    if T then run_tree( T, 0, call_method_part ) endif;
    call_method_failure();
enddefine;

define u_interpret( T ); lvars T;
    if T then run_tree( T, 0, call_method_part ) endif;
    () -> call_method_failure();
enddefine;

define relink_interpreter( Interpreter, Generic ); lvars Generic, Interpreter;
    lvars MT = Generic.method_table;

    lockMethodTable( MT );

    lvars centries = cEntriesMethodTable( MT );
    lvars uentries = uEntriesMethodTable( MT );

    lvars need_updater = not( null( uentries ) );

    lvars tree  = net_to_tree( centries );
    lvars utree = need_updater and net_to_tree( uentries );

    unlockMethodTable( MT );

    lvars carity = cArityMethodTable( MT );
    lvars uarity = need_updater and uArityMethodTable( MT );

    if tree then
        instantiate( tree, false )
    endif;
    if need_updater and utree then
        instantiate( utree, false )
    endif;

    c_interpret -> Interpreter.indirect_pdpart;
    tree -> indirect_frozval( 1, Interpreter );
    set_pdnargs_mode( carity, Interpreter, CALL_MODE );

    if need_updater then
        u_interpret(% utree %) -> Interpreter.updater;
        set_pdnargs_mode( uarity, Interpreter, UCALL_MODE );
    else
        false -> Interpreter.updater
    endif;
enddefine;

define unlink_interpreter( Int, Gen ); lvars Int, Gen;
    lvars IG = conspair( Int, Gen );
    IG -> indirect_frozval( 1, Int );
    procedure( IntGen ); lvars IntGen;
        relink_interpreter( IntGen.destpair );
        chain( IntGen.front );
    endprocedure -> Int.indirect_pdpart;
    procedure( IntGen ); lvars IntGen;
        relink_interpreter( IntGen.destpair );
        lvars i = IntGen.front;
        if i.updater then
            chain( i );
        else
            () -> i();      ;;; Generate the right error message.
        endif
    endprocedure(% IG %) -> Int.indirect_updater;
enddefine;

define new_interpreter( Gen ) -> Int; lvars Int, Gen;
    newindirect_n( identfn, undef, 1 ) -> Int;
    unlink_interpreter( Int, Gen );
enddefine;

#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct  4 1995
        Added a compiling version of the interpreter for use by Popc
--- Steve Leach, 7th March 2003
        Now uses set_pdnargs_mode to improve the pdnarg values that
        are set on methods.
 */
