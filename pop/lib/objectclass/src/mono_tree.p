/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/mono_tree.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */

section $-objectclass;

constant procedure full_method;

define is_simple_tree( T ); lvars T;
    T.nextTree == [] and
    not( T.defaultTree ) and
    T.actionTree
enddefine;

/*
define is_proc_tree( T ); lvars T;
    lvars a = T.is_simple_tree;
    a and a.isprocedure and a
enddefine;
*/

define is_mono_tree( T ); lvars T;
    if T then
        if T.nextTree == [] then
            lvars ( assn, def, n ) = is_mono_tree( T.defaultTree );
            if def then
                ( assn, def, n + 1 )
            elseif assn and T.actionTree.isprocedure then
                ( assn, T.actionTree, n + 1 )
            else
                ( false, false, false )
            endif
        else
            if T.defaultTree then
                ;;; There is a default that DID NOT simplify to an
                ;;; action.  This is highly unsuitable.
                ( false, false, false )
            else
                lvars answer = true;

                ;;; Efficiency is a fair issue here.  So we dump the
                ;;; results on the stack as we find them.  By keeping
                ;;; a count we can clean up afterwards if it turns out
                ;;; not to be a mono-method tree.

                lvars n =
                    #|
                        lvars b;
                        for b in T.nextTree do
                            lvars ts = treesBranch( b );
                            lvars a = ts.exactly_one and ts.hd.is_simple_tree;
                            if a then
                                keysBranch( b ), a
                            else
                                false -> answer;
                                quitloop;
                            endif
                        endfor
                    |#;

                if answer then
                    lvars L = [];
                    repeat n div 2 times
                        lvars ( ks, a ) = ();
                        lvars k;
                        for k in ks do
                            [^k ^a] :: L -> L
                        endfor
                    endrepeat;
                    ( L, T.actionTree, 1 )
                else
                    erasenum( n );
                    ( false, false, false )
                endif
            endif;
        endif;
    else
        ( false, false, false )
    endif
enddefine;

define plant_mono( G, mode, assn, def, level ); lvars G, mode, assn, def, level;

    define lconstant newtable();
        newproperty( [], 16, false, "perm" )
    enddefine;

    define lconstant cached_call_next_method(mode);
        sysPROCEDURE(false, 0);
            sysPUSHQ(full_method(G)), sysPUSHQ(newtable()), sysPUSHQ(level);
            apply_mode(sysCALL, mode)("ident cached_dispatch_call_next_method");
        sysENDPROCEDURE();
    enddefine;

    define lconstant convert( k, a, G ); lvars k, a;
        if a.isxslot then
            lvars ( mode, posn, unchanged, p ) = slot_proc_for_key( k, a, G );
            p
        elseif a == c_invoke_call_next_method then
            cached_call_next_method( CALL_MODE );
        elseif a == u_invoke_call_next_method then
            cached_call_next_method( UCALL_MODE );
        else
            a
        endif;
    enddefine;

    lvars table = newproperty( [], assn.length, def, "perm" );
    lvars p;
    for p in assn do
        lvars ( k, a ) = p.explode;
        convert( k, a, G ) -> table( k )
    endfor;
    convert( false, def, G ) -> def;
    push_arg( level );
    sysCALLQ( table );
    if def then
        plant_callq( false )
    else
        lvars v = sysNEW_LVAR();
        sysPOP( v );
        sysPUSH( v );
        sysIFNOT( "failed" );
        sysPUSH( v );
        plant_callq( false );
        sysGOTO( "return" );
        sysLABEL( "failed" );
        plant_method_failure( mode );
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct  4 1995
        Popc changes
 */
