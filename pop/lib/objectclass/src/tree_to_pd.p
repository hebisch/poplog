/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/tree_to_pd.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Tree to Procedure ----------------------------------------------------


section $-objectclass;

;;; Forward declarations.
constant procedure ( full_method );


;;; -- Utilities for planting code ------------------------------------------

define plant_call_slot_proc_for_key( key, slot, G );
    lvars specs =
        if key.iskey then
            key.class_field_spec
        else
            internal_fault( [ key ^key ] )
        endif;
    lvars ( mode, posn, unchanged, p ) = slot_proc_for_key( key, slot, G );
    if unchanged then
        apply(
            posn, specs, false, false,
            apply_mode( sysFIELD, mode )
        );
        sysGOTO( "return" )
    else
        plant_callq( p )
    endif;
enddefine;


;;; Plants code to compare the level-th argument with key.  Note that
;;; we count from the top of the stack downwards (top = 1).
;;;
define_lconstant_procedure cmp_key( key, level );
    lvars ka;
    if key.issingletonclass then
        ;;; Don't use a key, use the argument itself.
        sysSUBSCR_STACK( level );
        sysPUSHQ( key.issingletonclass_table );
    elseif Keyargs and Keyargs( level ) ->> ka then
        ;;; The key has been deemed worth cached & is cached in variable ka
        sysPUSH( ka );
        sysPUSHQ( key );
    else
        ;;; We have decided that it isn't worth allocating variables
        ;;; to cache the key of this argument.  We'll just calculate the
        ;;; keys on the fly.
        sysSUBSCR_STACK( level );
        sysCALL( "datakey" );
        sysPUSHQ( key );
    endif;
    sysCALL( "==" );
enddefine;


;;; -- Compiling a method ---------------------------------------------------

constant several_keys = 3;
constant lots_of_keys = 6;

;;; Plant a test for argument at a given level to match a keys-list.
;;; Use the key comparator to do the guts of the test.
;;;
define_lconstant_procedure Plant_test( keys, level );
    lvars L = keys.length;
    if L <= 0 then
        internal_error()
    elseif L == 1 then
        cmp_key( keys.front, level )
    elseif L <= several_keys then
        lvars done = sysNEW_LABEL();
        lvars x;
        for x on keys do
            lvars last_one = x.back == [];
            cmp_key( x.front, level );
            unless last_one do sysOR( done ) endunless;
        endfor;
        sysLABEL( done );
    elseif L <= lots_of_keys then
        push_arg( level );
        sysPUSHQ( keys );
        sysCALL( "fast_lmember" );
    else
        ;;; loadsakeys!
        lvars t = newproperty( [], L, false, "tmparg" );
        lvars k;
        for k in keys do
            true -> t( k )
        endfor;
        push_arg( level );
        sysCALLQ( t );
    endif;
enddefine;


;;; Plant code for a slot s for a set of keys.  Note that the slot might
;;; be an updater slot.
;;;
define_lconstant_procedure Plant_call_slot_proc( s, keys, G );
    lvars next = sysNEW_LABEL();
    lvars ks;
    for ks on keys do
        sysLABEL( next );
        sysNEW_LABEL() -> next;
        lvars ( k, rest ) = ks.dest;
        cmp_key( k, 1 );
        sysIFNOT( next );
        plant_call_slot_proc_for_key( k, s, G );
    endfor;
    sysLABEL( next );
enddefine;

;;; Plant code for part P in method M at a particular argument level.
;;;
define_lconstant_procedure Plant_call_part( G, P, keyslist, level );
    if P.isprocedure then
        lvars cmode;
        if P.get_invokes_call_next_method ->> cmode then
            sysPUSHQ( full_method( G ) );
            if cmode == CALL_MODE then
                sysPUSH( "ident dispatch_call_next_method" );
            else
                ;;; fast_chain doesn't have an update form,
                ;;; unfortunately, so we push the updater explicitly
                sysPUSH( "ident dispatch_ucall_next_method" );
            endif;
            sysCALL( "fast_chain" )
        else
            plant_callq( P );
        endif
    elseif P.isxslot then
        unless level == 1 do internal_fault( [ level ^level ] ) endunless;
        Plant_call_slot_proc( P, keyslist, G );
    else
        internal_fault( [ part ^P ] )
    endif
enddefine;


;;; -- Plant Tree -----------------------------------------------------------

constant procedure Plant_tree;

;;; Returns true when there is only one tree in the list of
;;; trees and it represents a slot.
define lconstant procedure Has_slot( trees ); lvars trees;
    returnif( trees.null )( false );
    returnunless( trees.fast_back.null )( false );
    lvars t = trees.fast_front;
    returnunless( t.nextTree == [] )( false );
    lvars a = t.actionTree;
    return( a.isxslot and a );
enddefine;

define lconstant procedure Default( G, tree, level, level1 ); lvars G, tree, level, level1;
    Plant_tree( G, tree.defaultTree, level1 );
    lvars act = tree.actionTree;
    if act.isprocedure then
        Plant_call_part( G, act, false, level )
    elseif act then
        internal_fault( 'Slot in default part' )
    endif;
enddefine;

define lconstant procedure count_keys( L ) -> ( NB, NK, justprocs );

    define lconstant procedure isProcTree( t ); lvars t;
        simpleTree( t ) and actionTree( t ).isprocedure and
        not( t.actionTree.get_invokes_call_next_method )
    enddefine;

    true -> justprocs;
    0 -> NB;
    0 -> NK;
    lvars b;
    for b in L do
        NB + 1 -> NB;
        length( keysBranch( b ) ) + NK -> NK;
        lvars ts = treesBranch( b );
        justprocs and ts.exactly_one and ts(1).isProcTree -> justprocs;
    endfor;
enddefine;

define lconstant procedure get_findproc( L, nk ) -> findproc;
    lvars findproc = newproperty( [], max( nk, 1 ), false, "perm" );
    lvars b, k;
    for b in L do
        for k in keysBranch( b ) do
            actionTree( treesBranch( b )( 1 ) ) -> findproc( k )
        endfor;
    endfor;
enddefine;

define lconstant procedure get_indexer( L, nk ) -> indexer;
    lvars indexer = newproperty( [], max( nk, 1 ), 0, "perm" );
    lvars n = 0;
    lvars b, k;
    for b in L do
        n + 1 -> n;
        for k in keysBranch( b ) do
            n -> indexer( k )
        endfor;
    endfor;
enddefine;

define lconstant plant_for_branches( G, b, cascading, next, level1, failed );
    lvars keys = b.keysBranch;
    if keys.null then internal_fault( 'Branch with no keys?!' ) endif;
    lvars trees = b.treesBranch;
    lvars s = trees.Has_slot;
    if s then
        Plant_call_part( G, s, keys, level1 );
    else
        if cascading then
            Plant_test( keys, level1 );
            sysIFNOT( next );
        endif;
        lvars t;
        for t in trees do
             Plant_tree( G, t, level1 );
        endfor;
        unless oneCantFail( trees ) do
            sysGOTO( failed )
        endunless;
    endif
enddefine;


;;; Plant code for a tree of a method G at a given argument level after
;;; having seen previous arguments matching the keys in keyslist.  keyslist
;;; is a list of key-lists.  Each key-list corresponds to a disjunction
;;; of key matches (or singletonclass matches).  There is a key-list for
;;; each level.
;;;
define Plant_tree( G, tree, level );
    dlocal pop_new_lvar_list;

    returnunless( tree );   ;;; Copes with false trees.
    lvars failed = sysNEW_LABEL();
    lvars level1 = level + 1;

    lvars next_trees = tree.nextTree;

    lvars ( n_branches, n_keys, justprocs ) = count_keys( next_trees );

    if n_keys > lots_of_keys and n_branches > 1 then
        ;;; Generalisation of mono-method optimisation.
        if justprocs then
            lvars findproc = get_findproc( next_trees, n_keys );
            push_arg( level1 );
            sysCALLQ( findproc );
            lvars contlab = sysNEW_LABEL();
            sysOR( contlab );
            sysGOTO( failed );
            sysLABEL( contlab );
            plant_callq( false );
        else
            lvars indexer = get_indexer( next_trees, n_keys );
            push_arg( level1 );
            sysCALLQ( indexer );
            lvars lablist = [% applynum( sysNEW_LABEL, n_branches ) %];
            sysGO_ON( lablist, failed );
            lvars lab, b;
            for lab, b in lablist, next_trees do
                sysLABEL( lab );
                plant_for_branches( G, b, false, false, level1, failed );
            endfor;
        endif
    else
        lvars b;
        for b in next_trees do
            lvars next = sysNEW_LABEL();
            plant_for_branches( G, b, true, next, level1, failed );
            sysLABEL( next );
        endfor;
    endif;
    sysLABEL( failed );

    Default( G, tree, level, level1 );
enddefine;


;;; -- Is it worthwhile allocating variables for keys? ----------------------

;;; Estimate how often a key is used.  This has to take into
;;; account that the code for generating a comparison gets smart
;;; if there are lots of keys involved.
;;;
define needKey( T );
    lvars procedure need = newproperty( [], 8, false, "perm" );

    define lconstant procedure scan( T, level ); lvars T, level;
        returnunless( T );
        lvars level1 = level + 1;
        lvars b;
        for b in nextTree( T ) do
            unless andlist( b.keysBranch, issingletonclass ) do
                lvars nk = length( b.keysBranch );
                if nk > several_keys then
                    ;;; we flip representations.
                    1 -> nk
                endif;
                ( need( level ) or 0 ) + nk -> need( level )
            endunless;
            lvars t;
            for t in treesBranch( b ) do
                scan( t, level1 )
            endfor;
        endfor;
        scan( T.defaultTree, level1 );
    enddefine;

    scan( T, 1 );               ;;; estimate how many time each arg is used
    return( need );
enddefine;

define_lconstant_procedure AllocateKeyargs( nargs, tree );
    lvars need = needKey( tree );
    {%
        lvars i;
        for i from 1 to nargs do
            lvars nd = need( i );
            if nd and nd > 1 then
                lvars v = sysNEW_LVAR();
                ;;; This is faster than using the fake sysSUBSCR_STACK,
                ;;; I reckon.  However, a real sysSUBSCR_STACK would be
                ;;; much more efficient & replace this next bit of code.
                if i == 1 then
                    sysPUSHS( undef );
                    sysCALL( "datakey" );
                    sysPOP( v );
                else
                    sysSWAP( 1, i );
                    sysPUSHS( undef );
                    sysCALL( "datakey" );
                    sysPOP( v );
                    sysSWAP( 1, i );
                endif;
                v
            else
                false
            endif
        endfor;
    %};
enddefine;

;;; Is it worthwhile allocating variables to cache the keys of input
;;; locals?  Yes, often it is.  But there are important special
;;; cases when it isn't.  This code detects those cases.
;;;
;;; "single"    = There is only a single condition for each argument,
;;;               which is a comparison against a single key.
;;; <procedure> = There are no conditions against any argument and
;;;               this is the underlying procedure that gets called.
;;; <false>     = The tree is false.
;;;
define categoriseTree( T );
    if T then
        if nextTree( T ) == [] then
            not( defaultTree( T ) ) and actionTree( T ) or
            not( actionTree( T ) ) and categoriseTree( defaultTree( T ) )
        elseif nextTree( T ).more_than_one then
            false
        elseif defaultTree( T ) then
            false
        else
            lvars b = hd( nextTree( T ) );
            b.keysBranch.exactly_one and
            andlist( b.treesBranch, categoriseTree ) and
            "single"
        endif
    else
        ;;; Copes with <false> instead of trees.
        false
    endif
enddefine;


;;; -- Compile a procedure from a tree --------------------------------------

define tree_to_pd( G, tree, mode, props, arity );
    lvars nargs = arity.isinteger and arity or 0;
    mode.check_mode -> mode;

    dlocal Keyargs = false;
    lvars kind_of_tree = categoriseTree( tree );
    procedure();
        sysPROCEDURE( props, nargs );
        if kind_of_tree.isprocedure then
            Plant_call_part( G, kind_of_tree, undef, undef);
        else
            lvars ( assn, def, n ) = is_mono_tree( tree );
            if assn then
                plant_mono( G, mode, assn, def, n );
            else
                kind_of_tree /== "single" and
                arity.isinteger and
                AllocateKeyargs( nargs, tree ) -> Keyargs;

                Plant_tree( G, tree, 0 );
                unless tree.cantFailTree do
                    plant_method_failure( mode )
                endunless;
            endif
        endif;
        sysLABEL( "return" );
        sysPUSHQ( sysENDPROCEDURE() );
    endprocedure.plant_and_execute;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct  4 1995
        Popc changes
;;; -------------------------------------------------------------------------
;;; Modified, 4/6/93, sfk
;;;     *   Added (U)CALL_MODE selection for dispatch_call_next_method,
;;;         & pick_mode.
;;; -------------------------------------------------------------------------
;;; Modified, 4/5/93, sfk
;;;     *   Minor polishing in preparation to rewrite.  The rewrite
;;;         will assume that methods take a fixed number of arguments
;;;         (although updaters & callers can have different numbers).
;;;         This will simply code and make method dispatch slightly
;;;         faster in the complex cases.  In the simple case, where
;;;         there is only one case, it will probably be best to have a
;;;         special case.
;;; -------------------------------------------------------------------------
;;; Modified, 10/12/92, sfk
;;;     *   Singletonclasses completely revised.
;;; -------------------------------------------------------------------------
;;; Modified, 6/12/92, sfk
;;;     *   Changed erroneous call in Plant_tree to Plant_call_slot to
;;;         Plant_call_part.  This will make full methods work with
;;;         slots properly.
;;;     *   Fixed bug in Plant_call_part which caused Plant_call_slot
;;;         to be called with a non-list parameter.
;;;     *   Fixed bug in Plant_call_part which generated a superfluous
;;;         sysGOTO( "return" )
;;; -------------------------------------------------------------------------
;;; Modified 6/10/92, sfk
;;;     *   Moved assignment to level1 to outside the loop.  This stupid
;;;         error would have been caught in Pop9x ... moan grumble.  This
;;;         caused some serious problems!
;;; -------------------------------------------------------------------------
 */
