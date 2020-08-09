/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/relink_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Relink a Method ---------------------------------------------------

compile_mode :pop11 +strict;

section $-objectclass;

define No_method_parts();
    mishap( 'ocmwp: APPLYING METHOD WITHOUT PARTS', [] )
enddefine;

define relink_method_with_table( M, MT ); lvars M, MT;

    if all_classed_methods_are_unlinked then
        if M.is_class_free_method then
            upgrade_methods( true )
        else
            ;;; Tell upgrade_methods not to bother checking whether
            ;;; methods are unlinked or not.  We are relinking a classed
            ;;; method.
            upgrade_methods( false );
            false -> all_classed_methods_are_unlinked;
        endif
    endif;

    if trace_matches( "link" ) then
        printf( ';;; RElinking method %p\n', [^M] )
    endif;

    lockMethodTable( MT );

    lvars centries = cEntriesMethodTable( MT );
    lvars uentries = uEntriesMethodTable( MT );

    lvars need_updater = not( null( uentries ) );

    lvars carity = cArityMethodTable( MT );
    lvars uarity = need_updater and uArityMethodTable( MT );

    lvars tree  = net_to_tree( centries );
    lvars utree = need_updater and net_to_tree( uentries );

    unlockMethodTable( MT );

    instantiate( tree, true );

    lvars p = false, u = false;
    if tree then
        tree_to_pd( M, tree, CALL_MODE, M.pdprops, carity ) -> p;
    endif;
    if need_updater do
        instantiate( utree, true );
        if utree then
            tree_to_pd( M, utree, UCALL_MODE, M.pdprops, uarity ) -> u;
        endif;
    endif;
    p or No_method_parts -> indirect_pdpart( M );
    set_pdnargs_mode( carity, M, CALL_MODE );
    if need_updater then
        u -> indirect_updater( M );
        if u then
            set_pdnargs_raw( uarity, M.indirect_updater )
        endif;
    else
        false -> indirect_updater( M )
    endif;

    relink_dependents( M );
    relink_hook( M );           ;;; This is how we implement traced methods.
enddefine;

define relink_method( M ); lvars M;
    relink_method_with_table( M.dup.method_table )
enddefine;

endsection;

;;; -------------------------------------------------------------------------
;;; Modified 15/11/92 -- sfk
;;;     *   Eliminated reference to "ufail" and thereby corrected the
;;;         no_updater optimisation.  This should only lead to an
;;;         improvement in compilation speed.
;;; -------------------------------------------------------------------------
;;; Modified, 3/5/93, sfk
;;;     *   Introduced the fixed arity for generics.  This simplifies
;;;         the calculation of arity.
;;;     *   Removed the variable no_updater -- obsolete after the
;;;         introduction of arity_entries.
;;; -------------------------------------------------------------------------
;;; Modified, 22/5/93, sfk
;;;     *   More checks required in tree_to_pd to prevent assigning
;;;         pdnargs to non-existant updater procedure.
;;; -------------------------------------------------------------------------
;;; Modified, 1/6/93, sfk
;;;     *   Introduced set_pdnargs to guard against bits of methods
;;;         being moved outside the heap after sys_lock_system.
;;; -------------------------------------------------------------------------
;;; Modified, 04/06/93, sfk
;;;     *   Added (U)CALL_MODE values to the invocations of tree_to_pd.
;;; -------------------------------------------------------------------------
;;; Modified, 07/03/03, sfk
;;;     *   Use set_pdnargs_mode to improve values for pdnargs
;;; -------------------------------------------------------------------------
