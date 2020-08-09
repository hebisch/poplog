/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/upgrade_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-objectclass;

;;; -- upgrade table --------------------------------------------------------

;;; Are there any upgrades outstanding?
;;;
vars upgrades_pending = false;

;;; The upgrade table is a property with a modified updater.  This
;;; modification allows the -upgrades_pending- variable to be kept
;;; up to date.  Alas, the upgrade table must be tmpval -- holding
;;; onto the old keys for the duration.  It has to do this because
;;; -pop_oc_sensitive_methods- requires methods can get
;;; upgraded.  And since we are delaying the upgrade we must hang
;;; on to the old classes to prevent relevant method-parts being
;;; deleted from methods.
;;;
constant procedure upgrade_table =
    newproperty( [], 16, false, "tmpval" );

lconstant uupgrade_table = updater( upgrade_table );

procedure( v, k ); lvars v, k;
    true -> upgrades_pending;
    uupgrade_table( v, k )
endprocedure -> updater( upgrade_table );

;;; Trace the full chain of references in the upgrade table.  We call
;;; this before we begin the upgrade process because we may have several
;;; generations of upgrades and we would like to discharge them all.
;;;
define refine_upgrade_table();

    define lconstant refine( k1, k2 ); lvars k1, k2;
        lvars k3 = upgrade_table( k2 );
        k3 and refine( k1, k3 ) or k2
    enddefine;

    appproperty(
        upgrade_table,
        procedure( k1, k2 ); lvars k1, k2;
            refine( k1, k2 ) -> upgrade_table( k1 )
        endprocedure
    )
enddefine;


;;; -- upgrade a method -----------------------------------------------------

;;; We want to upgrade the method from using the old class in its definitions
;;; to using the new class.  Depending on pop_oc_sensitive_method
;;; we may or may not wish to delete the use of the old class.
;;;
;;; Note that we only unlink the method if we make a change.
;;;
define upgrade_method( M ); lvars M;

    define lconstant procedure upgrade( tv ); lvars tv;
        lvars spot_old_class = false;
        lvars L;
        [%
            lvars c;
            for c in [% tv.explode_tmpvec %] do
                lvars NewClass = upgrade_table( c );
                if NewClass then
                    true -> spot_old_class;
                    NewClass
                else
                    c
                endif;
            endfor;
        %] -> L;
        spot_old_class and L.destlist.constmpvec
    enddefine;

    define lconstant procedure obsolete( tv, entries ); lvars tv, entries;
        ;;; # Phantoms and hence tmpvecs can be compared using =.
        ;;; # We don't have to worry about melting causing a fake <false>
        ;;;   here.  Only -tv- can melt (-entries- is frozen) and even it
        ;;;   it got included it would be filtered out later by the
        ;;;   subsequent method-linkage.
        member( tv, maplist( entries, classesEntry ) )
    enddefine;

    ;;; Defensive programming.
    returnunless( pop_oc_sensitive_methods );

    lvars MT = M.method_table;
    lockMethodTable( MT );

    lvars f;
    for f in fieldsOfMethodTable do
        lvars spotted_difference = false;   ;;; Do we need to make a change?
        lvars new_es =
            [%
                lvars e;
                for e in f( MT ) do
                    lvars a = actionEntry( e );
                    if a.isxslot then
                        ;;; Cannot upgrade slots!
                        e
                    else
                        lvars cs = classesEntry( e );
                        lvars new_cs = upgrade( cs );
                        if new_cs then
                            true -> spotted_difference;
                            if pop_oc_sensitive_methods == "both" then
                                e
                            endif;
                            if obsolete( new_cs, f( MT ) ) then
                                ;;; Throw the obsolete entry away.
                                if trace_matches( "upgrade" ) then
                                    printf( ';;; Upgrade has been obsoleted in method %p\n', [% M.pdprops %] )
                                endif
                            else
                                consEntry( new_cs, a );
                            endif;
                        else
                            e
                        endif
                    endif
                endfor;
            %];
        if spotted_difference then
            ;;; We need to alter the [u|c]entriesMethodable.
            if trace_matches( "upgrade" ) then
                printf( ';;; Upgrading entries for method %p\n', [% M.pdprops %] )
            endif;
            new_es -> f( MT );
            unlink_method( M );
        endif;
    endfor;

    unlockMethodTable( MT );
enddefine;

;;; -- upgrade all methods --------------------------------------------------

;;; Note how we always take the opportunity to spot when all the methods
;;; are unlinked.  If we can set the -all_classed_methods_are_unlinked- flag then
;;; everything goes much faster.
;;;
define upgrade_methods( do_check ); lvars do_check;
    if upgrades_pending then
        refine_upgrade_table();
        if do_check then
            lvars seen_linked = false;
            procedure( M ); lvars M;
                upgrade_method( M );
                unless isunlinked_method( M ) do
                    unless M.is_class_free_method do
                        true -> seen_linked;
                    endunless
                endunless;
            endprocedure.app_all_methods;
            not( seen_linked ) -> all_classed_methods_are_unlinked;
        else
            upgrade_method.app_all_methods;
        endif;
        false -> upgrades_pending;
        clearproperty( upgrade_table );
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 12 1995
        Added Steve Knight's fix to upgrade_method whereby method-table
        entries obsoleted by upgrading are deleted; an obsolete entry can
        arise if the method table has been updated while an upgrade is
        pending.
 */
