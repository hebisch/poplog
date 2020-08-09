/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/class_isa.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */

section $-objectclass => class_isa;

define lconstant Relink_isa(key, clos);
    if trace_matches( "link" ) then
        printf( ';;; RElinking isa-procedure for %p\n', [% key.class_name %] )
    endif;
    lvars infs = [];
    fast_repeat (#| all_infs([^key]) |#) times
        lvars inf = ();
        if iskey(inf) then inf :: infs -> infs endif;
    endrepeat;
    if infs == [] then
        ;;; mixin with no class inferiors -- can't be instantiated
        procedure(_);
            false
        endprocedure
    elseif fast_back(infs) == [] then
        ;;; class with no class inferiors -- use standard class recogniser
        class_recognise(fast_front(infs))
    else
        sysPROCEDURE(pdprops(clos), 1);
            sysCALL("datakey"), sysPUSHQ(infs), sysCALL("fast_lmember");
            ;;; convert to boolean
            lvars l = sysNEW_LABEL();
            sysAND(l), sysPUSH("true"), sysLABEL(l);
        sysENDPROCEDURE()
    endif -> pdpart(clos);
enddefine;

define lconstant Unlink_isa(key, clos);
    lconstant UNLINKED_ISA = 'UnlinkedIsa';
    unless pdprops(pdpart(clos)) == UNLINKED_ISA then
        if trace_matches( "link" ) and pdpart(clos) /== identfn then
            ;;; must have been previously linked
            printf( ';;; UNlinking isa-procedure for %p\n', [% key.class_name %] )
        endif;
        define lvars UnlinkedIsa(x);
            Relink_isa(key, clos);
            fast_chain(x, clos);
        enddefine;
        UnlinkedIsa -> pdpart(clos);
        UNLINKED_ISA -> pdprops(UnlinkedIsa);
    endunless;
enddefine;

;;; Has to be vars procedure because of memoisation.
sysunprotect( "class_isa" );
define global vars procedure class_isa( K ) -> H; lvars K, H;
    newindirect(identfn) -> H;
    derive_ako_name( class_name( K ) ) -> pdprops( H );
    Unlink_isa(K, H);
enddefine;
memofn( class_isa ) -> class_isa;
sysprotect( "class_isa" );

define unlink_all_isas();
    appproperty(class_isa, Unlink_isa);
enddefine;

define appisas( p ); lvars p;
    applist( property_range( class_isa ), p );
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jan 15 1997
        Fixed Relink_isa to consider only genuine keys, not mixins
--- Robert John Duncan, Oct  4 1995
        Popc changes
--- Integral Solutions Ltd, Sep  1 1994 (Julian Clinton)
        Modified debugging info to use class_name instead of
        class_dataword.
;;; -------------------------------------------------------------------------
;;; Modified, 15/04/93, sfk
;;;     *   Added explicit sys(un)protect to exported identifiers.
;;; -------------------------------------------------------------------------
;;; Modified, 10/12/92, sfk
;;;     *   Removed superfluous code for reasoning about singletons thanks
;;;         to the singletonclass revision.
;;;     *   Corrected the pdprops for the "isaXXX" procedure.  This got
;;;         zapped by an incautious edit back in version 5.04.
;;; -------------------------------------------------------------------------
;;; Modified by sfk, 9/11/92
;;;     *   removed sys_grbg_list from appisas after paranoid fantasy
;;;         explained by Chris Dollin.
;;; -------------------------------------------------------------------------

 */
