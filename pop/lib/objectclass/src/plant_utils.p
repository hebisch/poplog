/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/plant_utils.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
section $-objectclass;

;;; -- Compile-time dynamic variables ---------------------------------------

;;; This is bound to either <false> or a vector of variable names (or
;;; <false>) holding previously computed keys.
;;;
vars Keyargs;                   ;;; Used as a DLOCAL variable.


;;; -- Utilities ------------------------------------------------------------

define plant_method_failure( mode ); lvars mode;
    apply_mode( sysCALL, mode )( "ident failure" );
enddefine;

define push_arg( level ); lvars level;
    lvars ka;
    if Keyargs and Keyargs( level ) ->> ka then
        sysPUSH( ka )
    else
        sysSUBSCR_STACK( level );
        sysCALL( "datakey" );
    endif
enddefine;

define plant_callq( p ); lvars p;
    if p then sysPUSHQ( p ) endif;
    sysCALL( "fast_chain" )
    ;;; sysCALL( "fast_apply" );
    ;;; sysGOTO( "return" );
enddefine;

define slot_proc_for_key( key, slot, ind_proc );
    lvars mode = slot.isuslot and UCALL_MODE or CALL_MODE;
    lvars posn = xslot_posn( key, slot );
    lvars A, L;
    if mode == CALL_MODE then
        class_access( posn, key ) -> A;
        all_wrappers( key, class_access_wrapper ) -> L;
    else
        class_access( posn, key ).updater -> A;
        all_wrappers( key, class_update_wrapper ) -> L;
    endif;
    lvars p = wlist_to_closure( A, L, ind_proc );
    return( mode, posn, p == A, p )
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Steve Leach, 19th Sept 2002
        Renamed slot_for_key to slot_proc_for_key (defensive)
        Added indirect procedure to slot_proc_for_key
--- Robert John Duncan, Oct 26 1995
        Changed slot_for_key for new wrappers
--- Robert John Duncan, Oct  4 1995
        Popc changes
 */
