/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/class_new.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-objectclass => class_new ;

define do_class_new( k, ind_proc ) -> new;
    lvars name = derive_new_name( class_dataword( k ) );
    procedure();
        lvars fields = filterin( fields_of_class( k ), isxslot );
        sysPROCEDURE( name, 0 );

        lvars I = sysNEW_LVAR();
        lvars f, defprocs_needed = false;
        for f in fields do
            f.xslot_default_proc or defprocs_needed -> defprocs_needed;
            sysPUSHQ( f.xslot_default_value )
        endfor;

        sysCALLQ( class_construct( k ) );

        if defprocs_needed then
            sysPOP( I );
            lvars f, n = 0;
            for f in fields do
                n + 1 -> n;
                if f.xslot_default_proc then
                    sysPUSH( I );
                    sysCALLQ( f.xslot_default_proc );
                    sysPUSH( I );
                    sysUFIELD( n, k.class_field_spec, false, false );
                endif
            endfor;
            sysPUSH( I );
        endif;

        sysPUSHQ( sysENDPROCEDURE() );
    endprocedure.plant_and_execute -> new;

    lvars wrappers = all_wrappers( k, class_new_wrapper );
    if ind_proc then
        wlist_to_closure( new, wrappers, ind_proc ) -> new;
    endif
enddefine;

define lconstant Memo_class_new =
    memofn(
        procedure( oc ) -> ind_proc;
            newindirect( apply ) -> ind_proc;
            do_class_new( oc, ind_proc ) -> indirect_pdpart( ind_proc )
        endprocedure
    )
enddefine;

define appclass_new( p ); lvars p;
    applist( property_domain( Memo_class_new ), p )
enddefine;

define relink_all_class_new();
    procedure( c ); lvars c;
        lvars ind_proc = Memo_class_new( c );
        do_class_new( c, ind_proc ) -> indirect_pdpart( ind_proc )
    endprocedure.appclass_new
enddefine;

sysunprotect( "class_new" );

define global class_new( k ); lvars k;
    Memo_class_new( k.checkr_objectclass )
enddefine;
;;;
define updaterof class_new( p, k ); lvars p, k;
    p -> Memo_class_new( k.checkr_objectclass )
enddefine;

sysprotect( "class_new" );

endsection;


/* --- Revision History ---------------------------------------------------
--- Steve Leach, 19th Sept 2002
        Changed seq_to_closure to wlist_to_closure along with consequent
        modification to pass in the indirect procedure
--- Robert John Duncan, Oct 26 1995
        Changed do_class_new for new wrappers
--- Robert John Duncan, Oct 25 1995
        Changed to use new checkr_objectclass
;;; -------------------------------------------------------------------------
;;; Modified, 2/7/93, sfk
;;;     *   Removed dreadful and bizarre duplication of the entire
;;;         body of -do_class_new-.  It fortunately is declarative so
;;;         no harm was being caused.
;;;     *   Simplified call to seq_to_closure using its recent improvement.
;;; -------------------------------------------------------------------------
;;; Modified, 14/05/93, sfk
;;;     *   Added explicit checks for singletonclasses.  Made
;;;         do_class_new available.
;;; -------------------------------------------------------------------------
;;; Modified, 15/04/93, sfk
;;;     *   Added explicit sys(un)protect to exported identifiers.
;;; -------------------------------------------------------------------------
;;; Modified, 6/12/92, sfk & jonm
;;;     *   Removed redundant use of seq_to_closure.
;;; -------------------------------------------------------------------------
;;; modified 21/10/92, sfk
;;;     *   Get rid of -initialise- with the introduction of wrappers.
;;; -------------------------------------------------------------------------
 */
