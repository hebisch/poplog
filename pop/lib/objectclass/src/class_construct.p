/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/class_construct.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-objectclass => class_construct;

    ;;; forward: checkr_objectclass excludes mixins/singletons
constant procedure checkr_objectclass;


define do_class_construct( key, ind_proc ) -> c;
    lvars c_wrappers = all_wrappers( key, class_cons_wrapper );
    lvars c = wlist_to_closure( class_cons( key ), c_wrappers, ind_proc );
    lvars d_wrappers = all_wrappers( key, class_destroy_wrapper );
    lvars d = wlist_to_closure( erase, d_wrappers, ind_proc );
    if d /== erase then
        ;;; add extra cons wrapper which records new instance in a
        ;;; destroy property
        wlist_to_closure( c, [% make_add_to_destroy_wrapper( d ) %], ind_proc ) -> c;
    endif;
enddefine;

define Memo_class_construct =
    memofn( 
        procedure( oc ) -> p;
            newindirect( apply ) -> p;
            do_class_construct( oc, p ) -> indirect_pdpart( p ); 
        endprocedure
    )
enddefine;

define appclass_construct( p );
    applist( property_domain( Memo_class_construct ), p )
enddefine;

define relink_all_class_construct();
    procedure( c );
        lvars ind_proc = Memo_class_construct( c );
        do_class_construct( c, ind_proc ) -> indirect_pdpart( ind_proc )
    endprocedure.appclass_construct
enddefine;

sysunprotect( "class_construct" );

define global class_construct( k );
    Memo_class_construct( k.checkr_objectclass )
enddefine;

;;; Should class_construct have an updater???
define updaterof class_construct( p, k );
    p -> Memo_class_construct( k.checkr_objectclass );
enddefine;

sysprotect( "class_construct" );

endsection;


/* --- Revision History ---------------------------------------------------
--- Steve Leach, 19th Sept 2002
        Added extra code to accommodate the new wlist_to_closure interface.
--- Robert John Duncan, Oct 26 1995
        Changed do_class_construct for new wrappers and to add a destroy
        wrapper if defined
--- Robert John Duncan, Oct 25 1995
        Changed to use new checkr_objectclass
 */
