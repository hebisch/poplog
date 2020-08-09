/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/full_methods.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */

section $-objectclass;

define lconstant Full_method_unlinker( Interp, G ); lvars Interp, G;
    if trace_matches( "link" ) then
        printf( ';;; UNlinking FULL method %p\n', [^Interp] )
    endif;
    unlink_interpreter( Interp, G )
enddefine;

define lconstant Full_method_relinker( Interp, G ); lvars Interp, G;
    if trace_matches( "link" ) then
        printf( ';;; Force relinking of FULL method %p\n', [^Interp] );
    endif;
    relink_interpreter( Interp, G )
enddefine;

define full_method =
    newanyproperty(
        [], 8, 1, false,
        false, false, "tmparg",
        false,
        procedure( G, slf ); lvars G, slf;
            if trace_matches( "link" ) then
                printf( ';;; Creating FULL method %p\n', [^G] )
            endif;
            lvars interp = new_interpreter( G );
            interp ->> slf( G );
            add_dependent( Full_method_unlinker, Full_method_relinker, [^interp], G );
            if G.isunlinked_method then
                ;;; The basic G isn't linked.
                Full_method_unlinker
            else
                ;;; Method is already linked.
                Full_method_relinker
            endif( interp, G );
            if trace_matches( "link" ) then
                printf( ';;; Created \n' );
            endif;
        endprocedure);
enddefine;

;;; Provided for debugging only.
define relink_full_method( M ); lvars M;
    Full_method_relinker( full_method( M ), M )
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct  4 1995
        Minor mods to trace output
;;; -------------------------------------------------------------------------
;;; Modified 27/9/92 -- sfk
;;;     *   Altered the lvars "self" to "slf" because it clashes with
;;;         the flavours library.
;;; -------------------------------------------------------------------------
 */
