/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/phantoms.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Phantoms -------------------------------------------------------------

compile_mode :pop11 +strict;

section $-objectclass;

lvars phantom_count = 0;

;;; -undefphantom- is the undef result returned when a phantom has been
;;; garbage collected.
constant undefphantom = consundef("phantom");

lconstant procedure Cont_phantom =
    newproperty( [], 64, undefphantom, "tmpval" );

define lconstant procedure Check_phantom( x ); lvars x;
    unless x.isintegral do
        mishap( 'Phantom needed', [^x] )
    endunless;
enddefine;

define contphantom( x ); lvars x;
    Check_phantom( x );
    Cont_phantom( x );
enddefine;

define updaterof contphantom( v, x ); lvars v, x;
    Check_phantom( x );
    v -> Cont_phantom( x );
enddefine;

define newphantom( x ); lvars x;
    phantom_count + 1 ->> phantom_count;    ;;; return result
    x -> contphantom( phantom_count );
enddefine;

define isundefphantom( x ); lvars x;
    Check_phantom( x );
    Cont_phantom( x ) == undefphantom
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Feb 15 1996
        Simplified construction of undefphantom
 */
