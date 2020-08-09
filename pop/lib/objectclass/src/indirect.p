/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/indirect.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-objectclass;

define newindirect_n( n ) -> I; lvars n, I;
    writeable consclosure( n ) -> I;
    ( writeable updater( I ) ) -> updater( I );
enddefine;

define newindirect( p ); lvars p;
    newindirect_n( p, 0 )
enddefine;

constant procedure indirect_pdpart = pdpart;

constant procedure indirect_updater = updater;

constant procedure indirect_frozval = frozval;

define copy_indirect( I ); lvars I;
    writeable copy( I )
enddefine;

endsection;

;;; -------------------------------------------------------------------------
;;; Modified by sfk, Tue Aug 31 18:05:55 BST 1993
;;;     *   Introduced newindirect_n in order to permit
;;;         direct definition of consmethodclosure.
;;; -------------------------------------------------------------------------
