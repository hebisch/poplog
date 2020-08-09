/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/will_apply_to.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => will_apply_to;

;;; Once we have loaded this file, we must not trash the method tables.
;;; This is sad but true!  The ability to link full methods for any
;;; method relies on the preservation of the method tables.
;;;
false -> can_trash_method_table;

uses full_method;

define global procedure will_apply_to( M ); lvars M;
    define dlocal call_method_failure();
        exitfrom( false, will_apply_to )
    enddefine;
    define dlocal call_method_part( p ); lvars p;
        exitfrom( true, will_apply_to )
    enddefine;
    full_method( M )();
    internal_error();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
