/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/parts.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Method Parts ---------------------------------------------------------

compile_mode :pop11 +strict;

section $-objectclass;

;;;
;;; A method part is
;;;     (1) a procedure with or without an updater
;;;     (2) a slot record
;;;     (3) an updater slot record
;;;

define part_updater( p ); lvars p;
    if p.isprocedure then
        p.updater
    elseif p.iscslot then
        newuslot( p )
    elseif p.isuslot then
        false
    else
        internal_fault( [part ^p] )
    endif
enddefine;

define part_pdnargs( p ); lvars p;
    p.isprocedure and p.pdnargs or
    p.iscslot and 1 or
    p.isuslot and 2 or
    internal_fault( [part ^p] )
enddefine;

endsection;

;;; -------------------------------------------------------------------------
;;; Modified 15/11/92 -- sfk
;;;     *   Removed obsolete reference to "ufail", a hangover from a short
;;;         lived "novel" treatment of method parts.
;;; -------------------------------------------------------------------------
