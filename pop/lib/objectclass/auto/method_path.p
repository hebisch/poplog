/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/method_path.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => method_path;

sysunprotect( "method_path" );

;;; Once we have loaded this file, we must not trash the method tables.
;;; This is sad but true!  The user program that mucks around with method
;;; tables requires them to live on.
;;;
false -> can_trash_method_table;

define global procedure method_path( classes, m ); lvars classes, m;
    if m.isgeneric do
        actionEntry( method_entry( classes, m.method_table ) )
    else
        mishap( 'Method needed', [^m] )
    endif
enddefine;

define updaterof method_path( action, classes, m ); lvars action, classes, m;
    if m.isgeneric do
        if action.isprocedure then
            update_method( action, false, classes, m )
        elseunless action then
            ;;; remove both caller/updater of method.
            update_method( false, true, classes, m );   ;;; update mode
            update_method( false, false, classes, m );  ;;; caller mode
        else
            mishap( 'Procedure or false needed', [^action] )
        endif;
    else
        mishap( 'METHOD NEEDED', [^m] )
    endif
enddefine;

sysprotect( "method_path" );

endsection;

;;; -------------------------------------------------------------------------
;;; Modified, 11/04/93, sfk
;;;     *   Altered the way methods are removed to exploit the enhanced
;;;         updateMethodTable.
;;; -------------------------------------------------------------------------

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
