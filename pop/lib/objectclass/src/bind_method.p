/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/bind_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */

section $-objectclass;

define bind_method( mname, mode ); lvars mname, mode;
    lvars id = sys_current_ident( mname );
    if mode.check_mode == UCALL_MODE then
        if id then
            unless mname.sys_current_val.detrace_generic.isgeneric do
                mishap( 'DEFINING UPDATER OF METHOD OF NON-METHOD', [^mname] )
            endunless
        else
            mishap( 'DEFINING UPDATEROF METHOD BEFORE THE METHOD IS DECLARED', [^mname] )
        endif;
    elseif id then
        ;;; ensure the identifier has a generic procedure value: use
        ;;; sys_current_val to assign the initial value because it's
        ;;; guaranteed to do it now whereas sysPASSIGN may wait until
        ;;; the next sysEXECUTE, and we might want to get the value back
        ;;; before then
        lvars previous = detrace_generic( sys_current_val( mname ) );
        if
            previous.isundef or
            id.isconstant and           ;;; catch lconstants
            id.isassignable and
            id.isident /== "perm"
        then
            newgeneric( mname ) -> sys_current_val( mname );
        elseunless previous.isgeneric then
            warning( 'PREVIOUS VALUE WAS NOT A METHOD', [^mname] );
            newgeneric( mname ) -> sys_current_val( mname );
        endif
    else
        internal_error()
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct  4 1995
        Popc changes
;;; Modified, 30/05/93, sfk
;;;     *   Completely rewritten in line with new compilation strategy
;;;         for classes.
 */
