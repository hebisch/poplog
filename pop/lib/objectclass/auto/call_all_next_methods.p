/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/call_all_next_methods.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => call_all_next_methods;

define global syntax call_all_next_methods;
    unless MethodName do
        mishap( 'CALL_ALL_NEXT_METHODS: MUST BE USED INSIDE A METHOD', [] )
    endunless;

    if MethodMode == UCALL_MODE then
        true -> CurrentProperties( "invokes_ucall_next_method" );
    else
        true -> CurrentProperties( "invokes_call_next_method" );
    endif;

    pop11_need_nextreaditem( "(" ).erase;
    sysPUSH( "popstackmark" );
    pop11_comp_expr_seq_to( ")" ).erase;
    sysCALL( "sysconslist" );
    apply_mode( sysCALL, MethodMode )( "ident do_call_all_next_methods" );

    if pop_expr_update then
        mishap( 'CALL_ALL_NEXT_METHODS NO LONGER HAS UPDATE MODE', [^MethodName] )
    endif;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
--- Robert John Duncan, Oct  4 1995
        do_call_all_next_methods moved to runtime library
;;; -------------------------------------------------------------------------
;;; Modified, 6/10/93, JJC
;;;     *   removed checking for pop_oc_version < 6.
;;; -------------------------------------------------------------------------
;;; Modified, 3/6/93, sfk
;;;     *   Removed explicit update mode for call_all_next_methods.
;;;         Now uses MethodMode.
;;;     *   Fixed 2 long-standing bugs in update mode of call_all_next_methods.
;;;         (It incorrectly used CallNextMethodProcs)
;;; -------------------------------------------------------------------------
 */
