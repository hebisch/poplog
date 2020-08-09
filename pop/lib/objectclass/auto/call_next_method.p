/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/call_next_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => call_next_method;

define syntax call_next_method;
    unless MethodName do
        mishap( 'CALL_NEXT_METHOD: must be used inside a method', [] )
    elseif pop_expr_update then
        mishap( 'CALL_NEXT_METHOD: not allowed in update mode', [^MethodName] )
    endunless;
    true -> CurrentProperties( "invokes_call_next_method" );
    pop11_need_nextreaditem( "(" ).erase;
    pop11_comp_expr_seq_to( ")" ).erase;
    apply_mode( sysCALL, MethodMode )("ident do_call_next_method");
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
--- Robert John Duncan, Oct  5 1995
        do_call_next_method moved to the runtime library
;;; -------------------------------------------------------------------------
;;; Modified, 6/10/93, JJC
;;;     *   removed checking for pop_oc_version < 6.
;;; -------------------------------------------------------------------------
;;; Modified, 4/6/93, sfk
;;;     *   Softend check for updater mode use of call_next_method.
;;;         Will be outlawed by version 6.0.
;;; -------------------------------------------------------------------------
;;; Modified, 3/6/93, sfk
;;;     *   Removed explicit update mode for call_next_method.
;;;     *   Deleted superfluous declarations for (U)CallNextMethodProcs.
;;; -------------------------------------------------------------------------
 */
