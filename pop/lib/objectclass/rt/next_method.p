/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/rt/next_method.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

;;; dispatch_call_next_method
;;; -------------------------
;;; Called to process call_next argument.  The point of this procedure
;;; is that it allows us (a) to chain out of the invoking method, freeing
;;; up local stack and (b) introduce a dlocal to hold the chain of
;;; method invocations.

section $-objectclass;

;;; -- Run-time dynamic variables -------------------------------------------

;;; Two dynamic variables, localised during dispatch_[u]call_next_method, to
;;; bind to the list of actions to call.
;;;
vars CallNextMethodProcs = [];  ;;; defensive -- not necessary.
vars UCallNextMethodProcs = []; ;;;           -- as above


;;; These two variables are dlocalised whenever we call the full method.
;;; They are responsible for dispatching failure and success.  NOTE: to
;;; make the dlocalisation work we have to change section during compilation
;;; back to the objectclass section.
;;;
vars procedure call_method_part = chain;
vars procedure call_method_failure = failure;


;;; -- Do call-next-method ------------------------------------------------

define do_call_next_method();
	dlocal CallNextMethodProcs;
	if CallNextMethodProcs == [] then
		mishap( 'CALL_NEXT_METHOD: no next method', [] )
	else
		( fast_destpair( CallNextMethodProcs ) -> CallNextMethodProcs )()
	endif;
enddefine;

;;; NOT true updater -- simplifies code-planting
;;;
define updaterof do_call_next_method();
	dlocal UCallNextMethodProcs;
	if UCallNextMethodProcs == [] then
		mishap( 'CALL_NEXT_METHOD: no next update-method', [] )
	else
		( fast_destpair( UCallNextMethodProcs ) -> UCallNextMethodProcs )()
	endif;
enddefine;


;;; -- Dispatching call next method -----------------------------------------

define lconstant drop_thru();
enddefine;

define updaterof drop_thru();
enddefine;

define find_all_parts( full ); lvars full;
	dlvars start_procs = false;
	dlvars procs;

	dlocal call_method_failure = drop_thru;

	define dlocal call_method_part( p ); lvars p;
		if start_procs then
			conspair( p, [] ) ->> back( procs ) -> procs
		else
			conspair( p, [] ) ->> procs -> start_procs
		endif
	enddefine;

	full();
	return( start_procs or [] )
enddefine;

;;; NOTE: do NOT attempt to apply sys_grbg_list to any of the
;;; list store generated.  It will screw up horribly in the context
;;; of process switching.

define dispatch_call_next_method( full ); lvars full;
	dlocal CallNextMethodProcs = find_all_parts( full );
	( destpair( CallNextMethodProcs ) -> CallNextMethodProcs )();
enddefine;

define dispatch_ucall_next_method( full ); lvars full;
	dlocal UCallNextMethodProcs = find_all_parts( full.updater );
	( destpair( UCallNextMethodProcs ) -> UCallNextMethodProcs )();
enddefine;
;;;
;;; NOT true updater
dispatch_ucall_next_method -> updater(dispatch_call_next_method);

define cached_dispatch_call_next_method( full, table, level ); lvars full, table, level;
	lvars K = level.subscr_stack.datakey;
	dlocal CallNextMethodProcs = table( K );
	unless CallNextMethodProcs do
		find_all_parts( full ) ->> table( K ) -> CallNextMethodProcs
	endunless;
	( destpair( CallNextMethodProcs ) -> CallNextMethodProcs )();
enddefine;

;;; NOT true updater.
define updaterof cached_dispatch_call_next_method( full, table, level ); lvars full, table, level;
	lvars K = level.subscr_stack.datakey;
	dlocal UCallNextMethodProcs = table( K );
	unless UCallNextMethodProcs do
		find_all_parts( full.updater ) ->> table( K ) -> UCallNextMethodProcs
	endunless;
	( destpair( UCallNextMethodProcs ) -> UCallNextMethodProcs )();
enddefine;


;;; -- Calling all next methods -------------------------------------------

define lconstant procedure fast_dl( L ); lvars L;
	until L == [] do
		fast_destpair( L ) -> L
	enduntil
enddefine;

define do_call_all_next_methods( L ); lvars L;
	dlocal CallNextMethodProcs;
	until CallNextMethodProcs == [] do
		( fast_destpair( CallNextMethodProcs ) -> CallNextMethodProcs )
			( fast_dl( L ) )
	enduntil;
enddefine;
;;;
define updaterof do_call_all_next_methods( L ); lvars L;
	dlocal UCallNextMethodProcs;
	until UCallNextMethodProcs == [] do
		( fast_destpair( UCallNextMethodProcs ) -> UCallNextMethodProcs )
			( fast_dl( L ) )
	enduntil;
enddefine;

endsection;		/* $-objectclass */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Sep 29 1995
		Moved to runtime library
;;; Modified, 4/6/93, sfk
;;;     *   Made dispatch_call_next_method have dispatch_ucall_next_method
;;;         as its updater.  This allows selection via (U)CALL_MODE.
 */
