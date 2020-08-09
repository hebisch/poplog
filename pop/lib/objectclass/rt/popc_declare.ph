/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:			C.all/lib/objectclass/rt/popc_declare.ph
 > Purpose:			Objectclass runtime declarations for Popc
 > Author:			Robert John Duncan, Sep 29 1995 (see revisions)
 */
compile_mode:pop11 +strict;

library_declare_section '$usepop/pop/lib/objectclass/rt/'

section $-objectclass;

constant procedure (
	check_slot_default,
	shared_slot,
	if_needed,
	failure,
	report_failure,
	do_call_next_method,
	dispatch_call_next_method,
	dispatch_ucall_next_method,
	cached_dispatch_call_next_method,
	destroy_action,
);

vars procedure (
	call_method_part,
	call_method_failure,
);

vars
	CallNextMethodProcs,
	UCallNextMethodProcs,
;

endsection;		/* $-objectclass */

end_library_declare_section;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
		Moved from proto lib
--- Robert John Duncan, Oct 27 1995
		Added if_needed
 */
