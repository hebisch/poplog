/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:			C.all/lib/objectclass/rt/destroy_action.p
 > Purpose:			Objectclass runtime support
 > Author:			Robert John Duncan, Oct 25 1995
 */
compile_mode:pop11 +strict;

section $-objectclass;

;;; destroy_action:
;;; 	destroy property for classes with destroy wrappers

define destroy_action =
	clearproperty(dup(copy(sys_destroy_action)));
enddefine;

endsection;		/* $-objectclass */
