/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:			C.all/lib/objectclass/objectclass.standalone.p
 > Purpose:			Objectclass: load core sources
					using test directory.
 > Author:			Aaron Sloman 9 Mar 2003 (See revision notes)
 > Documentation:	HELP * OBJECTCLASS
 > Related Files:	LIB * OBJECTCLASS
 */
compile_mode:pop11 +strict;

section $-objectclass => pop_oc_version;

;;; Setup the environment
constant objectclass_dir = sys_fname_path(popfilename);

;;; This compiles sources and declars "objectclass"
uses objectclass;

endsection;		/* $-objectclass */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 9 Mar 2003
	Made available for testing purposes
	It is not sufficient to compile objectclass.p, as that does not
	declare "objectclass" as an identifier, so that repeated invocations of
	'uses objectclass' will mishap.
	This is needed only for a stand-alone, test version of objectclass
	not installed in the usual location.

 */
