/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/edinsynt.pl
 > Purpose:         Load EDINSYNT (Edinburgh syntax compatibility) library
 > Author:          Simon Nichols, Mar 22 1990 (see revisions)
 > Documentation:   PLOGHELP * EDINSYNT
 > Related Files:   C.all/plog/lib/edinsynt/ (directory)
 */

:- prolog_language(pop11).

#_IF not(DEF edinsynt)

compile_mode:pop11 +strict;

section;

procedure();
    dlocal current_directory = '$usepop/pop/plog/lib/edinsynt';
    subsystem_compile('edinsynt.pl', false);
endprocedure();

global vars edinsynt = true;

endsection;

#_ENDIF

:- prolog_language(prolog).

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, May 20 1993
        Changed to use subsystem_compile.
--- Simon Nichols, Jun 28 1990
        Changed to do nothing after it has been loaded once.
 */
