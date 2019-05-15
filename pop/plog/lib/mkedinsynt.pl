/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/plog/lib/mkedinsynt.pl
 > Purpose:         Create saved image with library EDINSYNT loaded
 > Author:          Simon Nichols, Feb  7 1990 (see revisions)
 > Documentation:   PLOGHELP * EDINSYNT
 > Related Files:   C.all/plog/lib/edinsynt.pl
 */

:- library(edinsynt).
:- prolog_syntax(edinburgh).

:- prolog_language(pop11).

if sys_lock_system('$poplocalbin/edinsynt.psv', true, pop_system_version) then
    prolog_version('Edinburgh Syntax');
    sys_startup_subsystems();
    setpop();
endif;

sysunlink('$poplocalbin/edinsynt.psv-') -> ;

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Oct 24 1991
        Changed call of syntax/1 to prolog_syntax/1.
--- Simon Nichols, Jul 19 1990
        Moved to from C.all/plog/lib/edinsynt to C.all/plog/lib/
--- Simon Nichols, Jul 18 1990
        Changed to use -sys_startup_subsystems- (part of new LIB SUBSYSTEM).
 */
