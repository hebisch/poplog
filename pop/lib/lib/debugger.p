/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/lib/debugger.p
 > Purpose:         Loads the symbolic debugger
 > Author:          Simon Nichols, Jun 18 1991
 > Documentation:   HELP * DEBUGGER
 > Related Files:   debugger/core.p debugger/user.p
 */

#_TERMIN_IF DEF POPC_COMPILING or DEF debugger

section;

loadlib('debugger/core.p');
loadlib('debugger/user.p');

$-debugger$-redefine_code_planters();
true -> debugger_debugging;

endsection;
