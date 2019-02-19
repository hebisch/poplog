/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/pop_callstack_lim.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
 */

;;; ------------ GET/UPDATE USER LIMIT ON CALLSTACK LENGTH -----------------

#_INCLUDE 'declare.ph'

global constant
        procedure Sys$-Set_call_stack_lim
    ;

global vars
        _pop_callstack_lim
    ;

;;; -----------------------------------------------------------------------

section $-Sys => pop_callstack_lim;

define active pop_callstack_lim;
    _pint(_pop_callstack_lim)
enddefine;

define updaterof active pop_callstack_lim lim;
    lvars lim;
    Check_integer(lim, 0);
    _int(lim) -> _pop_callstack_lim;
    ;;; set the actual value according to O/S limits
    Set_call_stack_lim()
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  1 1989
        User-assigned limit now stored in _pop_callstack_lim; actual
        limit derived from this by -Set_call_stack_lim-.
--- John Gibson, Aug 31 1989
        Added call of -Set_call_stack_lim- at end of updater to
        adjust assigned value.
--- John Gibson, Apr  1 1988
        Moved out of control.p
 */
