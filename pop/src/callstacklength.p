/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/callstacklength.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
 */

;;; ----------------- GET LENGTH OF CALLSTACK ----------------------------

#_INCLUDE 'declare.ph'

global constant
    procedure Sys$-Get_closure_procedure
    ;

;;; -----------------------------------------------------------------------

section $-Sys => callstacklength;

    ;;; get the call stack length of a caller of the current procedure
    ;;; target specified as Nth caller or given procedure
define callstacklength(target);
    lvars target, _sframe = _caller_sp_flush(), _lim = _call_stack_hi;
    if issimple(target) then
        ;;; target is number of caller
        Check_integer(target, 0)
    else
        Check_procedure(target);
        if target!PD_FLAGS _bitst _:M_PD_CLOSURE then
            Get_closure_procedure(target) -> target
        endif
    endif;
    repeat
        if _sframe == _lim then
            returnif(_lim == _call_stack_hi) (false);
            _sframe!SF_NEXT_SEG_HI -> _lim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;

        if issimple(target) then
            quitif(target == 0);
            target fi_- 1 -> target
        else
            quitif(_sframe!SF_OWNER == target)
        endif;
        _nextframe(_sframe) -> _sframe
    endrepeat;
    _pint( ##(csword){_call_stack_hi, _sframe} )
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 10 1989
        Rewritten for segmented callstack
--- John Gibson, Sep  4 1989
        Made it return false if target not found, instead of 0
--- John Gibson, Jul 16 1988
        Replaced _caller_sp() with _caller_sp_flush()
--- John Gibson, Apr  1 1988
        Moved out of control.p
 */
