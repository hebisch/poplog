/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/iscaller.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
 */

;;; --------- TEST FOR A PROCEDURE IN THE CALLING CHAIN -------------------

#_INCLUDE 'declare.ph'

constant
        procedure Sys$-Get_closure_procedure
    ;

;;; -----------------------------------------------------------------------

section $-Sys => iscaller;

    /*  Test if a given procedure is in the calling chain,
        starting from an optional caller number
    */
define iscaller(target);
    lvars target, startnum = 0, _sframe = _caller_sp_flush(), _num = 0,
        _lim = _call_stack_seg_hi;
    if isinteger(target) then
        ;;; caller number to start from
        target -> startnum -> target;
        Check_integer(startnum, 0)
    endif;
    Check_procedure(target);
    if target!PD_FLAGS _bitst _:M_PD_CLOSURE then
        Get_closure_procedure(target) -> target
    endif;
    repeat
        if _sframe == _lim then
            quitif(_lim == _call_stack_hi);
            _sframe!SF_NEXT_SEG_HI -> _lim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;
        returnif(_num fi_>= startnum and _sframe!SF_OWNER == target) (_num);
        _nextframe(_sframe) -> _sframe;
        _num fi_+ 1 -> _num
    endrepeat;
    false
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 10 1989
        Changed for segmented callstack
--- John Gibson, Jul 16 1988
        Replaced _caller_sp() with _caller_sp_flush()
--- John Gibson, Apr  1 1988
        Moved out of control.p
 */
