/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/src/sig_async.p
 > Purpose:         routines to control asynchronous interrupts
 > Author:          Roger Evans, Apr 18 1988 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   signals.p sigtab.p signals.ph sig_*.p
 */

#_INCLUDE 'declare.ph';
#_INCLUDE 'signals.ph';

lconstant _WORD_RANGE_MASK = _int(2**(WORD_BITS-1)-1);

    ;;; controlling asynchronous interrupts
define active pop_enable_interrupts;
    _not(_disable _bitst _1);
enddefine;

define updaterof active pop_enable_interrupts(/* flag */) with_nargs 1;
    if /* flag */ then
        _disable _biclear _1
    else
        _disable _biset _1
    endif -> _disable
enddefine;

#_IF DEF UNIX

    ;;; send (asynchronous) signal to a process
define sys_send_signal(pid, signum);
    lvars pid, signum;
    Sys$-Check_integer(signum, 0);
;;; (_extern kill(Sys$-Pint_->_sint(pid, _-1), _int(signum)))
    _nonneg(_extern kill(Sys$-Pint_->_sint(pid, _WORD_RANGE_MASK),
            _int(signum)))
enddefine;

#_ENDIF



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Mar 13 2005
        Patch provided by Waldek Hebisch for AMD64 Poplog
--- John Gibson, Jun 17 1992
        Changed sys_send_signal to allow any signed (big)integer value for pid
--- John Gibson, Jun 27 1988
        Put -sys_send_signal- inside #_IF DEF UNIX ...
 */
