/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/src/xt_cbcore.p
 > Purpose:         X toolkit - callback core routines
 > Author:          Roger Evans, Oct  2 1990 (see revisions)
 > Documentation:
 > Related Files:   xt_callback.p xt_input.p xt_timer.p xt_action.p
 */

#_INCLUDE 'xt_declare.ph'

section $-Sys$-Xt  =>
                        XptCallbackHandler,
                        XptTraceCallback,
                        XptApplyCallback,
;

global vars XptTraceCallback = false;   ;;; no tracing
global vars XptApplyCallback = true;    ;;; default apply handling


define global vars XptCallbackHandler(proc, type);
    dlvars proc, type, nargs;

    unless isprocedure(proc) then
        if isident(proc) then
            fast_idval(proc)
        else
            valof(proc)
        endif -> proc;
        unless isprocedure(proc) then
            mishap(proc,1,'PROCEDURE NEEDED');
        endunless;
    endunless;

    /* default callback trace routine */
    define lconstant TraceCallback();
        lvars pname;
        dlocal cucharout;

        returnunless(pdprops(proc)->>pname);

        returnif(
            islist(XptTraceCallback) and
            not(fast_lmember(type, XptTraceCallback))
        );

        if isprocedure(cuchartrace) then
            cuchartrace -> cucharout
        endif;

        printf(pname, type, ';;; %p: %p');
        `(`;
        fast_repeat nargs times;
            cucharout();
            pr(subscr_stack(nargs));
            `,`;
            nargs-1 -> nargs;
        endfast_repeat;
        ->;
        cucharout(`)`); cucharout(`\n`);
    enddefine;

    if XptTraceCallback then
        pdnargs(proc) -> nargs;
        if isprocedure(XptTraceCallback) then
            fast_repeat nargs times;
                subscr_stack(nargs);
            endfast_repeat;
            nargs; proc; type;
            XptTraceCallback();     ;;; user supplied
        else
            TraceCallback();        ;;; system default
        endif;

    endif;

    proc;
    if XptApplyCallback and isprocedure(XptApplyCallback) then
        XptApplyCallback;           ;;; use procedure given
    endif;
    fast_apply();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 19 1993
        Removed XptCallb*ackFlags -- made an autoloadable macro
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Sep  3 1992
        o Default tracing procedure now produces no garbage to stop
          possible side effects with -XptGarbageCursor-
        o Callback type no longer displayed in upper case since it does
          not have to be a string or word.
--- Roger Evans, Jun 28 1991 removed old callback code
--- Roger Evans, Jun 27 1991 removed nargs arg from XptCallbackHandler
--- Roger Evans, Jun 25 1991
        Rewrote callback handling to exploit exfunc_export facilities
--- John Gibson, Jun  4 1991
        Changed -Apply_callback- so that previous value of -interrupt-
        is called if interrupt not called from -mishap-.
--- Roger Evans, Jan 30 1991
        made callbacks ignore CCA_REF fields!
--- Roger Evans, Jan 27 1991
        made callbacks cope better with false CCA_REF fields
--- Roger Evans, Nov  8 1990
        made action numparams arg dereference int ptr
        and fixed bitwise trace test
--- Roger Evans, Nov  4 1990 added actionhooks
--- Roger Evans, Oct 11 1990 Much revised
 */
