/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/src/xt_event.p
 > Purpose:         X Toolkit - events
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'
#_IF DEF UNIX
#_INCLUDE '../../src/unix_select.ph'
#_ENDIF


    /*  N.B. This Ved declaration is needed */
weak constant
        procedure vedprocess_try_input
    ;


section $-Sys$-Xt  =>
                        fast_XtAppPending,
                        fast_XtAppPeekEvent,
                        fast_XtAppNextEvent,
                        fast_XtAppProcessEvent,
                        fast_XtDispatchEvent,
                        fast_XtAppMainLoop,
                        fast_XtSetSensitive,
                        fast_XtIsSensitive,
                        fast_XptAppTryEvents,
                        XptAsyncAppContext,
                        XptSetXtWakeup,
                        fast_XptInterruptHandler,
                        fast_XtInsertEventHandler,
                        fast_XtAddEventHandler,
                        fast_XtRemoveEventHandler,
                        fast_XtInsertRawEventHandler,
                        fast_XtAddRawEventHandler,
                        fast_XtRemoveRawEventHandler,
                        XptInterruptHandler
;

constant procedure Clear_async_appcons;


;;; -----------------------------------------------------------------------

;;; Add event handler to widget dependancies
;;;     widget - widget in question
;;;     proc   - external proc to be registered
;;;     data   - client data the proc is registered with
;;;     mask   - uint, event mask
;;;     others - bool, -true- if proc registered with non-maskable events
;;;     raw    - bool, -true- if a raw event handler
define lconstant AddEventHandler(widget, proc, data, mask, others, raw);
    lvars widget, proc, data, mask, others, raw;
    lvars ptr, vec;

    if (Get_Descriptor(XDT_WIDGET,widget) ->> widget) then

        ;;; Get widgets list of event handlers
        widget!XD_DEPENDENTS -> widget;
        widget!XD_W_EVENTHANDLERS -> ptr;

        ;;; Is handler already a dependent?
        until (ptr == []) do;
            ptr!P_FRONT -> vec;

            ;;; If it is, alter masks & return
            if  vec!V_WORDS[_0] == proc and vec!V_WORDS[_1] == data
            and vec!V_WORDS[_4] == raw
            then
                vec!V_WORDS[_2] || mask -> vec!V_WORDS[_2];
                vec!V_WORDS[_3] or others -> vec!V_WORDS[_3];
                return(); /** EARLY EXIT **/
            endif;

            ptr!P_BACK -> ptr;
        enduntil;

        ;;; Otherwise, add a new handler to the widgets list
        consvector(proc,data,mask,others,raw,5) :: widget!XD_W_EVENTHANDLERS
            -> widget!XD_W_EVENTHANDLERS;
    endif;

enddefine;


;;; Remove an event handler & arg from a widgets dependencies (if necessary)
;;; (args as for -AddEventHandler-)
define lconstant RemoveEventHandler(widget, proc, data, mask, others, raw);
    lvars widget, proc, data, mask, others, raw;
    lvars prev_pair=false, pair, vec;

    if (Get_Descriptor(XDT_WIDGET,widget) ->> widget) then

        ;;; Get widgets list of event handlers
        widget!XD_DEPENDENTS -> widget;
        widget!XD_W_EVENTHANDLERS -> pair;

        ;;; Try to find handler in list
        until (pair == []) do;
            pair!P_FRONT -> vec;

            quitif(
                vec!V_WORDS[_0] == proc
                and vec!V_WORDS[_1] == data
                and vec!V_WORDS[_4] == raw
            );

            pair -> prev_pair;
            pair!P_BACK -> pair;
        enduntil;

        ;;; If handler not found, return
        ;;; (proc should not error according to the intrinsics)
        returnif(pair == []); /** EARLY EXIT **/

        ;;; Update the event masks
        ((~~mask) && vec!V_WORDS[_2]) ->> vec!V_WORDS[_2] -> mask;
        not(others) -> vec!V_WORDS[_3];

        ;;; If handler recieves no events - remove it
        if mask == 0 and others == true then
            if prev_pair then
                pair!P_BACK -> prev_pair!P_BACK;
            else
                pair!P_BACK -> widget!XD_W_EVENTHANDLERS;
            endif;
            sys_grbg_destpair(pair) -> ->;
        endif;

    endif;
enddefine;


define fast_XtInsertEventHandler(
    widget, event_mask, nonmaskable, proc, client_data, pos
);
    lvars widget event_mask nonmaskable proc client_data pos;

    X_apply(
        widget,
        event_mask, XTC_bool(nonmaskable),
        proc, client_data,
        pos,
    _6, _extern XtInsertEventHandler) ->;

    AddEventHandler(
        widget,
        proc, client_data,
        event_mask, nonmaskable,
        false
    );

enddefine;


define fast_XtAddEventHandler(
    widget, event_mask, nonmaskable, proc, client_data
);
    lvars widget event_mask nonmaskable proc client_data;

    X_apply(
        widget,
        event_mask, XTC_bool(nonmaskable),
        proc, client_data,
    _5, _extern XtAddEventHandler) ->;

    AddEventHandler(
        widget,
        proc, client_data,
        event_mask, nonmaskable,
        false
    );

enddefine;


define fast_XtRemoveEventHandler(
    widget, event_mask, nonmaskable, proc, client_data
);
    lvars widget event_mask nonmaskable proc client_data;

    X_apply(
        widget,
        event_mask, XTC_bool(nonmaskable),
        proc, client_data,
    _5, _extern XtRemoveEventHandler) ->;

    RemoveEventHandler(
        widget,
        proc, client_data,
        event_mask, nonmaskable,
        false
    );

enddefine;


define fast_XtInsertRawEventHandler(
    widget, event_mask, nonmaskable, proc, client_data, pos
);
    lvars widget event_mask nonmaskable proc client_data pos;

    X_apply(
        widget,
        event_mask, XTC_bool(nonmaskable),
        proc, client_data,
        pos,
    _6, _extern XtInsertRawEventHandler) ->;

    AddEventHandler(
        widget,
        proc, client_data,
        event_mask, nonmaskable,
        true
    );

enddefine;


define fast_XtAddRawEventHandler(
    widget, event_mask, nonmaskable, proc, client_data
);
    lvars widget event_mask nonmaskable proc client_data;

    X_apply(
        widget,
        event_mask, XTC_bool(nonmaskable),
        proc, client_data,
    _5, _extern XtAddRawEventHandler) ->;

    AddEventHandler(
        widget,
        proc, client_data,
        event_mask, nonmaskable,
        true
    );

enddefine;


define fast_XtRemoveRawEventHandler(
    widget, event_mask, nonmaskable, proc, client_data
);
    lvars widget event_mask nonmaskable proc client_data;

    X_apply(
        widget,
        event_mask, XTC_bool(nonmaskable),
        proc, client_data,
    _5, _extern XtRemoveRawEventHandler) ->;

    RemoveEventHandler(
        widget,
        proc, client_data,
        event_mask, nonmaskable,
        true
    );

enddefine;

;;; -----------------------------------------------------------------------

define fast_XtAppPending with_nargs 1;
    _pint(X_cb_apply(_1, _extern XtAppPending));
enddefine;

define fast_XtAppPeekEvent with_nargs 2;
    X_cb_apply(_2, _extern XtAppPeekEvent) == _:XtTrue;
enddefine;

define fast_XtAppNextEvent with_nargs 2;
    X_cb_apply(_2, _extern XptAppNextEvent) ->;
enddefine;

define fast_XtAppProcessEvent with_nargs 2;
    X_cb_apply(_2, _extern XptAppProcessEvent) ->;
enddefine;

define fast_XtDispatchEvent with_nargs 1;
    X_cb_apply(_1, _extern XptDispatchEvent) == _:XtTrue;
enddefine;

define fast_XtAppMainLoop with_nargs 1;
    Clear_async_appcons();
    X_cb_apply(_1, _extern XptAppMainLoop) ->;
enddefine;

define fast_XtSetSensitive with_nargs 2;
    X_cb_apply(XTC_bool(), _2, _extern XtSetSensitive) -> ;
enddefine;

define fast_XtIsSensitive with_nargs 1;
    X_cb_apply(_1, _extern XtIsSensitive) == _:XtTrue;
enddefine;

define fast_XptAppTryEvents(appcon);
    lvars appcon, want_result = false, event_processed = false;

    if isboolean(appcon) then ((), appcon) -> (appcon, want_result) endif;

    ;;; do nothing if currently inside Xt calls
    unless _nonzero(_in_X_call) then
        while _nonzero(X_cb_apply(appcon, _1, _extern XptAppTryEvents)) do
            true -> event_processed;
            _CHECKINTERRUPT
        endwhile
    endunless;

    if want_result then event_processed endif
enddefine;


;;; --- async appcontexts ------------------------------------------------

define XptAsyncAppContext(appcon);
    lvars appcon, desc = Get_Descriptor(XDT_APPCONTEXT,appcon), clos;
    unless desc then mishap(appcon, 1, 'AppContext NEEDED') endunless;
    (desc!XD_DEPENDENTS!XD_AC_ASYNC ->> clos) and fast_frozval(1,clos)
enddefine;
;;;
define updaterof XptAsyncAppContext(trap_p, appcon);
    lvars   clos, trap_p, appcon, desc = Get_Descriptor(XDT_APPCONTEXT,appcon);

    define lconstant pending_handler(trap_p, desc);
        lvars trap_p, desc;
        returnunless(trap_p);       ;;; in case cancelled
        if _nonzero(_in_X_call) then
            ;;; do nothing if currently inside Xt calls, but ensure that
            ;;; polling is re-enabled for appcon
            _extern _pop_set_async_appcon(desc!XP_PTR,
                            desc!XD_DEPENDENTS!XD_AC_ASYNC,
                            if trap_p==true then _1 else _0 endif) ->
        elseif trap_p == true then
            ;;; handle same as fast_XptAppTryEvents(%appcon%)
            while _nonzero(X_cb_apply(desc, _1, _extern XptAppTryEvents)) do
                _CHECKINTERRUPT
            endwhile
        else
            ;;; call user handler
            fast_chain(trap_p)
        endif
    enddefine;

    unless desc then mishap(appcon, 1, 'AppContext NEEDED') endunless;
    desc!XD_DEPENDENTS!XD_AC_ASYNC -> clos;

    if trap_p then
        ;;; setting
        if trap_p /== true then Check_procedure(trap_p) endif;
        unless clos then
            copy_fixed(pending_handler(%false, desc%)) ->> clos
                                    -> desc!XD_DEPENDENTS!XD_AC_ASYNC
        endunless;
        ;;; enable -- the closure will be passed as arg to
        ;;; AST_APP_PENDING traps, which will call it
        if _neg(_extern _pop_set_async_appcon(desc!XP_PTR, clos,
                                if trap_p==true then _1 else _0 endif))
        then
            mishap(appcon, 1, 'CANNOT SET ASYNC APPCONTEXT')
        endif;
        trap_p -> fast_frozval(1,clos)          ;;; set trap procedure

    else
        ;;; clearing
        if clos and fast_frozval(1,clos) then
            ;;; is enabled -- clear
            _extern _pop_set_async_appcon(desc!XP_PTR, _NULL, _0) -> ;
            false -> fast_frozval(1,clos)       ;;; clear trap procedure
        endif
    endif
enddefine;

    ;;; Clear all async appcontexts (used by XtAppMainLoop and sysfork)
define Clear_async_appcons();
    lvars clos;
    ;;; returns -clos- for first one or NULL if none
    until (_extern _pop_async_appcon_clos() ->> clos) == _NULL do
        false -> XptAsyncAppContext(fast_frozval(2,clos))
    enduntil
enddefine;


;;; -- read/wait handlers -------------------------------------------
;;; These combine the wait states for X toolkit events, Xved, and
;;; interactive-device input/hibernation. A sort of primitive process
;;; scheduler ...

    ;;; __pop_appcons_normal is a bit mask for those appcons enabled with
    ;;; normal processing (i.e. true -> XptAsyncAppContext(appcon)).
lconstant macro WAIT_APPCONS_ACTIVE = [
    _zero(_in_X_call) and _nonzero(_extern __pop_appcons_normal:data!(int))
];

lconstant macro XVED_TRY_INPUT = [
    if testdef vedprocess_try_input then
        weakref vedprocess_try_input()
    endif
];

define Xpt_pause();
    ;;; if can't handle it return false to do ordinary wait
    returnunless(WAIT_APPCONS_ACTIVE) (false);

    ;;; process any XVed input before going into a wait
    ;;; N.B. essential no interrupts are serviced between doing this and
    ;;; going into XptPause -- otherwise XVed events could be left
    ;;; unprocessed.
    XVED_TRY_INPUT;

    ;;; pause for one sequence of events or an interrupt
    X_cb_apply(_0, _extern XptPause) -> ;
    true
enddefine;


#_IF DEF UNIX

lconstant arg_exptr = writeable consexternal_ptr();

    /*  Called by sysread on an interactive device
    */
define Xpt_read_wait(_fd);
    lvars _fd;
    ;;; if can't handle it return false and let ordinary read do the wait
    returnunless(WAIT_APPCONS_ACTIVE) (false);

    ;;; process any XVed input before going into a wait
    ;;; N.B. essential no interrupts are serviced between doing this and
    ;;; going into XptIOWait -- otherwise XVed events could be left
    ;;; unprocessed.
    XVED_TRY_INPUT;

    lstackmem wait_fdesc_sets _fdsets;
    lvars _rdset = _fdsets@WFDS_RD_SET;

    _fd _add _1 -> _rdset!FDS_NFDS;
    _fd -> _rdset!FDS_MINFD;
    FD_ZERO(_rdset@FDS_FDSET);
    FD_SET(_fd, _rdset@FDS_FDSET);

    _0 -> _fdsets@WFDS_WR_SET!FDS_NFDS;
    _0 -> _fdsets@WFDS_EX_SET!FDS_NFDS;

    _fdsets -> arg_exptr!XP_PTR;
    ;;; returns number of devices ready (i.e. 1), or -1 if interrupted
    _pint(X_cb_apply(arg_exptr, _1, _extern XptIOWait))
enddefine;

    /*  Called by sys_device_wait
    */
define Xpt_IO_wait(_fdsets);
    lvars _fdsets;
    ;;; if can't handle it return false and let ordinary select do the wait
    returnunless(WAIT_APPCONS_ACTIVE) (false);

    ;;; relativise this lstackmem pointer just in case of process suspension
    @@(csword){_fdsets, _sp()} -> _fdsets;

    ;;; process any XVed input before going into a wait
    ;;; N.B. essential no interrupts are serviced between doing this and
    ;;; going into XptIOWait -- otherwise XVed events could be left
    ;;; unprocessed.
    XVED_TRY_INPUT;

    _sp()@(csword){_fdsets} -> arg_exptr!XP_PTR;
    ;;; returns number of devices ready, or -1 if interrupted
    _pint(X_cb_apply(arg_exptr, _1, _extern XptIOWait))
enddefine;

#_ELSEIF DEF VMS

define Xpt_read_wait(_efnum);
    lvars _efnum;               ;;; event flag number
    ;;; if can't handle it return false and let ordinary read do the wait
    returnunless(WAIT_APPCONS_ACTIVE) (false);

    ;;; process any XVed input before going into a wait
    ;;; N.B. essential no interrupts are serviced between doing this and
    ;;; going into XptReadWait -- otherwise XVed events could be left
    ;;; unprocessed.
    XVED_TRY_INPUT;

    ;;; returns number of devices ready (i.e. 1), or -1 if interrupted
    _pint(X_cb_apply(_pint(_efnum), _1, _extern XptReadWait))
enddefine;

#_ENDIF

    ;;; Force return from XptPause or XptReadWait
define XptSetXtWakeup();
    _extern _pop_set_xt_wakeup(_1) ->
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 13 1994
        For Unix, added Xpt_IO_wait and changed Xpt_read_wait to use
        _extern XptIOWait as well (VMS code essentially unchanged)
--- John Gibson, Mar 21 1994
        Added optional boolean arg to fast_XptAppTryEvents to say return
        boolean 'event processed' result
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Dec  8 1992
        Changed Xpt_read_wait so that it returns 0 for interrupt in Unix
        -- therefore the same for both Unix and VMS.
--- John Gibson, Dec  5 1992
        Rewrote VMS Xpt_read_wait
--- Adrian Howard, Sep 17 1992
        Fixed fast_XtIsSensitive
--- John Gibson, Jun 29 1992
        Removed (fast_)XptInt*erruptHandler (no longer supported)
--- John Gibson, Dec  2 1991
        Moved XVED_TRY_INPUT in -Xpt_read_wait- and -Xpt_pause- to BEFORE
        going into wait state (ensures that no XVed events are left
        outstanding before hanging up).
--- John Gibson, Dec  1 1991
        Changed VMS Xpt_read_wait to take _statbuf as arg
--- Adrian Howard, Oct  3 1991 : Added event handler routines.
--- John Gibson, Aug 29 1991
        Renamed Sys$-Ved$-Xved_try_input as vedprocess_try_input
--- John Gibson, Apr 15 1991
        Added #_IFs in (fast_)XptInterruptHandler and call of
        Clear_async_appcons in XtAppMainLoop.
--- John Gibson, Apr  9 1991
        Added XVED_TRY_INPUT calls, and -XptSetXtWakeup-.
--- John Gibson, Mar 14 1991
        Added -XptAsyncAppContext-. Revised definitions of -Xpt_read_wait-.
        Also made fast_ XtAppNextEvent, XtAppProcessEvent, XtDispatchEvent
        and XtAppMainLoop call Xpt- routines instead of Xt- ones.
--- Ian Rogers, Feb 20 1991
        Corrected -fast_XtDispatchEvent- so that it now returns a boolean
        cf. BR robdu.1
--- John Gibson, Feb 11 1991
        Split off Xpt_pause from Xpt_read_wait and added VMS version of
        latter.
--- Roger Evans, Feb  6 1991 added check for no displays in Xpt_read_wait
--- John Gibson, Jan 30 1991
        Added test for _in_X_call in Xpt_read_wait
--- Roger Evans, Jan 26 1991 added check to XptInterruptHandler
        corrected fast_XtAppPending to return integer result!!
--- John Gibson, Jan 24 1991
        Corrected test in -fast_XtAppPending- from == _:XtTrue to _nonzero
--- John Gibson, Jan 19 1991
        Removed timeout argument from -Xpt_read_wait-.
        Moved tests for _in_X_call into -fast_XptAppTryEvents-.
        Changed X_apply to X_cb_apply to allow async callback.
--- Roger Evans, Dec  7 1990 removed use of XptDisplayAppContext
--- Roger Evans, Nov  9 1990 added Xpt_read_wait
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jul  4 1990 changed to use X_cb_apply
--- Roger Evans, Jan 30 1990
    re-arranged callback vs CHECKINTERRUPT calls
--- Ian Rogers, Dec 15 1989
    Changed _int() to _:
 */
