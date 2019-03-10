/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sys_timer.p
 > Purpose:
 > Author:          John Gibson, Dec 24 1990 (see revisions)
 > Documentation:   REF *TIMES
 */

;;; ------------------------ TIMERS ---------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'signals.ph'

global constant
        procedure (sys_raise_ast)
    ;

global vars
        procedure (timer_interrupt)
    ;

section $-Sys;

constant
        procedure (Check_integral),
        Ast_Dont_Queue, _free_clos_tab
    ;

weak global constant
        procedure Xt$-Xpt_pause
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys => sys_timer, syshibernate, syssleep, syssettimer, syscantimer;

lconstant macro (
    N_TIMENTS   = 32,

    ;;; flags to _extern pop_timer
    _TF_VIRT    = _2:1e0,
    _TF_NOCANC  = _2:1e1,
    _TF_ABS     = _2:1e2,
    _TF_REPEAT  = _2:1e3,
    );

vars                                ;;; so not restored
    _timer_setup_done = false;

lconstant
    _timertab   = (writeable initv(N_TIMENTS))@V_WORDS,
    _timerlim   = _timertab@(w)[_int(N_TIMENTS)],

    _timerflags = (writeable inits(N_TIMENTS))@V_BYTES,
    ;

lvars
    _free_next,
    ;


define :inline lconstant _INDEX(_t);
    ##(w){_t, _timertab}
enddefine;


    /*  Also called from signals.p
    */
define Get_timer_trap(_t);
    lvars _t;
    returnif(issimple(_t!(w))) (false);     ;;; safety check

    ;;; return the trap procedure (could be false if cancelled)
    _t!(w);

    ;;; then free the entry (unless repeating)
    unless _timerflags!(b)[_INDEX(_t)] _bitst _TF_REPEAT then
        _pint(##(w){_free_next, _timertab}++) -> _t!(w);
        _t -> _free_next
    endunless
enddefine;

define lconstant Get_tim(astp, _flags, _tvp);
    lvars astp, _t = _timertab, _flags, _tvp, _i;
    while _t <@(w) _timerlim do
        if (_t!(w)++ -> _t) == astp then
            _t--@(w) -> _t;
            _INDEX(_t) -> _i;
            _timerflags!(b)[_i] _biset _flags -> _flags;
            ;;; get anything at low level
            if _nonneg(_extern pop_timer(_flags, _t, _NULL, _tvp)) then
                unless _flags _bitst _TF_NOCANC then
                    ;;; free the entry
                    _0 -> _timerflags!(b)[_i];
                    Get_timer_trap(_t) ->
                endunless
            else
                ;;; no low-level entry -- must have expired but handler
                ;;; not yet run
                if _tvp /== _NULL then
                    _0 ->> _tvp!TIM_SEC -> _tvp!TIM_USEC
                endif;
                unless _flags _bitst _TF_NOCANC then
                    false -> _t!(w);
                    _0 -> _timerflags!(b)[_i];
                    _CHECKINTERRUPT;    ;;; this should free the entry ...
                    ;;; ... but in case it doesn't
                    unless _t!(w) then Get_timer_trap(_t) -> endunless
                endunless
            endif;
            return(true)
        endif
    endwhile;
    false
enddefine;

    /*  Clear all timers, or just those with ASTP_ERROR_DELETE set.
    */
define Clear_timers(clear_all);
    lvars astp, clear_all, _t = _timertab;
    while _t <@(w) _timerlim do
        _t!(w)++ -> (astp, _t);
        nextunless(iscompound(astp));
        if clear_all
        or (ispair(astp) and fast_back(astp) &&/=_0 ASTP_ERROR_DELETE) then
            Get_tim(astp, _0, _NULL) ->
        endif
    endwhile
enddefine;

define sys_timer(astp);
    lvars usec, astp, _t, _sec, _usec, _flags = _TF_NOCANC, _f;
    lstackmem struct TIMEVAL _tvp;

    if isinteger(astp) then
        ;;; optional flags arg
        ((), astp) -> (astp, _f);
        _int(_f) -> _f;
        if _f _bitst _:TIMER_CANCEL then
            _flags _biclear _TF_NOCANC -> _flags
        endif
    endif;
    Check_astp_arg(astp);

    returnunless(_timer_setup_done) (false);

    if Get_tim(astp, _flags, _tvp) then
        ;;; return time remaining
        _pint(_tvp!TIM_SEC)*1e6 + _pint(_tvp!TIM_USEC)
    else
        false
    endif
enddefine;

define updaterof sys_timer(usec, astp);
    lvars usec, astp, _t, _sec, _usec, _flags = _0, _f;
    lstackmem struct TIMEVAL _tvp;

    unless _timer_setup_done then
        ;;; set up free chain
        _timertab ->> _t -> _free_next;
        while _t <@(w) _timerlim do
            _pint(##(w){_t, _timertab} _add _2) -> _t!(w)++ -> _t
        endwhile;
        0 -> _t!(w)[_-1];
        true -> _timer_setup_done
    endunless;

    if isinteger(astp) then
        ;;; optional flags arg
        ((), usec, astp) -> (usec, astp, _f);
        _int(_f) -> _f;
        if _f _bitst _:TIMER_VIRTUAL then _TF_VIRT -> _flags endif;
        if _f _bitst _:TIMER_REPEAT then _flags _biset _TF_REPEAT -> _flags endif
    endif;
    Check_astp_arg(astp);

    ;;; cancel any existing timer for astp
    Get_tim(astp, _0, _NULL) -> ;
    returnunless(usec);     ;;; if cancelling only

    if isinteger(usec) and usec fi_>= 0 then
        _0 -> _sec;
        _int(usec) -> _usec;
        if _usec _sgreq _1e6 then _usec _div _1e6 -> (_usec, _sec) endif
    else
        Check_integral(usec);
        if usec > 0 then
            _int(usec // 1e6 -> _sec) -> _usec;
            Check_integer(_sec, 0);
            _int(_sec) -> _sec
        else
            mishap(usec, 1, 'sys_timer: INVALID TIME INTERVAL')
        endif
    endif;
    _sec  -> _tvp!TIM_SEC; _usec -> _tvp!TIM_USEC;

    if (_free_next ->> _t) <@(w) _timertab
    or _neg(_extern pop_timer(_flags, _t, _extern _pop_timer_trap, _tvp))
    then
        mishap(0, 'sys_timer: NO SPACE LEFT FOR TIMERS')
    else
        ;;; timer running -- remove from free chain
        _timertab@(w)[_int(_t!(w)) _sub _1] -> _free_next;
        ;;; set handler for when it expires
        astp -> _t!(w);
        _flags -> _timerflags!(b)[_INDEX(_t)]
    endif
enddefine;


;;; --- SLEEPING ---------------------------------------------------------

    /*  Sleep until interrupted
    */
define syshibernate();
    unless testdef Xt$-Xpt_pause and weakref Xt$-Xpt_pause() then
        _extern pause_popintr() ->
    endunless;
    _CHECKINTERRUPT
enddefine;

define Timed_wait_apply(usecs, p);
    lvars tpair, tclos, usecs, procedure p;

    dlocal 0 % if dlocal_context == 1 then false -> tpair endif,
               if dlocal_context fi_<= 2 and tpair then
                    false -> sys_timer(tpair);
                    _free_pairs -> fast_back(tpair);
                            tpair -> _free_pairs;
                    _free_clos_tab!(w)[_1] -> tclos!PD_PROPS;
                            tclos -> _free_clos_tab!(w)[_1]
               endif
             %;

    procedure(tclos);
        lvars tclos;
        false -> fast_frozval(1, tclos)
    endprocedure(%0%) ->> tclos -> fast_frozval(1, tclos);
    conspair(tclos, ASTP_BLOCK_NEVER) -> tpair;

    usecs -> sys_timer(tpair);
    p(tclos)
enddefine;

    /*  Sleep for a specified time
    */
define syssleep(/*hsecs*/) with_nargs 1;
    Timed_wait_apply(() * 1e4,
                procedure(tclos);
                    lvars tclos;
                    while fast_frozval(1, tclos) do syshibernate() endwhile
                endprocedure)
enddefine;


;;; --- OLD TIMER --------------------------------------------------------

define lconstant Raise_Sigalrm = sys_raise_ast(% SIG_ALRM %) enddefine;

define syssettimer(hsecs);
    lvars hsecs;
    if isprocedure(hsecs) then
        ((), hsecs) -> (hsecs, timer_interrupt)
    endif;
    hsecs * 1e4 -> sys_timer(Raise_Sigalrm)
enddefine;

define syscantimer = updater(sys_timer)(% false, Raise_Sigalrm %) enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 20 1996
        Replaced _free_1*closures with _free_clos_tab[_1]
--- John Gibson, Aug 26 1995
        Added Clear_timers
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs and free*1closures -> _free_1*closures
--- John Gibson, Jul  7 1994
        Uses lstackmem
--- John Gibson, Apr 13 1994
        Allowed trap_p to sys_timer to be an AST handler arg (pair
        or procedure); changed syssleep to use the ASTP_BLOCK_NEVER flag.
--- John Gibson, Apr  8 1992
        -sys_timer- now allows a repeating timer.
        Also made -Timed_wait_apply- add closure to -free*1closures- after use
--- John Gibson, Feb 18 1991
        Corrected bug in biginteger case
--- John Gibson, Feb 11 1991
        Now uses -Xpt_pause-
--- John Gibson, Jan 15 1991
        -syshibernate- now uses -Xpt_read_wait-
--- John Gibson, Jan  5 1991
        Added -sys_timer- in new file together with commoned VMS and Unix
        systimer.p
 */
