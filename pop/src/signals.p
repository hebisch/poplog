/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/signals.p
 > Purpose:         Signal handling utilities
 > Author:          Roger Evans, Dec 17 1987 (see revisions)
 > Documentation:   REF SIGNALS
 > Related Files:   asignals.s signals.ph sigtab.p
 */


;;;------------------- SIGNAL HANDLING ------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'signals.ph'
#_INCLUDE 'ident.ph'
#_IF DEF UNIX
#_INCLUDE 'unixdefs.ph'
#_ELSEIF DEF VMS
#_INCLUDE 'vmsdefs.ph'
#_ELSEIF DEF WIN32
#_INCLUDE 'win32defs.ph'
#_ENDIF

constant
        procedure (set_process_entry_term, sys_grbg_destpair,
        sys_grbg_closure)
    ;

section $-Sys;

constant
        procedure (System_error, Callstack_reset, Zap_nextfree_save,
        Get_timer_trap, Clear_timers, Io$-Get_devio_trap)
    ;

weak constant
        procedure (Get_os_process_trap, Get_handle_trap,)
    ;

endsection;


;;; -------------------------------------------------------------------

section $-Sys =>    sys_signal_flag, pop_asts_enabled, sys_reset_signal,
                    sys_raise_ast, sys_signal_handler;

constant
    Sys_ast_queue   = writeable conspair(false,[]),

    ;;; used for the pdprops of an ast procedure that's not to be queued
    Ast_Dont_Queue      = '',
    ;

vars
    Sys_ast_queue_end   = Sys_ast_queue,
    _ignore_ast_queue       = true,
    _ast_queue_flags        = _0,
    ;

lvars
    _asts_enabled           = true
    ;

    /*  Struct for signal details in _extern __pop_sigcontext,
        set for error signals by _extern _pop_errsig_handler
    */
struct
  { int     PSC_SIG,
            PSC_CODE;
    <code>  PSC_PC;             ;;; <type> means pointer field of C size
    <byte>  PSC_ADDR;
    { long  PSC_SIGARRAY[];     ;;; VMS
    | long  PSC_RW_FLAG;        ;;; WIN32
    }
  };


;;; --- ERROR SIGNALS --------------------------------------------------------

#_IF DEF VMS

    ;;; Signal numbers used in c_core.c
define_extern_name
    __SIG_INT   = _:SIG_INT,
    __SIG_ALRM  = _:SIG_ALRM,
    __SIG_CHLD  = _:SIG_CHLD,
    __SIG_IO    = _:SIG_IO,
    ;


    /*  Called for conditions (error signals) by _extern _pop_errsig_handler
        Signal details are in _extern __pop_sigcontext
    */
define Error_signal();
    lvars _psc = _extern __pop_sigcontext:data, _sig = _psc!PSC_SIG;

    if _sig == _:'SS$_DEBUG' then
        ;;; restarted after Ctrl-Y -- do a setpop
        _DISABLE_INTERRUPTS -> _disable;
        setpop()
    else
        ;;; produce mishap after cleaning up callstack -- see errors.p
        chain(_sig, _psc!PSC_CODE, _psc!PSC_PC, _psc!PSC_ADDR,
                        _psc@PSC_SIGARRAY, System_error, Callstack_reset)
    endif
enddefine;


#_ELSEIF DEF UNIX


    /*  Called for error signals by _extern _pop_errsig_handler
        Signal details are in _extern __pop_sigcontext
    */
define Error_signal();
    lvars n, _psc = _extern __pop_sigcontext:data, _sig = _psc!PSC_SIG;

    ;;; clear signal mask in case any have been left blocked
    _extern _pop_sigmask(_0) -> ;

    if _sig /== _:SIG_QUIT then
        ;;; produce mishap after cleaning up callstack -- see errors.p
        chain(_sig, _psc!PSC_CODE, _psc!PSC_PC, _psc!PSC_ADDR,
                                    System_error, Callstack_reset)
    endif;

    ;;; Handle SIG_QUIT. If a further QUIT arrives within one second then
    ;;; exit, otherwise calls setpop.

    define lconstant quit_timerp = identfn(%%) enddefine;

    if sys_timer(quit_timerp) then
        ;;; this is the second quit, so just quit
        ;;; if standard input is a terminal, reset it to process-entry state
        set_process_entry_term();
        _extern _exit(_1)
    else
        ;;; not already waiting after a quit signal
        ;;; set wait for 1 second to see if another quit happens
        1e6 -> sys_timer(quit_timerp);  ;;; set timer for 1 second
        repeat
            _extern pause() -> ;        ;;; go to sleep
            sys_timer(quit_timerp) -> n;
            quitunless(n and n /== 0)
        endrepeat;

        ;;; comes here if we didn't get another quit
        false -> sys_timer(quit_timerp);
        ;;; disable high level handling of signals (to ensure no
        ;;; blocked signal clobbers the setpop), then unblock them
        _DISABLE_INTERRUPTS -> _disable;
        setpop()
    endif
enddefine;

lconstant
    error_sig = [% SIG_ILL, SIG_IOT, SIG_EMT, SIG_FPE, SIG_BUS, SIG_SEGV %],
    indep_sig = [% SIG_ALRM, SIG_VTALRM, SIG_IO %],
;

    /*  Set Unix signal handling to reflect table value
    */
define lconstant Set_signal(sig);
    lvars sig, p, _handler;

    ;;; these sigs have independent handlers
    returnif(fast_lmember(sig, indep_sig));

    if fast_lmember(sig, error_sig) then
        _extern _pop_errsig_handler
    elseif sig /== SIG_CHILD
    and isboolean(fast_subscrv(sig,Sys_signal_procs) ->> p) then
        if p then _:SIG_DFL else _:SIG_IGN endif
    elseif sig == SIG_QUIT then
        ;;; handled as errsig unless being ignored
        _extern _pop_errsig_handler
    else
        ;;; passed thru to user level
        _extern _pop_usersig_handler
    endif -> _handler;
    _extern _pop_sigaction(_int(sig), _handler) ->
enddefine;

    ;;; reset poplog signal-handling
define sys_reset_signal();
    lvars n;
    fast_for n to MAXSIG do Set_signal(n) endfor
enddefine;

    /*  Called at the beginning of Setup_system
    */
define Init_signals();
    lvars sig;
    _extern _pop_sigaction(_:SIG_QUIT, _extern _exit) -> ;
    fast_for sig in indep_sig do
        _extern _pop_sigaction(_int(sig), _:SIG_IGN) ->
    endfor
enddefine;

#_ELSEIF DEF WIN32

    /*  Called for all continuable exceptions through exception_filter in
        "exception.c"; exception details are in _extern __pop_sigcontext
    */
define Error_signal();
    lvars _psc = _extern __pop_sigcontext:data;
    ;;; produce mishap after cleaning up callstack -- see errors.p
    chain(_psc!PSC_SIG, _psc!PSC_CODE, _psc!PSC_PC, _psc!PSC_ADDR,
        _psc!PSC_RW_FLAG, System_error, Callstack_reset);
enddefine;

#_ELSE_ERROR
#_ENDIF


;;; --- UTILITY PROCEDURES ----------------------------------------------

define :inline lconstant IS_SIGNAL(s);
    (isinteger(s) and 0 fi_< s and s fi_<= Sys_max_signal)
enddefine;

    /* used by user-access routines
    */
define lconstant Check_signal(_sig);
    lvars _sig;
    unless IS_SIGNAL(_sig) then
        mishap(_sig, 1, 'INVALID SIGNAL')
    endunless;
enddefine;


;;; --- SIGNAL ENABLING/BLOCKING ------------------------------------------

lconstant procedure Do_ast_queue;

define lconstant Sig_flag(sig, _raise_local);
    lvars sig, _s, _last, _v, _raise_local;
    if isvector(sig) then
        ;;; value(s) required for signal set -- return single
        ;;; value if all the same
        0 -> _last;
        fast_for _s in_vector sig do
            Check_signal(_s);
            fast_subscrv(_s,Sys_signal_flags) -> _v;
            if _last == 0 then
                _v -> _last
            elseif _v /== _last then
                1 -> _last
            endif
        endfor;
        if _last == 0 then
            ;;; vec was empty
            []
        elseif _last /== 1 then
            ;;; all the same
            _last
        else
            ;;; not all the same -- return list
            [%  fast_for _s in_vector sig do
                    fast_subscrv(_s,Sys_signal_flags)
                endfor
            %]
        endif
    else
        Check_signal(sig);
        fast_subscrv(sig,Sys_signal_flags)
    endif
enddefine;
;;;
define updaterof Sig_flag(val, sig, _raise_local);
    lvars sig, val, _s, _v, _raise_local;

    define lconstant Check_val(_v);
        lvars _v;
        if _v == 1 then
            true -> _v  ;;; for upward compatibility
        elseunless isboolean(_v) or isvector(_v) then
            mishap(_v, 1, 'INVALID VALUE FOR sys_signal_flag')
        endif;
        ;;; if enabling any signal, clear flag to check queue
        if _v then false -> _ignore_ast_queue endif
    enddefine;

    if islist(val) then
        ;;; multiple values for sig set
        unless _raise_local
        or (isvector(sig) and listlength(val) == datalength(sig)) then
            mishap(val, sig, 2, 'INVALID ARGUMENTS FOR sys_signal_flag');
        endunless;
        fast_for _s in_vector sig do
            Check_signal(_s);
            Check_val(if _raise_local then
                        sys_grbg_destpair(val)
                      else
                        fast_destpair(val)
                      endif -> val ->> _v);
            _v -> fast_subscrv(_s,Sys_signal_flags)
        endfor
    else
        ;;; single value
        Check_val(val);
        if isvector(sig) then
            ;;; same val for sig set
            fast_for _s in_vector sig do
                Check_signal(_s);
                val -> fast_subscrv(_s,Sys_signal_flags)
            endfor
        else
            Check_signal(sig);
            val -> fast_subscrv(sig,Sys_signal_flags)
        endif
    endif
enddefine;

define sys_signal_flag(/*sig*/) with_nargs 1;
    Sig_flag(false)
enddefine;
;;;
define updaterof sys_signal_flag(/*val, sig*/) with_nargs 2;
    _CHECKINTERRUPT;    ;;; handle outstanding async signals before update
    () -> Sig_flag(false);
    unless _ignore_ast_queue then chain(false, Do_ast_queue) endunless
enddefine;

define active pop_asts_enabled;
    _asts_enabled
enddefine;
;;;
define updaterof active pop_asts_enabled with_nargs 1;
    if () ->> _asts_enabled then chain(false, Do_ast_queue) endif
enddefine;


;;; --- RAISING ASTS/SIGNALS -----------------------------------------------

lvars
    ast_executing_list = [];    ;;; list of executing ast procedures

define lconstant Remove(lptr);
    lvars lptr, ptr = fast_back(lptr);
    ;;; remove entry from queue and return pair to free list
    if ptr == Sys_ast_queue_end then lptr -> Sys_ast_queue_end endif;
    sys_grbg_destpair(ptr) -> (, fast_back(lptr))
enddefine;

define lconstant Add(/*ast*/);
    conspair((), []) ->> fast_back(Sys_ast_queue_end)
                      -> Sys_ast_queue_end
enddefine;

define lconstant Raise_astp(p, lptr);
    lvars   procedure p, pair = false, lptr, pdpt = p,
            _flags = _:ASTP_BLOCK_RECURSIVE, _slen;
    dlocal  ast_executing_list, cucharout;

    if ispair(p) then
        p -> pair;
        fast_destpair(p) -> (p, _flags);
        p -> pdpt;
        _int(_flags) -> _flags;
        if _flags _bitst _:ASTP_TEMP_CLOSURE then
            p!PD_CLOS_PDPART -> pdpt
        endif
    endif;

    unless _flags _bitst _:ASTP_BLOCK_NEVER then
        ;;; test for blocking conditions
        returnunless((lptr or _ignore_ast_queue) and _asts_enabled) (false);

        if _call_stack_seg_hi /== _call_stack_hi
        ;;; and don't run inside external callback ...
        and _flags _bitst _:ASTP_BLOCK_IN_EXTERNAL then
            _ast_queue_flags _biset _:QC_CALLBACK -> _ast_queue_flags;
            ;;; force return from Xt wait state (if running and inside it)
            _extern _pop_set_xt_wakeup(_1) -> ;
            return(false)
        endif;

        if fast_lmember(pdpt, ast_executing_list)
        ;;; and don't run same handler recursively ...
        and _flags _bitst _:ASTP_BLOCK_RECURSIVE then
            _ast_queue_flags _biset _:QC_RECURSIVE -> _ast_queue_flags;
            return(false)
        endif;
    ;;; else never disabled -- just run it
    endunless;

    if lptr then Remove(lptr) endif;    ;;; remove from queue

    ;;; add pdpart to executing list
    if pair and _flags _bitst _:ASTP_TEMP_PAIR then
        ;;; can reuse the pair
        pdpt -> fast_front(pair);
        ast_executing_list -> fast_back(pair)
    else
        conspair(pdpt, ast_executing_list) -> pair
    endif;
    pair -> ast_executing_list;

    ;;; apply procedure -- this must not change the stack
    ;;; set cucharout to standard value in case p does any printing
    ;;; and cucharout is set to something special (like identfn)
    charout -> cucharout;
    _stklength() -> _slen;
    fast_apply(p);
    if _stklength() /== _slen then
        mishap(p, 1, 'USERSTACK HAS CHANGED AFTER ASYNCHRONOUS TRAP PROCEDURE')
    endif;
    sys_grbg_destpair(pair) -> (,);
    if _flags _bitst _:ASTP_TEMP_CLOSURE then
        sys_grbg_closure(p)
    endif;

    if _ast_queue_flags _bitst _:QC_RECURSIVE then
        false -> _ignore_ast_queue
    endif;
    true
enddefine;

define lconstant Raise_sig(sig, lptr);
    lvars   sig, lptr, handler, _sig, _slen;
    dlvars  flag;
    dlocal  cucharout;

    returnunless((lptr or _ignore_ast_queue) and _asts_enabled) (false);

    _int(sig) _sub _1 -> _sig;
    ;;; return if not enabled
    returnunless(Sys_signal_flags!V_WORDS[_sig] ->> flag) (false);

    if lptr then Remove(lptr) endif;    ;;; remove from queue
    Sys_signal_procs!V_WORDS[_sig] -> handler;
    if isprocedure(handler) then
        /* everything is ok */
    elseif isident(handler) then
        fast_idval(handler) -> handler
    else
        /* something else (probably boolean) - treat as identfn */
        return(true)
    endif;

    _stklength() -> _slen;
    ;;; supply an arg if handler expects it
    if pdnargs(handler) == 1 then sig endif;

    if flag == true then sig -> flag endif;     ;;; block self only
    ;;; run handler with appropriate signals blocked -- must not change the
    ;;; stack.
    ;;; set cucharout to standard value in case handler does any printing
    ;;; and cucharout is set to something special (like identfn)
    procedure();
        dlocal cucharout = charout, % Sig_flag(flag, true) % = false;
        fast_apply()
    endprocedure(handler);
    if _stklength() /== _slen then
        mishap(handler, 1, 'USERSTACK HAS CHANGED AFTER SIGNAL PROCEDURE')
    endif;

    true
enddefine;

    /*  Process ast queue -- lptr points to preceding pair
        in queue (used for removing entries from the queue)
        initially it points to the dummy pair at the head of the
        queue. ptr points to the current pair in the queue
    */
define lconstant Do_ast_queue(_async);
    lvars ptr, ast, lptr = Sys_ast_queue, _async;
    returnunless(_asts_enabled);
    _0 -> _ast_queue_flags;
    repeat
        unless _async then _CHECKINTERRUPT endunless;
        quitif((fast_back(lptr) ->> ptr) == []);
        fast_front(ptr) -> ast;
        if isprocedure(ast) or ispair(ast) then
            Raise_astp(ast, lptr)       ;;; leave result on stack
        elseif IS_SIGNAL(ast) then
            Raise_sig(ast, lptr)        ;;; leave result on stack
        else
            Remove(lptr);           ;;; remove from queue
            mishap(ast, 1, 'INVALID ITEM IN AST/SIGNAL QUEUE');
            true                    ;;; in case it returns
        endif;
        if () then
            ;;; handled -- reset pointers to process whole queue again
            ;;; (only sensible thing to do since handler may have
            ;;; arbitrarily processed the queue)
            Sys_ast_queue -> lptr
        else
            ;;; disabled -- move on to next one
            ptr -> lptr
        endif
    endrepeat;

    ;;; set no-check-needed flag
    true -> _ignore_ast_queue
enddefine;

define Check_astp_arg(astp);
    lvars astp, _flags;
    returnif(isprocedure(astp));
    if ispair(astp) then
        fast_destpair(astp) -> (astp, _flags);
        returnif(isprocedure(astp) and isinteger(_flags)
                and (not(_int(_flags) _bitst _:ASTP_TEMP_CLOSURE)
                    or astp!PD_FLAGS _bitst _:M_PD_CLOSURE))
    endif;
    mishap(astp, 1, 'INVALID AST PROCEDURE ARGUMENT')
enddefine;

    /*  User level checking version
    */
define sys_raise_ast(ast);
    lvars ast;
    if ast then
        if isinteger(ast) then
            Check_signal(ast);
            Raise_sig(ast, false)
        else
            Check_astp_arg(ast);
            Raise_astp(ast, false)
        endif;
        unless () then Add(ast) endunless
    endif;
    _CHECKINTERRUPT;
    unless _ignore_ast_queue then Do_ast_queue(false) endunless
enddefine;


;;; ----------------------------------------------------------------------

    /*  Raising an ASynchronous Trap (called from _checkall)
    */
define Async_raise_signal();
    lvars ast, _ioset, _s1, _s2;
    dlvars _type, _data;

    dlocal 0 % SAVE_ERRNO(_s1,_s2), RESTORE_ERRNO(_s1,_s2) %;

    until _zero(_extern _pop_rem_ast(ident _type, ident _data)) do
        Zap_nextfree_save();
        if _type == _:AST_TIMER then
            ;;; data is timer ident
            ;;; Get timer trap procedure
            if Get_timer_trap(_data) ->> ast then
                Raise_astp(ast, false)
            else
                true    ;;; else ignore
            endif
        elseif _type == _:AST_APP_PENDING then
            ;;; Xt appcontext events pending -- data is a fixed-address
            ;;; closure that handles the trap
            Raise_astp(_data ->> ast, false)
        elseif _type == _:AST_SIGNAL then
            ;;; data is signal number
            if _data == _:SIG_CHILD then
                if testdef Get_os_process_trap
                and weakref Get_os_process_trap() ->> ast then
                    Raise_astp(ast, false)
                else
                    true
                endif
            else
                ;;; check needed for upward compatibility
                if _data == _:SIG_INT then `\n` -> weakref poplastchar endif;
                Raise_sig(_pint(_data) ->> ast, false)
            endif
        elseif _type == _:AST_HANDLE and testdef Get_handle_trap then
            if weakref Get_handle_trap(_data) ->> ast then
                Raise_astp(ast, false)
            else
                true    ;;; else ignore
            endif;
        elseif _type == _:AST_QUEUE_CHECK then
            ;;; check queue
            false -> _ignore_ast_queue;
            true
        elseif (_type _sub _:AST_DEV ->> _ioset) _sgreq _0
        and _ioset _slteq _2 then
            ;;; input/output waiting -- data is fd/unit number.
            ;;; Get trap procedure to run from corresponding device
            if Io$-Get_devio_trap(_data, _ioset) ->> ast then
                Raise_astp(ast, false)
            else
                true    ;;; else ignore
            endif
        else
            mishap(_pint(_type), 1, 'UNKNOWN AST TYPE')
        endif;
        unless () then Add(ast) endunless;
        unless _ignore_ast_queue then Do_ast_queue(true) endunless
    enduntil
enddefine;

    /*  Clear all queued asts
    */
define Clear_ast_queue(clear_all);
    lvars clear_all, ptr, ast, lptr;
    dlvars _dummy;
    if clear_all then
        until _zero(_extern _pop_rem_ast(ident _dummy, ident _dummy)) do
        enduntil;
        Sys_ast_queue -> Sys_ast_queue_end;
        true
    else
        ;;; clear only ASTs with ASTP_ERROR_DELETE set
        dlocal _asts_enabled = false;
        Async_raise_signal();       ;;; get everything in queue
        Sys_ast_queue -> lptr;
        until (fast_back(lptr) ->> ptr) == [] do
            fast_front(ptr) -> ast;
            if ispair(ast) and fast_back(ast) &&/=_0 ASTP_ERROR_DELETE then
                Remove(lptr)
            else
                ptr -> lptr
            endif
        enduntil;
        Clear_timers(false);    ;;; clear timers with ASTP_ERROR_DELETE set
        if Sys_ast_queue_end /== Sys_ast_queue then
            ;;; so queue will get checked at next interrupt checkpoint
            _extern _pop_add_ast(_:AST_QUEUE_CHECK, _0) -> ;
            false
        else
            true
        endif
    endif -> _ignore_ast_queue
enddefine;


;;; --- SIGNAL HANDLERS ------------------------------------------------

define sys_signal_handler(sig);
    lvars sig;
    Check_signal(sig);
    fast_subscrv(sig,Sys_signal_procs)
enddefine;

define updaterof sys_signal_handler(p, sig);
    lvars p, sig;
    Check_signal(sig);
    unless isboolean(p) then
        unless isprocedure(p)
        or (iscompound(p) and p!KEY == ident_key
            and p!ID_IDENTPROPS _bitst _:M_ID_PROCEDURE)
        then
            mishap(sig,p,2,'IMPERMISSIBLE HANDLER VALUE FOR SIGNAL');
        endunless;
    endunless;

    ;;; handle outstanding async signals before update
    _CHECKINTERRUPT;
    p -> fast_subscrv(sig,Sys_signal_procs);
#_IF DEF UNIX
    Set_signal(sig);
#_ENDIF
enddefine;

endsection;     ;;; $-Sys


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1996
        Fixed pathetically silly bug in Check_astp_arg (was using _bitst
        on a pop integer).
--- John Gibson, Aug 30 1996
        Made both Raise_astp and Raise_sig check that handler procedure
        does not change the stack; also both set cucharout to charout
        while running the handler.
--- John Gibson, Aug 20 1996
        Made Raise_astp reuse its pair arg if ASTP_TEMP_PAIR is set in
        the flags, and garbage its closure arg if ASTP_TEMP_CLOSURE set.
--- Robert Duncan, May 24 1996
        Added processing of AST_HANDLE traps for Win32
--- John Gibson, Aug 26 1995
        Added call to Clear_timers in Clear_ast_queue
--- Robert John Duncan, Sep  5 1994
        Added definition of Error_signal for Win32
--- John Gibson, May 17 1994
        Fixed handling of Unix SIG_QUIT
--- John Gibson, May  5 1994
        Added processing for AST_DEV traps
--- John Gibson, Apr 22 1994
        sys*_raise_signal    -> sys_raise_ast
        sys*_signals_enabled -> pop_asts_enabled
        sys*_signal_queue    -> pop_ast_queue
--- John Gibson, Apr 19 1994
        Added dedicated handling code for SIG_CHILD in Async_raise_signal
--- John Gibson, Apr 13 1994
        Various changes to allow AST procedure args to be a pair of the form
        conspair(p, flags) where bits in flags specify block conditions etc.
--- John Gibson, Jan 31 1994
        Reorganised ast code, and added Ast_Dont_Queue for use by syssleep
--- John Gibson, Aug 19 1992
        Changed sys*_raise_signal to allow it to take a trap procedure
        as argument
--- John Gibson, Mar 14 1991
        Added AST_APP_PENDING to -Async_raise_signal-
--- John Gibson, Feb  9 1991
        Added VMS -Error_signal-
--- John Gibson, Jan 15 1991
        Changed -Set_signal- to leave SIG_IO alone.
--- John Gibson, Jan  5 1991
        Changed -Set_signal- to leave timer sigs alone, -Error_signal-
        to use new -sys_timer-, and -Async_raise_signal- to service timer
        ASTs.
--- John Gibson, Jan  3 1991
        Now uses _extern _pop_sigaction to set signal handlers, etc.
--- John Gibson, Dec 29 1990
        Replaced old Unix QUIT procedure with -Error_signal-.
--- John Gibson, Dec  3 1990
        Rewritten to allow arbitrary trap procedures in the signal queue.
        -Async_raise_signal- now handles different ast types (of which
        signal numbers are just one).
--- John Gibson, Sep 21 1990
        Combined with sig_flag.p and sig_handler.p (which are now removed);
        rewrote various things so that values for sys_signal_flag can be
        false, true or a vector of sigs to block while executing handler,
        etc.
--- John Gibson, Sep 10 1990
        Changed test in -Raise_sig- to == SIG_RE_ENTRANT, i.e. 1 (and
        disable recursive invocation of signal handler if not, which is now
        the default given by true).
--- John Gibson, Aug 21 1990
        Subroutine _r*emsig now replaced with C routine _pop_rem_sig
--- Rob Duncan, Jun 28 1990
        Removed special handling of SIG_CHILD in -Set_signal-: the handler
        is set to SIG_DFL by default in "sigtab.p", and localised
        appropriately by -sys*wait-, so it shouldn't matter if it's changed
        elsewhere.
--- Simon Nichols, Jun 18 1990
        Added test for RISC/os in -Set_signal-, to ensure SIG_CHILD handler
        is always set to SIG_DFL.
--- John Gibson, Aug 25 1989
        Replaced all sysint SIG_ macros with _: on pop ones.
--- John Gibson, Aug 24 1989
        Removed old non-S*IGNALS code
--- John Gibson, Aug 22 1989
        Bobcat now tested for as HPUX.
--- Roger Evans, Oct  7 1988
        Raising signals with boolean values now treats them like identfn,
        instead of mishapping
--- Roger Evans, Sep 14 1988
        For BOBCAT, forced -SIG_CHILD to ALWAYS be SIG_DFL (ie never assign
        user handler either)
--- John Gibson, Aug 25 1988
        For BOBCAT, -Set_signal- never assigns SIG_IGN for SIG_CHILD
        (it screws up wait system calls, etc).
--- John Gibson, Apr 29 1988
        Put remaining stuff (including non-SIGNALS code) into Sys.
--- Roger Evans, Apr 18 1988
        moved optional user access procedures to sig_*.p
        moved code into section Sys
--- Roger Evans, Apr  8 1988
        Added flag _ignore_ast_queue to avoid redundant checks of signal
        queue when nothing has changed since last check
--- John Gibson, Mar 21 1988
        Revised updater of -sys_max_signal- so as not to use
        -move_subvector-, -fill-, etc.
        Also replaced use of -identtype-.
--- John Gibson, Mar 15 1988
        Weakref'ed -poplastchar-
--- Roger Evans, Feb 23 1988
        (Re-)Installed in system masters
 */
