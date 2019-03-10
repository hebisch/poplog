/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/sysfork.p
 > Purpose:         Fork a new process
 > Author:          John Gibson (see revisions)
 > Documentation:   REF * SYSUTIL
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'signals.ph'
#_INCLUDE 'unixdefs.ph'

constant
        procedure (clear_prop_entries, applist, Sys$-Add_os_process_entry,
        Sys$-Clear_timers),
        _caller_return, Sys$- _do_vfork, sys_process_destroy_action
    ;

vars
        Sys$- _vfork_child, Sys$-os_process_list,
    ;

weak constant
        procedure (
            XptAsyncAppContext,
            Sys$-Xt$-Clear_async_appcons,
            Sys$-Xt$-Kill_displays,
        )
    ;

weak vars
        poppid, XptDefaultAppContext, XptDefaultDisplay,
    ;

;;; -------------------------------------------------------------------

section $-Sys => sys_fork, sys_vfork;

define Waitpid(_pid, _status_id);
    lvars _pid, _status_id, _child_id;
    _extern waitpid(_pid, _status_id, _:WNOHANG) -> _child_id;
    if _child_id _sgr _0 then
        termin
    else
        ;;; false if interrupted, true otherwise
        _child_id == _0 or _ERRNO /== _:EINTR
    endif
enddefine;

#_IF DEF BSD_VFORK
;;; has a useful, working vfork

struct VFRET { int RETOFFS, CSTKLEN; };

lvars
    vf_save_interrupt,
    ;;; allows for upto 4 nested vforks
    _vf_stk = (writeable inits(_pint(##(b)[_4|struct VFRET])))@V_WORDS,
    _vf_n   = _0,
    _vf_tmp,

    _vf_save_sigio,
    _vf_save_sigalrm,
    _vf_save_sigvtalrm,
    _vf_save_sigchild,
    ;

#_ENDIF

define lconstant Set_poppid();
    if testdef poppid then
        Uint_->_pint(_extern getpid()) -> weakref poppid
    endif
enddefine;

define sys_fork(will_do_wait);
    lvars will_do_wait, astp = false, _child_id;

    unless isboolean(will_do_wait) then
        ((), will_do_wait) -> (will_do_wait, astp);
        Check_astp_arg(astp)
    endunless;

    _extern pop_fork() -> _child_id;

    if (_child_id) _eq _-1 then
        Syserr_mishap(0, 'UNABLE TO FORK')
    elseif _zero(_child_id) then
        ;;; the child
#_IF DEF BSD_VFORK
        if _vfork_child then
            ;;; After sys_fork we are no longer inside a vfork child
            false -> _vfork_child;
            _0 -> _vf_n;
            _extern _pop_sigaction(_:SIG_IO, _vf_save_sigio) -> ;
            _extern _pop_sigaction(_:SIG_ALRM, _vf_save_sigalrm) -> ;
            _extern _pop_sigaction(_:SIG_VTALRM, _vf_save_sigvtalrm) -> ;
            _extern _pop_sigaction(_:SIG_CHILD, _vf_save_sigchild) -> ;
            _extern _pop_sigmask(_0) ->
        endif;
#_ENDIF
        [] -> os_process_list;
        identfn -> io_interrupt;
        sysexit -> interrupt;
        if testdef XptAsyncAppContext then
            weakref[XptAsyncAppContext] Xt$-Clear_async_appcons()
        endif;
        Clear_ast_queue(true);
        Clear_timers(true);
        Set_poppid();

        ;;; CLEAR PROCESS SPECIFIC DESTROY PROPERTY
        clear_prop_entries(sys_process_destroy_action);

        ;;; CLEAR DEFAULT X DISPLAY & APPLICATION CONTEXT IF SET
        false -> weakref XptDefaultAppContext;
        false -> weakref XptDefaultDisplay;

        ;;; CLOSE ALL DISPLAYS
        if testdef Xt$-Kill_displays then
            weakref Xt$-Kill_displays();
        endif;

        false           ;;; result

    else
        ;;; the parent
        Uint_->_pint(_child_id) ->> _child_id;      ;;; result
#_IF DEF BSD_VFORK
        returnif(_vfork_child);
#_ENDIF
        Add_os_process_entry(will_do_wait, astp, _child_id)
    endif
enddefine;


#_IF DEF BSD_VFORK

    /*  Parent returns to this procedure after a vfork
    */
define Vfork_parent(_child_id);
    lvars _child_id;

    define lconstant reset_return();
        ;;; set return address back to what it was
        _caller_sp()!SF_OWNER!PD_EXECUTE@(code)
                { _vf_stk@(struct VFRET)[_vf_n]!RETOFFS }
                                                -> _caller_return()
        ;;; then return to there
    enddefine;

    if _zero(_vf_n _sub _1 ->> _vf_n) then
        vf_save_interrupt -> interrupt;
        false -> _vfork_child;
        Add_os_process_entry((/*will_do_wait,astp*/), Uint_->_pint(_child_id));
        _extern _pop_sigmask(_0) ->
    endif;
    Set_poppid();

    ;;; chain reset_return to inside the caller of sys_vfork, which
    ;;; then sets the return into that procedure to be as it was when
    ;;; it called sys_vfork. Pass it child id as argument.
    chainto(Uint_->_pint(_child_id),
            _pint( _vf_stk@(struct VFRET)[_vf_n]!CSTKLEN ),
            reset_return)
enddefine;

    /*  sys_vfork implements the vfork system call as from C,
        i.e. the caller of sys_vfork must NOT exit when the child.

        sys_vfork mustn't have any register lvars, because in the
        parent these will not be restored.
    */
define sys_vfork(/*will_do_wait*/) with_nargs 1;
    unless _vfork_child then
        ;;; leave the args on the stack but check astp
        if isboolean(dup()) then false else Check_astp_arg(dup()) endif;
        ;;; make sure there's plenty of free heap space before doing the
        ;;; FIRST vfork -- if it isn't enough, things will go wrong ...
        Get_store(@@(w)[_8192]) -> Get_store();
        interrupt -> vf_save_interrupt;
        _extern _pop_sigmask(_1) -> ;   ;;; block signals
        _CHECKINTERRUPT                 ;;; then clear signal queue
    endunless;

    _caller_sp_flush() -> _vf_tmp;
    @@(code){_caller_return(), _vf_tmp!SF_OWNER!PD_EXECUTE}
                -> _vf_stk@(struct VFRET)[_vf_n]!RETOFFS;
    ##(csword){_call_stack_hi, _vf_tmp}
                -> _vf_stk@(struct VFRET)[_vf_n]!CSTKLEN;

    if _do_vfork() _eq _-1 then
        unless _vfork_child then _extern _pop_sigmask(_0) -> endunless;
        Syserr_mishap(0, 'UNABLE TO VFORK')
    endif;

    ;;; I am the child -- parent returns to Vfork_parent
    _vf_n _add _1 -> _vf_n;
    unless _vfork_child then
        true -> _vfork_child;
        _extern _pop_sigaction(_:SIG_IO,    _:SIG_IGN) -> _vf_save_sigio;
        _extern _pop_sigaction(_:SIG_ALRM,  _:SIG_IGN) -> _vf_save_sigalrm;
        _extern _pop_sigaction(_:SIG_VTALRM,_:SIG_IGN) -> _vf_save_sigvtalrm;
        _extern _pop_sigaction(_:SIG_CHILD, _:SIG_IGN) -> _vf_save_sigchild;
        sysexit -> interrupt;
        _extern _pop_sigmask(_0) ->
    endunless;

    Set_poppid();
    false
enddefine;

#_ELSE

constant procedure
    sys_vfork = sys_fork;

#_ENDIF

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 26 1995
        Added call to Clear_timers in sys_fork
--- Robert John Duncan, Feb  1 1995
        Simplified closing of X displays in sys_fork by calling new
        Xt$-Kill_displays
--- Robert John Duncan, Sep  5 1994
        Reinstated as a Unix-only file: generic O/S process handling moved
        to new C.all/src/os_process.p, and VM*S-specific code moved back
        to sys_spawn.p
--- John Gibson, Jul  8 1994
        Replaced sys_a*sync_input with sys_async_io
--- John Gibson, Apr 19 1994
        Replaced sys(v)fork with sys_(v)fork taking two arguments.
        Added same two args to new VM*S sys_spawn and incorporated it
        into this file (getting rid of VM*S sysspawn.p).
        (Both sys_(v)fork and sys_spawn now use a common sys_wait.)
--- Adrian Howard, Jul  6 1992
        Moved -kill_X_display- to xt_error.p
--- Adrian Howard, Jul  1 1992
        Fixed bug in closing of X displays on -sys*fork-.
--- Robert John Duncan, Jun 23 1992
        New macro BSD_VFORK now used to determine whether to define a
        special -sys*vfork-. The flag should be set in "sysdefs.p" for those
        systems which want it.
--- Adrian Howard, Jun 17 1992
        Calls -XIO_sys_error_handler- on all active displays in the child
        process after a -sys*fork-. This causes the file descriptor of the
        display to be closed, and the POP-11 structures relating to the
        displays and there associated application contexts to be cleared.
--- Adrian Howard, Jun 15 1992
        Cleared -XptDefaultAppContext- and -XptDefaultDisplay- on -sys*fork-
--- Adrian Howard, Jun  9 1992
        Made sure that all the prop entries in -sys_process_destroy_action-
        were cleared on -sys*fork-.
--- Adrian Howard, Mar 17 1992
        Cleared -sys_process_destroy_action- on -sys*fork- (see REF *PROPS,
        *sys*fork)
--- Robert John Duncan, Nov  7 1991
        Disabled -vfork- for HPUX (no longer works for HP-UX 8.0)
--- Robert John Duncan, Jun 21 1991
        Disabled -vfork- for SG IRIX
--- John Gibson, Mar 18 1991
        Added clearing of async appcontexts in -sys*fork-
--- John Gibson, Jan 27 1991
        sys*fork uses _extern pop_fork instead of _extern fork
--- Robert John Duncan, Jan 10 1991
        Temporarily disabled -vfork- for MIPS Risc/os
--- John Gibson, Jan  3 1991
        Now uses _extern _pop_sigmask to block/unblock sigs, and
        _extern _pop_sigaction to set handlers.
--- Aaron Sloman, Oct  3 1990
        Moved some declarations for vfork up, so that I could alter
        -sys*fork- to do
        false -> _vfork_child;
        _0 -> _vf_n;
--- John Gibson, Aug 28 1990
        Added code to disable async input in -sys*fork-, and disable
        associated signals only in -sys*vfork-. Changed -sys*vfork- to cope
        properly with upto 4 nested invocations.
--- John Gibson, Jan 25 1990
        Initialisation of _vfork_child to false moved to sysio.p
--- John Gibson, Jan 30 1989
        -sys*vfork- was ensuring a block of memory was free before doing the
        vfork system call (so as to prevent a GC happening in the child),
        but it was doing this on a second or subsequent vfork (which was
        then itself sometimes causing a GC). Fixed it so the memory check
        is done only if _vfork_child is false (also halved the size of the
        memory block, which was far more than needed).
--- John Gibson, Jul 16 1988
        Revised -Vfork_parent- to cope with Sun-4
--- John Gibson, Mar 17 1988
        Moved out of sysutil.p
 */
