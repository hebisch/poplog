/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/src/control.p
 > Purpose:         Control procedures
 > Author:          John Gibson (see revisions)
 */

;;; --------------- CONTROL PROCEDURES AND THE CALLSTACK ------------------

#_INCLUDE 'declare.ph'

global constant
        procedure Sys$-Abs_callstack_lim,
        _erase_sp_1, _move_callstack
    ;

global vars
        _call_stack_lim, _pop_callstack_lim
    ;

;;; ----------------------------------------------------------------------

section $-Sys => caller apply fast_apply chain fast_chain
                 chainto chainfrom exitto exitfrom;

        ;;; get the n-th caller of the current procedure
define caller(n);
    lvars n, _sframe = _caller_sp_flush(), _sflim = _call_stack_seg_hi;
    Check_integer(n, 0);
    repeat
        if _sframe == _sflim then
            quitif(_sflim == _call_stack_hi);
            _sframe!SF_NEXT_SEG_HI -> _sflim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;
        returnif(n == 0) (_sframe!SF_OWNER);
        n fi_- 1 -> n;
        _nextframe(_sframe) -> _sframe
    endrepeat;
    false
enddefine;

    ;;; apply a given procedure
define apply() with_nargs 1;
    ()()
enddefine;
;;;
define updaterof apply() with_nargs 1;
    -> ()()
enddefine;

    ;;; apply a given procedure without checks (and no extra stack frame)
define fast_apply() with_nargs 1;
    fast_chain()
enddefine;
;;;
define updaterof fast_apply() with_nargs 1;
    if dup()!PD_UPDATER then
        fast_chain(()!PD_UPDATER)
    else
        -> Exec_nonpd()
    endif
enddefine;


    ;;; leave the current procedure and chain another
define chain(/*p*/) with_nargs 1;
    lvars _sframe = _caller_sp_flush();
    if _sframe >=@(csword) _call_stack_hi or _sframe!SF_OWNER == setpop then
        mishap((), 1, 'CHAINING OUT OF SETPOP')
    else
        _chainfrom_caller(/*p*/)
    endif
enddefine;

define fast_chain() with_nargs 1;
    _fast_chainfrom_caller()
enddefine;

        ;;; Chain up the call stack for chainto and from.
        ;;; (callstack must be flushed when called)
define Chainto(/*targetpdr*/) with_nargs 1;
    ;;; A procedure has been left on stack by chainto/from.
    ;;; Unwind to targetpdr, then run the stacked procedure
    if dup() == _caller_sp()!SF_OWNER then
        ;;; chainto will have left procedure on stack.
        ;;; Leave this one and run stacked procedure
        fast_chain(->)
    else
        ;;; continue unwinding
        _chainfrom_caller((), Chainto)
    endif
enddefine;

        ;;; Same, but until reached a given callstack length.
        ;;; (callstack must be flushed when called)
define Chainto_frame(/*targetlen*/) with_nargs 1;
    if @@(csword)[_int(dup())] _sgreq @@(csword){_call_stack_hi, _caller_sp()}
    then
        ;;; Reached target -- leave this one and run stacked procedure
        fast_chain(->)
    else
        ;;; continue unwinding
        _chainfrom_caller((), Chainto_frame)
    endif
enddefine;

define Get_closure_procedure(p) -> p;
    lvars p, pdrpart;
    while isclosure(p) do
        ;;; its a closure - won't be on calling chain. Dig out pdpart or frozval
        if (p!PD_CLOS_PDPART ->> pdrpart) == weakref systrace
        and testdef systrace then
            fast_frozval(1, p) -> p
        else
            pdrpart -> p
        endif
    endwhile
enddefine;

define lconstant Check_targetpdr(target) -> target;
    lvars target, _sframe = _caller_sp_flush(), _sflim = _call_stack_seg_hi;
    Check_procedure(target);
    if target!PD_FLAGS _bitst _:M_PD_CLOSURE then
        Get_closure_procedure(target) -> target
    endif;
    repeat
        if _sframe == _sflim then
            quitif(_sflim == _call_stack_hi);
            _sframe!SF_NEXT_SEG_HI -> _sflim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;
        returnif(_sframe!SF_OWNER == target);
        _nextframe(_sframe) -> _sframe
    endrepeat;
    mishap(target, 1, 'PROCEDURE NOT IN CALLING CHAIN')
enddefine;

        ;;; exit to a given procedure or call stack length
        ;;; and then chain another procedure
define chainto(target, chainpdr);
    lvars target, chainpdr, _sframe;
    Check_procedure(chainpdr);
    if isinteger(target) then
        ;;; unwinding to length
        unless target fi_>= 4 then
            mishap(target, 1, 'INVALID CALL STACK LENGTH')
        endunless;
        _sp_flush() -> ;
        chain(chainpdr, target, Chainto_frame)
    else
        ;;; check that the target procedure is in fact in the calling chain
        Check_targetpdr(target) -> target;
        ;;; unwind to target procedure then call chainpdr
        chain(chainpdr, target, Chainto)
    endif
enddefine;

        ;;; exit from a given procedure or call stack length
        ;;; and chain another procedure
define chainfrom(target, chainpdr);
    lvars target, chainpdr;
    Check_procedure(chainpdr);
    if isinteger(target) then
        ;;; unwinding to length
        unless target fi_> 4 then
            mishap(target, 1, 'INVALID CALL STACK LENGTH')
        endunless;
        _sp_flush() -> ;
        chain(chainpdr, fast_chain, target, Chainto_frame)
    else
        ;;; check that the target procedure is in fact in the calling chain
        Check_targetpdr(target) -> target;
        if target == setpop then
            mishap(0, 'ATTEMPTING TO CHAINFROM SETPOP')
        endif;
        ;;; unwind to target procedure then chain chainpdr
        chain(chainpdr, fast_chain, target, Chainto)
    endif
enddefine;

        ;;; exit to a given procedure or call stack length
define exitto() with_nargs 1;
    ;;; target on the stack
    chainto((), identfn)
enddefine;

        ;;; exit from a given procedure or call stack length
define exitfrom() with_nargs 1;
    ;;; target on the stack
    chainfrom((), identfn)
enddefine;


;;; --- RESET THE CALLSTACK -----------------------------------------------

    /*  System reset.
        If top of stack is false, unwind all procedure calls, checking for
        possible corruption of the callstack. Then call setpop.
        If top of stack is procedure, clean up once then chain to it
    */
define Callstack_reset();
    lvars _sframe = _caller_sp_flush(), _next, _sflim = _call_stack_seg_hi;

    define lconstant Is_valid_procedure(_item);
        lvars _item;
        _isaddress(_item)
        and _LOWEST_POP_ADDRESS <=@(w) _item
        and _item <@(w) _open_seg_free_ptr
        and _item!KEY == procedure_key
        and not(_item!PD_FLAGS _bitst _:M_PD_CLOSURE)
    enddefine;

    if _sframe <@(csword) _sflim then
        ;;; test this and the next 2 up are valid stack frames
        if Is_valid_procedure(_sframe!SF_OWNER)
        and (_nextframe(_sframe) -> _next; _next == _sflim
             or (_next <@(csword) _sflim and Is_valid_procedure(_next!SF_OWNER)
                and (_nextframe(_next) -> _next; _next == _sflim
                     or (_next <@(csword) _sflim
                        and Is_valid_procedure(_next!SF_OWNER)))))
        then
            ;;; assume it's OK -- call procedure or on to the next one
            if isprocedure(dup()) then
                fast_chain()
            else
                _chainfrom_caller(Callstack_reset)
            endif
        endif;
        ;;; Else the call stack is corrupt (e.g. due to the presence of
        ;;; subroutine/external calls). Erase one item from it and try again
        ;;; (_erase_sp_1 chains back to this procedure)
        _srchain(_erase_sp_1)
    else
        ;;; call stack cleared
        if isprocedure(dup()) then
            ;;; chain given procedure
            fast_chain()
        else
            ;;; being called from setpop, chain back to it
            chain(setpop)
        endif
    endif
enddefine;


;;; --- CALLSTACK LIMITS -------------------------------------------------

define Nframe_cslen(_nframes);
    lvars _nframes;
    ;;; Use dummy procedure with 3 lvars as the basis for calculation
    define lconstant p3lvar(); lvars _a= _0, _b= _0, _c= _0; enddefine;
    p3lvar!PD_FRAME_LEN _mult _nframes
enddefine;

define Trim_callstack_lim(_lim, _gc);
    lvars _alim, _lim, _gc;
    ;;; get absolute O/S imposed limit (see sysutil.p)
    Abs_callstack_lim() -> _alim;
    ;;; then always leave 32 frames spare and, unless this is for the
    ;;; garbage collector, allow another 2**9 minimum for a garbage collection
    _alim@(csword)[ Nframe_cslen(if _gc then _32
                                 else _32 _add _2:1e9
                                 endif) ] -> _alim;
    ;;; return the more restrictive of the two
    if _lim <@(csword) _alim then _alim else _lim endif
enddefine;

define Set_call_stack_lim();
    lvars   _tmp, _safe = _sp()@(csword)-[Nframe_cslen(_2:1e7)],
            _plim = _call_stack_hi@(csword)-[_pop_callstack_lim];
    ;;; if _pop_callstack_lim is large, the offset calculation can wrap
    ;;; round
    if _plim >@(csword) _call_stack_hi then
        ;;; choose some arbitrary limit close to the appropriate end of
        ;;; memory
        _0@(csword)[_1|vpage] -> _plim;
    endif;
;;;        _extern
;;;         printf('Set_call_stack_lim, _plim = %ld\n _pop_callstack_lim = %ld\n',
;;;                  _plim, _pop_callstack_lim) -> _;
;;;         _extern fflush(_0) -> _;
    if _safe <@(csword) _plim then _safe -> _plim endif;
;;; Trim_callstack_lim(_plim, false) -> _call_stack_lim;
        Trim_callstack_lim(_plim, false) -> _tmp;
;;;        _extern
;;;        printf('Set_call_stack_lim, _plim = %ld\n _callstack_lim = %ld\n',
;;;                  _plim, _tmp) -> _;
;;;        _extern fflush(_0) -> _;
        _tmp -> _call_stack_lim;
enddefine;

    /*  Move the callstack area from _sp() to _limaddr up by _nwords
    */
define Move_callstack(_nwords, _limaddr, _correct_hi);
    lvars _nwords, _limaddr, _correct_hi;
    if _neg(_nwords) then
        ;;; expanding -- check OK
        lvars _new_sp = _sp()@(csword)[_nwords];
        ;;; return falae if can't do it
        returnif(Trim_callstack_lim(_new_sp, false) >@(csword) _new_sp)
                                            (false)
    endif;
    ;;; do the move
    _move_callstack(@@(csword)[_nwords], _limaddr);
    if _correct_hi then
        ;;; moving whole callstack (down) -- correct _call_stack_hi
        _call_stack_hi@(csword)[_nwords] ->> _call_stack_hi
                                         -> _call_stack_seg_hi
    endif;
    ;;; return true and set new stack lim
    chain(true, Set_call_stack_lim)
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jul 29 1999
        Added a check in Set_call_stack_lim that calculation of the limit
        doesn't wrap round. This can happen if the stack grows down in low
        memory or up in high memory.
--- John Gibson, Jan  3 1991
        Made -Callstack_reset- check more stack frames
--- John Gibson, Nov 15 1990
        Added extra test for proper procedure (i.e. non-closure) in
        -Is_valid_procedure- in -Callstack_reset-
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
--- John Gibson, Nov 15 1989
        Changes for segmented callstack.
--- John Gibson, Oct 18 1989
        Assignment of [] to s*etpop_handlers no longer needed.
--- John Gibson, Sep  8 1989
        Removed assignment to ch*ain_trace (moved into setpop)
--- John Gibson, Sep  1 1989
        Added callstack limit procedures
--- John Gibson, Jul  2 1989
        _chainfrom_caller now a subroutine
--- John Gibson, Jun 30 1989
        Added -fast_apply- and -fast_chain-
--- John Gibson, May 15 1989
        Removed vmdefs.ph
--- John Gibson, Jan 22 1989
        Removed use of _sysapply
--- John Williams, Jun 17 1988
        -Callstack_reset- turns off exit tracing (cf BR johnw.118)
--- John Gibson, Mar 24 1988
        Fixed bug in updater of -pop_callstack_lim-
--- John Gibson, Feb 28 1988
        Weakref'ed -systrace-
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
 */
