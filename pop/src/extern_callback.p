/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/extern_callback.p
 > Purpose:
 > Author:          John Gibson, Apr 11 1990 (see revisions)
 > Documentation:   REF *EXTERNAL
 > Related Files:   src/aextern.s
 */

;;; --------------- CALLBACK FROM EXTERNAL PROCEDURES -----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'

global constant
        procedure (exacc_ntstring, cons_fixed, free_fixed_hold,
        sys_read_path, identof, isconstant)
    ;

weak global vars
        pop_current_process
    ;

section Sys;

global constant
        procedure (Move_callstack, Cons_extern_ptr, Reg_save_apply,
        Set_call_stack_lim, Setup_system)
    ;

endsection;


;;; ------------------------------------------------------------------------

section $-Sys$-Extern;

    /*  Called from _pop_external_callback in aextern.s
    */
define Callback(_argptr, _break_diff);
    lvars   p, _argptr, _func, _key, _break_diff;
    dlvars  _sav_xptr, _sav_len;

    lconstant
        exptr       = writeable struct EXTERNAL_PTR
                            =>> {% false, external_ptr_key, _NULL %},
        mystackmark = struct STACKMARK =>> {% _0, stackmark_key %},
        ;

    ;;; _context can only be 1 or 2, since suspension of processes
    ;;; etc across external calls is not allowed

    define lconstant Barrier(_context, _abexit_ret);
        lvars   _cs_seghi = _call_stack_seg_hi,
                _dumsf = _nextframe(_caller_sp_flush()),
                _argptr, _break_diff, _context, _abexit_ret, _plog_nwords;

        _saved_sp   -> _dumsf!SF_NEXT_SEG_SP;
        _cs_seghi   -> _dumsf!SF_NEXT_SEG_HI;
        _dumsf -> _call_stack_seg_hi;
        _NULL -> _saved_sp;             ;;; indicates not in external calls

        exptr!XP_PTR -> _sav_xptr;      ;;; localise exptr's value

        -> (_argptr, _break_diff);      ;;; remove args

        if _cs_seghi == _NULL then
            ;;; Being called from outside Pop -- only possible in a
            ;;; standalone linked program, not in ordinary Poplog
            _external_flags _biset _:PEF_CATCH_ABEXIT_NEXT -> _external_flags;
            if _in_external_control then
                ;;; Not first time
                _dumsf -> _call_stack_hi;
                ;;; Reset the callstack limit
                Set_call_stack_lim()
            else
                ;;; First time -- setup system
                true -> _in_external_control;
                _0 ->> _sav_len -> _break_diff;
                if _argptr!(w)[_0] == _6 /* SYSINIT */ then
                    ;;; this call is intended for system init, and can supply
                    ;;; parameters to setup
                    _argptr!(w)[_1]     ;;; prolog area size
                else
                    _0                  ;;; default size
                endif -> _plog_nwords;
                Setup_system(_dumsf, _plog_nwords) ->
            endif
        endif;

        ;;; put a barrier on the stack
        mystackmark, _stklength() -> _sav_len;
        (_argptr, _break_diff)      ;;; restack args
    enddefine;
    ;;;
    define updaterof Barrier(_context, _abexit_ret);
        lvars   _context, _dumsf = _call_stack_seg_hi, _abexit_ret, _n,
                _dlxsm, _mark_in_place, _tmp, _exflags = _external_flags;
        lconstant _NORMEXIT = _1, _ABNORMEXIT = _0, _ABNORMEXIT_UNWIND = _-1;

        ;;; clear one-shot flags
        _exflags _biclear (_:PEF_RETURN_ABEXIT_NEXT
                            _biset _:PEF_CATCH_ABEXIT_NEXT) -> _external_flags;

        _sav_xptr -> exptr!XP_PTR;  ;;; restore exptr's value

        ;;; restore callstack
        _dumsf!SF_NEXT_SEG_SP -> _saved_sp;
        _dumsf!SF_NEXT_SEG_HI -> _call_stack_seg_hi;

        returnif(_context == 1) (->, _NORMEXIT);    ;;; erase mystackmark

        ;;; abnormal exit
        () -> _dlxsm;       ;;; save the dlocal expr stackmark

        _stklength() _sub _sav_len -> _n;
        _nonneg(_n) and _nonzero(_sav_len)
            and _user_sp()!(w){_n} == mystackmark -> _mark_in_place;

        if _exflags _bitst (_:PEF_CATCH_ABEXIT_NEXT
                                _biset _:PEF_CATCH_ABEXIT_ANY) then
            ;;; Return into the external calls, finishing the abnormal exit
            ;;; -- clean up the stack
            if _mark_in_place then
                _user_sp()@(w){_n}++ -> _user_sp();
                ;;; return into the external calls
                _chainfrom_caller(_ABNORMEXIT, identfn)
            elseif _call_stack_seg_hi == _NULL then
                ;;; Being called from outside Pop
                _chainfrom_caller(_ABNORMEXIT, identfn)
            endif;
            ;;; if stack underflowed, allow abexit to continue
            _exflags _biset _:PEF_RETURN_ABEXIT_NEXT -> _exflags

        elseif _mark_in_place then
            ;;; remove mystackmark so as not to cause unnecessary problems.
            ;;; shift anything above it down
            _user_sp() -> _tmp;     ;;; must get this first
            ;;; move and erase move result and first item
            _move(_n, _tmp, _tmp@(w)++) -> (,)
        endif;

        if _exflags _bitst (_:PEF_RETURN_ABEXIT_NEXT
                                _biset _:PEF_RETURN_ABEXIT_ANY) then
            ;;; Return into the external calls, but set the abnormal exit
            ;;; to continue after returning from them.
            ;;; Set the current abnormal exit return as the return
            ;;; from _call_external and leave stack alone.
#_IF DEF SPARC
            Reg_save_apply(_abexit_ret,
                    procedure(_abexit_ret, _sframe, _addr);
                        lvars _sframe, _abexit_ret, _addr;
                        ;;; find _call_external stack frame
                        until _sframe!SF_FP == _saved_sp do
                            _sframe!SF_FP -> _sframe
                        enduntil;
                        _abexit_ret@(code)[_-2] -> _sframe!SF_CALLER_RETURN
                    endprocedure);
#_ELSE
            _abexit_ret -> _saved_sp!SF_RETURN_ADDR;
#_ENDIF

            _external_flags _biset _:PEF_DOING_ABEXIT -> _external_flags;
            ;;; return into the external calls
            _chainfrom_caller(_ABNORMEXIT, identfn)

        else
#_IF DEF EXTERN_CALLBACK_UNWIND
            ;;; Set the current abnormal exit return as the return
            ;;; from _call_external
            _abexit_ret -> _saved_sp!SF_RETURN_ADDR;
            ;;; then returning _ABNORMEXIT_UNWIND (-1) to
            ;;; _pop_external_callback causes it to unwind the block of
            ;;; external calls cleanly and return into _call_external
            _chainfrom_caller(_ABNORMEXIT_UNWIND, identfn)
#_ELSE
            ;;; else just erase the block of external calls by shifting
            ;;; up Callback and Barrier's stack frames
            _dlxsm;     ;;; put back the dlocal expr stackmark
            Move_callstack(##(csword){_saved_sp,_dumsf}, _dumsf, false) ->
#_ENDIF
        endif
    enddefine;

    dlocal
            ;;; stop processes suspending across external calls
            weakref pop_current_process = false,

            ;;; set up barrier for callstack (must be first dlocal expr)
            0 % Barrier(dlocal_context, _dlocal_abexit_return()) %,
        ;

    if testdef Mem_break_changed and _nonzero(_break_diff) then
        weakref Mem_break_changed()
    endif;

    ;;; arg 0 is function code
    _argptr!(w)++ -> _argptr -> _func;

    go_on _pint(_func) to
    ;;;   1      2       3       4        5        6
        MISHAP  CALL  GETIDENT  FREE  CHECKINTR SYSINIT

    else ERROR;

    MISHAP:
        ;;; arg 1 is pointer to null-terminated string
        mishap(0, exacc_ntstring(Cons_extern_ptr(_argptr!((b)))));
        goto RETURN;

    CALL:
        ;;; arg 1 is ref/ident/procedure
        ;;; arg 2 is pointer to arg/result array
        _argptr!(full)++ -> _argptr -> p;
        if iscompound(p) and p /== _NULL then
            p!KEY -> _key;
            if _key == ref_key then
                fast_cont(p) -> p;
                if iscompound(p) then p!KEY -> _key endif
            endif;
            if _key == ident_key then
                idval(p) -> p;
                if iscompound(p) then p!KEY -> _key endif
            endif;
            if _key == procedure_key then
                ;;; pass arg address in fixed external pointer record so
                ;;; basic call doesn't create garbage
                _argptr!(w) -> exptr!XP_PTR;
                fast_apply(exptr, p);
                goto RETURN
            endif
        endif;
        mishap(p, 1, 'EXECUTING NON-PROCEDURE FOR EXTERNAL CALLBACK');
        chain(identfn);     ;;; in case it returns

    GETIDENT:
        ;;; arg 1 is pointer to null-terminated ident pathname
        ;;; arg 2 is word for result

        define lconstant get_ident(namestring);
            lvars namestring, id;
            dlocal proglist_state = proglist_new_state(stringin(namestring));
            identof(sys_read_path(readitem(), false, false)) -> id;
            if isconstant(id) == true then idval(id) -> id endif;
            cons_fixed(id, ref_key, true)
        enddefine;

        get_ident(exacc_ntstring(Cons_extern_ptr(_argptr!((b))++ -> _argptr)))
                                                    -> _argptr!(full);
        goto RETURN;

    FREE:
        ;;; arg 1 is pop object
        free_fixed_hold(_argptr!(full));
        goto RETURN;

    CHECKINTR:
        ;;; check interrupts -- args are ignored
        _CHECKINTERRUPT;
        goto RETURN;

    SYSINIT:
        ;;; Initialise Pop from outside -- dealt with in Barrier
        goto RETURN;

    RETURN:
        ;;; this test can't be done inside Barrier because once that is
        ;;; run for normal exit, it would not get run again for the abnormal
        ;;; exit caused by the mishap
        if _stklength() /== _sav_len or dup() /== mystackmark then
            mishap(0, 'USERSTACK HAS CHANGED AFTER EXTERNAL CALLBACK');
            _chainfrom_caller(identfn)  ;;; in case it returns
        endif;
        return;

    ERROR:
        mishap(_pint(_func), 1, 'UNKNOWN FUNCTION CODE FOR EXTERNAL CALLBACK');

;;; DUMMY CODE -- naff in the extreme, but needed to force
;;; $popexternlib/c_callback.o to be linked in.
        _extern pop_call -> _argptr;

        chain(identfn);     ;;; in case it returns
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 25 1994
        Added EXTERN_CALLBACK_UNWIND code
--- John Gibson, Nov 12 1993
        Added the piece of dummy code to force inclusion of
        $popexternlib/c_callback.o
--- John Gibson, Nov 12 1993
        Added code for SYSINIT function to allow explicit or automatic
        initialisation of Pop from controlling external code in a standalone
        linked program.
--- John Gibson, Jul  8 1991
        PEF flags now pop values not nonpop. Also changed dlocal proglist
        to use proglist_state, etc.
--- John Gibson, Feb 14 1991
        Moved test for stack having changed from -Barrier- to end of main
        procedure (see comment).
--- John Gibson, Jan 19 1991
        _external_flags no longer controls whether in user external calls
        (done by _extern __pop_in_user_extern instead)
--- John Gibson, Dec 20 1990
        Made abnormal exit remove mystackmark
--- John Gibson, Nov 23 1990
        Removed redundant stack check around CALL case (now done by
        -Barrier-).
--- John Gibson, Nov 14 1990
        Now allows return into the external calls on abnormal exit
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- Roger Evans, Jun 26 1990 added CHECKINTR function
 */
