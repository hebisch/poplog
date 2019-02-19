/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/process.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCESS
*/

;;; ------------------------ PROCESSES --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'process.ph'
#_INCLUDE 'gcdefs.ph'

weak global constant
        procedure prolog_barrier_apply
    ;

global constant
        _swap_in_callstack, _swap_out_callstack,
        _ussave, _usrestore, _useras, _userasund,
    ;

section $-Sys;

global constant
        procedure (Chainto, Get_closure_procedure, Rawstruct_getsize)
    ;

weak global constant
        procedure (Plog$-Proc_save, Plog$-Proc_restore, Gc$-App_plog_contns)
    ;

weak global vars
        Plog$-barrier_count
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys => pop_current_process,
                 isprocess, isliveprocess, is_system_process,
                 runproc, suspend, suspend_chain, resume,
                 ksuspend, ksuspend_chain, kresume, saveproc,
                 consproc, consproc_to, process_key;

protected global vars
    pop_current_process = false,
    ;


lconstant

    ;;; For the process userstack stack structure
    stack_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_WRITEABLE,        ;;; K_FLAGS
        _:GCTYPE_STACK,         ;;; K_GC_TYPE
        Rawstruct_getsize,      ;;; K_GET_SIZE
        %},

    ;;; process state key
    proc_state_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_WRITEABLE,        ;;; K_FLAGS
        _:GCTYPE_PROC_STATE,    ;;; K_GC_TYPE
        Rawstruct_getsize,      ;;; K_GET_SIZE
        %},
    ;


    ;;; initialise process userstack stack
    ;;; need to define this as a perm constant so it can include
    ;;; pointers into itself (this is a kludge)
constant
    Ps_us_stack = writeable struct STACK
                    =>> {% @@STK_DATA _sub @@POPBASE, stack_key,
                            dup(Ps_us_stack@STK_DATA),
                            =>> {} %};
vars
    userstack_stack = Ps_us_stack;

lvars
    _rpcallstklen,
    ;

lconstant macro
    PBA_WEAK    = [weakref %"["% prolog_barrier_apply %"]"%];


;;; --- ERROR CONDITIONS -------------------------------------------------

define lconstant Ps_error(item);
    lvars item, ms;
    unless isprocess(item) then
        'PROCESS NEEDED'
    elseif item!PS_FLAGS _bitst _:M_PS_DEAD then
        'ATTEMPT TO RUN DEAD PROCESS'
    elseif item!PS_FLAGS _bitst _:M_PS_SUBSUMED then
        'ATTEMPT TO RUN SUBSUMED PROCESS'
    else
        'ATTEMPT TO RUN ALREADY-ACTIVE PROCESS'
    endunless -> ms;
    mishap(item, 1, ms)
enddefine;

define lconstant Ps_notinproc();
    mishap(0, 'NOT INSIDE A PROCESS')
enddefine;

define lconstant Ps_stackunder(numarg);
    lvars numarg;
    mishap(numarg, 1, 'NOT ENOUGH ITEMS ON STACK')
enddefine;


;;; --- PREDICATES --------------------------------------------------------

define isprocess(item);
    lvars item;
    iscompound(item) and item!KEY == process_key
enddefine;

define isliveprocess(item);
    lvars item;
    unless isprocess(item) then
        false
    elseif item!PS_FLAGS _bitst _:M_PS_DEAD then
        false
    elseif item!PS_FLAGS _bitst _:M_PS_ACTIVE then
        item
    else
        true
    endunless
enddefine;

define is_system_process(item);
    lvars item;
    isprocess(item) and item!PS_PERM_FLAGS _bitst _:M_PS_SYSTEM
enddefine;


;;; --- RUNPROC -----------------------------------------------------------

    /*  Called when chaining out of runproc
    */
define lconstant Abexit_kill();
    lvars proc = pop_current_process;
    returnunless(proc);
    if proc!PS_PERM_FLAGS _bitst _:M_PS_NON_VOLATILE
    and not(proc!PS_FLAGS _bitst _:M_PS_SUSPENDING) then
        _0      ;;; restore to runnable state
    else
        _:M_PS_DEAD
    endif -> proc!PS_FLAGS
enddefine;

    ;;; run a process
define runproc(_nargs, proc);
    lvars   proc, _us, _ptr, _size, _nargs, _stksize;
    dlocal  pop_current_process, PBA_WEAK Plog$-barrier_count, _rpcallstklen;

    ;;; allocate more space for the proc userstack stack
    define lconstant us_stack_alloc(_size);
        lvars us = userstack_stack, _new_us, _size, _oldsize;
        @@(w){us!STK_PTR, us@STK_DATA} -> _oldsize; ;;; current size
        _size _add _oldsize _add @@(w)[_32] -> _size;
        if _size _lt @@(w)[_256] then @@(w)[_256] -> _size endif;
        @@STK_DATA{_size} _sub @@POPBASE -> _size;  ;;; size of whole struct

        Get_store(_size) -> _new_us;
        stack_key -> _new_us!KEY;
        _size -> _new_us!RAW_SIZE;

        ;;; copy old stuff
        _moveq(_oldsize, us@STK_DATA, _new_us@STK_DATA) -> _new_us!STK_PTR;
        _new_us@POPBASE{_size} -> _new_us!STK_LIM;      ;;; new limit
        _new_us ->> userstack_stack         ;;; assign and return the new one
    enddefine;

    dlocal 0 ( % _0 -> _stksize,
                 unless _zero(_stksize) then
                    ;;; restore user stack under current stack
                    Alloc_user_space(_stksize);
                    userstack_stack -> _us;
                    _us!STK_PTR@(w)-{_stksize} -> _us!STK_PTR;
                    _usrestore(_stksize, _us!STK_PTR)
                 endunless %,

                % false -> pop_current_process,
                  if dlocal_context == 2 then Abexit_kill() endif%
            );

    unless isprocess(proc) and _zero(proc!PS_FLAGS) then
        Ps_error(proc)
    elseunless isinteger(_nargs) and _nargs fi_>= 0 then
        Check_integer(_nargs, 0)        ;;; produce error
    endunless;

    ;;; swap out the current user stack excluding top _nargs
    _stklength() _sub @@(w)[_int(_nargs)] -> _size;
    if _neg(_size) then
        Ps_stackunder(_nargs)
    elseif _nonzero(_size) then
        userstack_stack -> _us;
        _us!STK_PTR@(w){_size} -> _ptr;     ;;; will be new STK_PTR
        if _ptr >@(w) _us!STK_LIM then
            ;;; run out of proc u/s stack space - alloc more space
            us_stack_alloc(_size) -> _us;
            _us!STK_PTR@(w){_size} -> _ptr
        endif;
        _ussave(_size, _us!STK_PTR);        ;;; swap out u/s excluding top _nargs
        _ptr -> _us!STK_PTR;                ;;; new pointer
        _size -> _stksize                   ;;; says swapped out for exit code
    endif;

    ;;; current length of call stack
    @@(csword){_call_stack_seg_hi, _sp()} -> _rpcallstklen;

    ;;; loop until return from (k)suspend/(k)resume
    repeat
        proc -> pop_current_process;
        ;;; save nargs
        _nargs -> proc!PS_RUN_NARGS;
        ;;; set no prolog_barrier_applies
        false -> PBA_WEAK Plog$-barrier_count;

        ;;; swap in any user stack
        unless _zero(proc!PS_USERSTACK_SIZE ->> _size) then
            Alloc_user_space(_size);    ;;; make u/s space for _size
            ;;; swap in u/s UNDER current
            _usrestore(_size, proc!PS_CALLSTACK_LIM)
        endunless;

        ;;; if proc has a prolog saved state, swap that in
        if proc!PS_PLOG_STATE then
            PBA_WEAK Plog$-Proc_restore(proc!PS_PLOG_STATE)
        endif;

        ;;; recover callstack and enter
        _:M_PS_ACTIVE -> proc!PS_FLAGS;
        _swap_in_callstack(identfn, proc);

        if proc!PS_FLAGS _bitst _:M_PS_ACTIVE then
            ;;; did normal procedure exit -- kill it
            _:M_PS_DEAD -> proc!PS_FLAGS;
            quitloop
        else
            ;;; process was suspended/killed -- next to resume is on the stack
            quitunless(() ->> proc);
            () -> _nargs
        endif
    endrepeat
enddefine;


;;; --- SUSPENDING AND RESUMING --------------------------------------------

define lconstant Partial_reapply(app_p);
    lvars proc = pop_current_process, savproc, procedure app_p;
    ;;; save a copy of the outer proc
    Get_store(@@(struct PROCESS)++) -> savproc;
    _moveq(@@(struct PROCESS)++, proc@POPBASE, savproc@POPBASE) -> ;

    ;;; cause a new state to be created for it
    _NULL ->> proc!PS_STATE -> proc!PS_CALLSTACK_PARTIAL;
    false -> proc!PS_PLOG_STATE;

    ;;; now suspend/resume again
    app_p(/* args */);

    ;;; on coming back, set the proc back as it was
    _moveq(@@(struct PROCESS)++, savproc@POPBASE, proc@POPBASE) -> ;
    _NULL -> savproc!PS_STATE
enddefine;

    /*  Carry on suspending, etc until required process is reached
    */
define lconstant Continue_suspend();

    define lconstant Continue(last_proc);
        lvars last_proc;
        ;;; run the suspending procedure on the stack
        fast_apply();
        ;;; and when returning, rerun the last process suspended
        ;;; passing down the same number of args as the actual suspended
        ;;; process got re-invoked with
        _0 -> last_proc!PS_FLAGS;
        chain(pop_current_process!PS_RUN_NARGS, last_proc, runproc)
    enddefine;

    _:M_PS_SUBSUMED -> pop_current_process!PS_FLAGS;
    pop_current_process;            ;;; -last_proc- arg to Continue
    false -> pop_current_process;   ;;; to stop it being killed
    _chainfrom_caller(Continue)     ;;; chain Continue out of runproc
enddefine;

    /*  Set up stack args for (k)suspending/resuming a process other
        than the current
    */
define lconstant Set_up_other(target_proc, opt_arg, cllr);
    lvars opt_arg, target_proc, procedure cllr, _nres;

    if target_proc /== pop_current_process then
        ;;; suspending process other than current
        Check_integer(->> _nres, 0);
        unless target_proc!PS_FLAGS _bitst _:M_PS_ACTIVE then
            mishap(target_proc, 1, 'ATTEMPT TO SUSPEND INACTIVE OR DEAD PROCESS');
        endunless;
        ;;; set up args for Continue_suspend
        if opt_arg then
            (_nres, target_proc, opt_arg, cllr), _nres fi_+ 4
        else
            (_nres, target_proc, cllr), _nres fi_+ 3
        endif;
        true
    else
        false
    endif
enddefine;

    /*  Set the current process suspending
    */
define lconstant Susproc(_nres) -> _nres;
    lvars proc = pop_current_process, _state, _usize, _csize, _size, _nres;

    unless isinteger(_nres) and _nres fi_>= 0 then
        Check_integer(_nres, 0)
    endunless;

    ;;; u/s length without top _nres
    _stklength() _sub @@(w)[_int(_nres)] -> _usize;
    if _neg(_usize) then Ps_stackunder(_nres) endif;

    _:M_PS_ACTIVE _biset _:M_PS_SUSPENDING -> proc!PS_FLAGS;

    ;;; get length of process' call stack
    @@(csword){_call_stack_seg_hi, _nextframe(_caller_sp_flush())}
                                            _sub _rpcallstklen -> _csize;
    ;;; total size of state
    @@PSS_DATA{_csize _add _usize} _sub @@POPBASE -> _size;

    if (proc!PS_STATE ->> _state) == _NULL
    or _state@~PSS_DATA!RAW_SIZE _lt _size then
        ;;; need a new state
        ;;; zap current state so it'll get garbaged if there's a gc
        _NULL -> proc!PS_STATE;
        if proc!PS_PLOG_STATE ->> _state then
            ;;; set trail and contn to zero size in case of gc
            _0 ->> _state!PLGPS_TRAIL_SIZE -> _state!PLGPS_CONTN_SIZE
        endif;

        Get_store(_size) -> _state;
        proc_state_key -> _state!KEY;
        _size -> _state!RAW_SIZE;
        _state@PSS_DATA ->> _state -> proc!PS_STATE
    endif;

    ;;; end limit for callstack
    _state@(w){_csize} ->> _csize -> proc!PS_CALLSTACK_LIM;
    ;;; set callstack empty
#_IF DEF STACK_GROWS_UP
    _state -> proc!PS_CALLSTACK_PARTIAL;
#_ELSE
    _csize -> proc!PS_CALLSTACK_PARTIAL;
#_ENDIF

    ;;; save userstack
    unless _zero(_usize) then
        ;;; save u/s without _nres
        _ussave(_usize, proc!PS_CALLSTACK_LIM)
    endunless;
    _usize -> proc!PS_USERSTACK_SIZE;

    ;;; save the prolog stacks upto the oldest prolog_barrier_apply
    ;;; if inside a call of one
    if testdef prolog_barrier_apply and PBA_WEAK Plog$-barrier_count then
        PBA_WEAK Plog$-Proc_save(proc, PBA_WEAK Plog$-barrier_count)
    else
        false -> proc!PS_PLOG_STATE
    endif
enddefine;

    /*  Suspend a process
    */
define suspend() with_nargs 1;
    lvars cproc = pop_current_process;
    unless cproc then Ps_notinproc() endunless;
    if cproc!PS_CALLSTACK_PARTIAL /== _NULL then
        chain((), suspend, Partial_reapply)
    endif;
    if isprocess(dup()) and Set_up_other((), false, suspend) then
        Susproc() ->, Continue_suspend
    else
        Susproc() ->, false, identfn
    endif;
    _srchain((), cproc, _swap_out_callstack)
enddefine;

    /*  Suspend a process and chain a procedure
    */
define suspend_chain(p) with_nargs 2;
    lvars cproc = pop_current_process, procedure p;
    unless cproc then Ps_notinproc() endunless;
    Check_procedure(p);
    if cproc!PS_CALLSTACK_PARTIAL /== _NULL then
        chain((), p, suspend_chain, Partial_reapply)
    endif;
    if isprocess(dup()) and Set_up_other((), p, suspend_chain) then
        Susproc() ->, Continue_suspend
    else
        Susproc() ->, p, procedure();
                            false -> pop_current_process;
                            _fast_chainfrom_caller()
                         endprocedure
    endif;
    _srchain((), cproc, _swap_out_callstack)
enddefine;

    /*  Suspend a process and resume another
    */
define resume(rproc) with_nargs 2;
    lvars rproc, cproc = pop_current_process;
    unless cproc then Ps_notinproc() endunless;
    unless isprocess(rproc) and (_zero(rproc!PS_FLAGS) or rproc == cproc) then
        Ps_error(rproc)
    endunless;
    if cproc!PS_CALLSTACK_PARTIAL /== _NULL then
        chain(rproc, resume, Partial_reapply)
    endif;
    if isprocess(dup()) and Set_up_other((), rproc, resume) then
        Susproc() ->, Continue_suspend
    else
        Susproc(), rproc, identfn
    endif;
    _srchain((), cproc, _swap_out_callstack)
enddefine;

    /*  Set up a process to be killed, leaving top _nres items on stack
    */
define lconstant Killproc(_nres) -> _nres;
    lvars proc = pop_current_process, _nres, _size;
    unless isinteger(_nres) and _nres fi_>= 0 then
        Check_integer(_nres, 0)
    endunless;
    _stklength() _sub @@(w)[_int(_nres)] -> _size;  ;;; size without top _nres
    if _neg(_size) then Ps_stackunder(_nres) endif;
    unless _zero(_size) then
        _userasund(_size)   ;;; remove all but top _nres
    endunless;
    proc!PS_PERM_FLAGS _biclear _:M_PS_NON_VOLATILE -> proc!PS_PERM_FLAGS;
    _NULL -> proc!PS_STATE;
    false -> proc!PS_PLOG_STATE;
    _sp_flush() ->
enddefine;

    /*  Kill and exit a process, and chain a procedure
    */
define ksuspend_chain(p) with_nargs 2;
    lvars procedure p;
    unless pop_current_process then Ps_notinproc() endunless;
    Check_procedure(p);
    if isprocess(dup()) and Set_up_other((), p, ksuspend_chain) then
        Killproc() ->
    else
        Killproc() ->, p
    endif;
    chain((), fast_chain, runproc, Chainto)
enddefine;

define ksuspend = ksuspend_chain(% identfn %) enddefine;

    /*  Kill and exit a process, and resume another
    */
define kresume(rproc) with_nargs 2;
    lvars rproc;
    unless pop_current_process then Ps_notinproc() endunless;
    unless isprocess(rproc) and _zero(rproc!PS_FLAGS) then
        Ps_error(rproc)
    endunless;
    if isprocess(dup()) and Set_up_other((), rproc, kresume) then
        Killproc() ->, fast_chain
    else
        Killproc(), rproc, Abexit_kill
    endif;
    chain((), runproc, Chainto)
enddefine;

define saveproc();
    lvars cproc = pop_current_process;
    lconstant mark = 'mark';
    unless cproc then Ps_notinproc() endunless;
    if cproc!PS_CALLSTACK_PARTIAL /== _NULL then
        chain(saveproc, Partial_reapply)
    endif;
    procedure; Susproc(0) -> endprocedure();
    procedure();
        copy(pop_current_process), mark, 2, pop_current_process
    endprocedure, cproc;
    false -> cproc;     ;;; so copy doesn't have ptr to current proc
    _swap_out_callstack();
    ;;; resumed
    if stacklength() /== 0 and dup() == mark then
        ->  ;;; erase mark leaving copied proc on stack
    else
        false
    endif
enddefine;

;;; --- CREATING PROCESSES ---------------------------------------------------

define consproc(_nargs, p) -> proc;
    lvars proc, p, _state, _size, _csize, _usize, _nargs, _volatile = true;

    define lconstant proc_init(/* identfn, proc, */ p) with_nargs 3;
        lvars p;
        _swap_out_callstack(/* identfn, proc */);
        ;;; being run for first time -- run procedure
        fast_chain(p)
    enddefine;

    if isboolean(p) then
        p -> _volatile, _nargs -> p -> _nargs
    endif;
    Check_procedure(p);
    Check_integer(_nargs, 0);
    @@(w)[_int(_nargs)] -> _usize;              ;;; userstack size
    if _stklength() _lt _usize then Ps_stackunder(_nargs) endif;

    ;;; construct outer proc record and state record
    @@(w)[proc_init!PD_FRAME_LEN] -> _csize;    ;;; initial callstack size
    @@PSS_DATA{_csize _add _usize} _sub @@POPBASE -> _size;    ;;; total state size
    Get_store(_size _add @@(struct PROCESS)++) -> proc;
    process_key -> proc!KEY;
    _usize -> proc!PS_USERSTACK_SIZE;
    if _volatile then _0 else _:M_PS_NON_VOLATILE endif -> proc!PS_PERM_FLAGS;
    _:M_PS_ACTIVE _biset _:M_PS_SUSPENDING -> proc!PS_FLAGS;
    false -> proc!PS_PLOG_STATE;

    ;;; initialise state record
    proc@(struct PROCESS)++ -> _state;
    proc_state_key -> _state!KEY;
    _size -> _state!RAW_SIZE;

    ;;; insert state pointers into proc record
    _state@PSS_DATA ->> _state -> proc!PS_STATE;
    _state@(w){_csize} ->> _state -> proc!PS_CALLSTACK_LIM;
    _NULL -> proc!PS_CALLSTACK_PARTIAL;

    ;;; init userstack
    _user_sp() -> _size;                ;;; must get this first
    _moveq(_usize, _size, _state) -> ;
    _useras(_usize);                    ;;; remove args

    ;;; copy proc_init stack frame into callstack section
    proc_init(identfn, proc, p)
enddefine;

    /*  Construct a process upto a given procedure -target_p-, rerunning
        the process and returning it to caller
    */
define consproc_to(_nargs, target_p);
    lvars   proc, target_p, _addr, _csize, _size, _usize, _nargs,
            _plogbar_count, _volatile = true;

    if isboolean(target_p) then
        target_p -> _volatile, _nargs -> target_p;
        -> _nargs
    endif;
    Check_procedure(target_p);
    while isclosure(target_p) do
        ;;; its a closure - won't be on calling chain.
        ;;; Dig out pdpart or frozval
        Get_closure_procedure(target_p) -> target_p
    endwhile;

    ;;; find stack frame for target procedure
    0 -> _plogbar_count;
    _caller_sp_flush() ->> _addr -> _csize;
    repeat
        if _addr >=@(csword) _call_stack_seg_hi then
            mishap(target_p, 1, 'PROCEDURE NOT IN CALLING SEQUENCE')
        elseif (_addr!SF_OWNER ->> proc) == runproc then
            ;;; need to construct a process chain
            define lconstant Do_sus();
                Susproc() ->, Continue_suspend;
                _srchain((), pop_current_process, _swap_out_callstack)
            enddefine;
            (_nargs, target_p, _volatile, consproc_to, 4);
            if pop_current_process!PS_CALLSTACK_PARTIAL /== _NULL then
                chain((), Do_sus, Partial_reapply)
            else
                chain((), Do_sus)
            endif
        elseif proc == weakref prolog_barrier_apply then
            _plogbar_count fi_+ 1 -> _plogbar_count
        endif;
        _nextframe(_addr) -> _addr;     ;;; next frame
        quitif(proc == target_p)        ;;; finished if last was target_p
    endrepeat;

    if isprocedure(_nargs) then
        ;;; procedure supplied to compute _nargs
        fast_apply(_nargs) -> _nargs
    endif;
    Check_integer(_nargs, 0);
    @@(w)[_int(_nargs)] -> _usize;
    if _stklength() _lt _usize then Ps_stackunder(_nargs) endif;

    ;;; compute length of proc' call stack
    ;;; -- includes target_p length since _addr points to caller of target_p
    @@(csword){_addr, _csize} -> _csize;

    @@PSS_DATA{_csize _add _usize} _sub @@POPBASE -> _size; ;;; total size of state
    ;;; enough for proc rec and state
    Get_store(_size _add @@(struct PROCESS)++) -> proc;
    process_key -> proc!KEY;
    if _volatile then _0 else _:M_PS_NON_VOLATILE endif -> proc!PS_PERM_FLAGS;
    _:M_PS_ACTIVE _biset _:M_PS_SUSPENDING -> proc!PS_FLAGS;
    false -> proc!PS_PLOG_STATE;

    ;;; init state record
    proc@(struct PROCESS)++ -> _addr;
    proc_state_key -> _addr!KEY;
    _size -> _addr!RAW_SIZE;

    ;;; assign state pointers into proc record
    _addr@PSS_DATA ->> _addr -> proc!PS_STATE;  ;;; state record
    _addr@(w){_csize} ->> _csize -> proc!PS_CALLSTACK_LIM;  ;;; addr of u/s
    ;;; set callstack empty
#_IF DEF STACK_GROWS_UP
    _addr -> proc!PS_CALLSTACK_PARTIAL;
#_ELSE
    _csize -> proc!PS_CALLSTACK_PARTIAL;
#_ENDIF

    ;;; copy u/s to state
    _user_sp() -> _addr;                    ;;; get this first
    _moveq(_usize, _addr, _csize) -> ;
    _useras(_usize);                        ;;; remove the args
    _usize -> proc!PS_USERSTACK_SIZE;

    if _plogbar_count /== 0 then
        PBA_WEAK Plog$-Proc_save(proc, _plogbar_count)
    endif;

    ;;; swap out the call stack and run the process afterwards
    _srchain((proc, 1, proc, runproc), proc, _swap_out_callstack)
enddefine;


;;; --- COPY A PROCESS --------------------------------------------------

define Process_copy(proc) -> new;
    lvars proc, new, _state, _nstate, _size, _flags;
    proc!PS_FLAGS -> _flags;
    if _nonzero(_flags)
    and (_flags _bitst (_:M_PS_DEAD _biset _:M_PS_SUSPENDING)
        or (_flags _bitst _:M_PS_ACTIVE
            and not(proc!PS_PERM_FLAGS _bitst _:M_PS_NON_VOLATILE)))
    then
        mishap(proc, 1, 'ATTEMPT TO COPY NON-RUNNABLE PROCESS')
    endif;

    proc!PS_STATE@~PSS_DATA!RAW_SIZE -> _size;
    Get_store(@@(struct PROCESS)++ _add _size) -> new;
    _moveq(@@(struct PROCESS)++, proc@POPBASE, new@POPBASE)@~POPBASE -> _nstate;
    proc!PS_STATE@~PSS_DATA -> _state;
    _moveq(_state!RAW_SIZE, _state@POPBASE, _nstate@POPBASE) -> ;
    _size -> _nstate!RAW_SIZE;
    _nstate@PSS_DATA -> new!PS_STATE;
    _nstate@(w){proc!PS_CALLSTACK_LIM, _state} -> new!PS_CALLSTACK_LIM;
    _0 -> new!PS_FLAGS;

    if proc!PS_PLOG_STATE then
        ;;; copy its prolog saved state
        proc!PS_PLOG_STATE!RAW_SIZE -> _size;   ;;; state size
        Get_store(_size) ->> _nstate -> new!PS_PLOG_STATE;
        _moveq(_size, proc!PS_PLOG_STATE@POPBASE, _nstate@POPBASE) ->
    endif
enddefine;


;;; --- PROCESS KEY ------------------------------------------------------

define lconstant Ps_hash() with_nargs 1;
    ->; -1
enddefine;

constant
    process_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_PROCESS,       ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "process",              ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isprocess,              ;;; K_RECOGNISER
        WREF runproc,           ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Minimal_print,          ;;; K_SYS_PRINT
        WREF Minimal_print,     ;;; K_PRINT
        WREF Ps_hash,           ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct PROCESS)++,   ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};

;;; --- GARBAGE COLLECTION PROCEDURES ----------------------------------------

section Gc;

constant
        procedure (App_calls, App_userstack_range),
    ;

lvars
    _scan_tmp;

define App_proc_state(_proc, _state, _reloc, app_p, app_owner_p);
    lvars procedure (app_p, app_owner_p), _proc, _diff, _reloc, _state, _clim,
        _partial, _flags, _actvol;

    _state@PSS_DATA -> _state;          ;;; current state callstack address
    ;;; diff between where state was and is now
    @@(w){_state, _proc!PS_STATE} -> _diff;
    _proc!PS_CALLSTACK_LIM@(w){_diff} -> _clim; ;;; current limit address

    ;;; correct state pointers in process with extra relocation
    _state@(w)-{_reloc} -> _proc!PS_STATE;
    _clim@(w)-{_reloc} -> _proc!PS_CALLSTACK_LIM;

    _proc!PS_FLAGS -> _flags;
    _flags _bitst _:M_PS_ACTIVE
    and not(_proc!PS_PERM_FLAGS _bitst _:M_PS_NON_VOLATILE) -> _actvol;

    ;;; apply to saved user stack (if there is one)
    if not(_actvol) or _flags _bitst _:M_PS_SUSPENDING then
        App_userstack_range(_clim, _proc!PS_USERSTACK_SIZE, app_p)
    endif;

#_IF DEF STACK_GROWS_UP
    ;;; swop _state and _clim if stack inverted
    _clim, _state -> _clim -> _state;
#_ENDIF

    if (_proc!PS_CALLSTACK_PARTIAL ->> _partial) /== _NULL then
        ;;; Suspension/resumption of process interrupted to
        ;;; run dlocal expression code (must be active).
        _partial@(w){_diff} -> _partial;    ;;; where now
        _partial@(w)-{_reloc} -> _proc!PS_CALLSTACK_PARTIAL;

        ;;; For a volatile process, the callstack is only valid between
        ;;; _partial and _clim; for non-volatile, part between _state and
        ;;; _partial is also valid (providing process is not in the middle
        ;;; of suspending), in which case do whole area.
        if _actvol or _flags _bitst _:M_PS_SUSPENDING then
            _partial -> _state
        endif
    elseif _actvol then
        _clim -> _state
    endif;

    ;;; apply to saved call stack -- _state points to SF_PROC_BASE
    chain(_state@~SF_PROC_BASE, _clim, _NULL, app_p, app_owner_p, App_calls)
enddefine;


define App_process(_proc, app_p, app_owner_p, _scan_phase);
    lvars procedure (app_p, app_owner_p), _proc, _state, _scan_phase;
    returnif(_proc!PS_FLAGS _bitst _:M_PS_DEAD);        ;;; dead process

    if _scan_phase and (_proc!PS_PLOG_STATE ->> _state) then
        _proc!PS_FLAGS -> _state!PLGPS_FLAGS
    endif;
    app_p(_proc@PS_PLOG_STATE);

    returnif((_proc!PS_STATE ->> _state) == _NULL);;
    _state@~PSS_DATA -> _state;

    if _state >=@(w) _lowest_garbageable then
        if _scan_phase then
            _proc -> _state!PSS_PROCESS;
            _state -> _scan_tmp;
            chain(ident _scan_tmp, app_p)
        endif
    else
        chain(_proc, _state, _0, app_p, app_owner_p, App_proc_state)
    endif
enddefine;

define App_plog_proc_state(_plgstate, app_p);
    lvars procedure app_p, _plgstate, _flags, _trail, _size;
    _plgstate!PLGPS_FLAGS -> _flags;
    if not(_flags _bitst _:M_PS_ACTIVE)
    or _flags _bitst _:M_PS_SUSPENDING
    or _plgstate!PLGPS_PERM_FLAGS _bitst _:M_PS_NON_VOLATILE
    then
        _plgstate!PLGPS_TRAIL_SIZE -> _size;
        _plgstate@PLGPS_TRAIL -> _trail;
        ;;; do trail and continuation stack
        App_range(_trail, _size, app_p);
        chain(_trail@(w){_size}, _plgstate!PLGPS_CONTN_SIZE, app_p,
                                                    PBA_WEAK App_plog_contns)
    endif
enddefine;

endsection;     /* Gc */

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Sep 12 1991
        Moved Rawstruct_getsize to getstore.p
--- John Gibson, Jun 26 1991
        Changed is_v*ed_im_process to is_system_process
--- John Gibson, Jun 19 1991
        Changed -isliveprocess- to return false for a non-process instead
        of mishapping.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Nov 14 1989
        Changes for segmented callstack.
--- John Gibson, Sep 15 1989
        Overhauled whole process mechanism: replaced -consprocto- with
        -consproc_to-, which is better in combination with new
        -suspend_chain- (also -ksuspend_chain- new and -saveproc- now in
        system rather than library; old -consprocto- moved to library).
            All processes can now be volatile or non-volatile (volatile
        meaning stacks disappears from process record while process is
        running).
--- John Gibson, Apr 10 1989
        -barrier_count- into section Sys$-Plog
--- John Gibson, Jul 28 1988
        Fixed non-weak prolog reference
--- John Gibson, Jul  5 1988
        New treatment of return addresses in stack frames (all return
        addresses in process stack frames are now relative).
--- John Gibson, Jul  3 1988
        Replaced double word field PS_PARTIAL_SAVE with single word field
        PS_PARTIAL_RETURN holding the relative return address set up by
        process swap in/out subroutines. This means that the garbage
        collector now doesn't have to do anything about relocating the
        partial-state return address.
--- John Gibson, Feb 25 1988
        Sectionised processes and garbage collector routines, etc.
--- John Gibson, Feb 11 1988
        nonexported Gc_app_process
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
 */
