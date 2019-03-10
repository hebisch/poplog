/* --- Copyright University of Sussex 2004. All rights reserved. ----------
 > File:            C.all/src/errors.p
 > Purpose:
 > Author:          John Gibson et al (see revisions)
 > Documentation:   REF *MISHAPS
 */

;;;------------------- ERROR PROCEDURES ------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'signals.ph'
#_INCLUDE 'memseg.ph'

#_IF DEF UNIX
#_INCLUDE 'unixdefs.ph'
#_ELSEIF DEF VMS
#_INCLUDE 'vmsdefs.ph'
#_ELSEIF DEF WIN32
#_INCLUDE 'win32defs.ph'
#_ENDIF

constant
        procedure (recursive_front, is_poplog_item, conslist, destlist,
        word_string, sys_message_printf)
    ;

vars
        procedure (pop_exception_final),
        pop_exit_ok, poplineprefix,
        _call_stack_lim, _pop_callstack_lim
    ;

weak constant
        active (vedusewindows),

    ;

global constant

    active
        ;;; added A.S.29 Jun 2003
        DO_ERRNO_VAL
       ;

weak vars
        procedure (sysCOMPILE, wved_mishap_reset),
        popfilename, pop_first_setpop
    ;

section $-Sys;

vars
        charout_dev, charerr_dev,
        _lowest_heap_seg, _inhibit_gc
    ;

weak constant
        procedure (Vm$-Pr_compiling_list),
        raw_dev_out_undef
    ;

weak vars
        charin_dev
    ;

endsection;


;;; ---------------------------------------------------------------------



section $-Sys => prmishap, sysprmishap, sys_pr_message,
                 popsyscall, popmessages, popmishaps, popwarnings,
                 pop_mishap_doing_lim, pop_message_trans,
                 pop_message_min_detail,
                 sys_pr_exception, pop_pr_exception,
                 sys_exception_final, pop_exception_final,
                 sys_exception_handler, pop_exception_handler,
                 sys_raise_exception, mishap,
                 sysprwarning, prwarning,
                ;

vars procedure (
    pop_message_trans       = erase,
    );

vars
    popsyscall              = false,
    popmishaps              = [],
    popmessages             = [],
    popwarnings             = [],
    pop_mishap_doing_lim    = false,
    pop_message_min_detail  = 0,

    _memory_is_corrupt      = false,    ;;; set true during GC
    ;


;;; Added by A.Sloman on advice from Waldek Hebisch 30 Jun 2003

define active DO_ERRNO_VAL() -> _x;
    _extern get_libc_errno() -> _x;
enddefine;


define updaterof active DO_ERRNO_VAL(_x);
    _extern set_libc_errno(_x) -> _
enddefine;


define Exec_nonpd(item) with_props '(Sys$-Exec_nonpd)';
    lvars item;
;;;        _extern printf('Exec_nonpd %p\n', item) -> _;
;;;        _extern fflush(_0) -> _;
    mishap(item, 1, 'enp: EXECUTING NON-PROCEDURE')
enddefine;
;;;
define updaterof Exec_nonpd(item) with_props '(->Sys$-Exec_nonpd)';
    lvars item;
    if isprocedure(item) then
        mishap(item, 1, 'enp: EXECUTING NON-EXISTENT UPDATER')
    else
        mishap(item, 1, 'enp: EXECUTING NON-PROCEDURE (AS UPDATER)')
    endif
enddefine;

define Exec_only_updater(p);
    lvars p;
    mishap(p, 1, 'EXECUTING PROCEDURE WITH ONLY UPDATER DEFINED')
enddefine;

define Bad_err_interrupt(ip) -> ip;
    lvars ip;
    unless ip == sysexit or ip == fast_sysexit or ip!PD_PROPS == sysexit then
        setpop -> ip
    endunless
enddefine;

define Save_errno_apply(procedure p);
    lvars _se1, _se2;
    dlocal 0 % SAVE_ERRNO(_se1, _se2), RESTORE_ERRNO(_se1, _se2) %;
    p();
enddefine;

    /*  Call stack overflow
    */
define Call_overflow();
    dlocal  interrupt = Bad_err_interrupt(interrupt),
            _disable  = _disable _biset _DISABLE_STKCHECKS,
            pr = sys_syspr,
        ;

    mishap(0,if _call_stack_hi@(csword)-[_pop_callstack_lim]
                                        <@(csword) _call_stack_lim then
                'rom: NO MORE MEMORY AVAILABLE (needing callstack space)',
                'callstack:mem-nomore'
             else
                'rle: RECURSION LIMIT (pop_callstack_lim) EXCEEDED',
                'callstack:mem-limit'
             endif)
enddefine;

    /*  User stack underflow
    */
define User_underflow();
    mishap(0, 'ste: STACK EMPTY (missing argument? missing result?)')
enddefine;

    /*  User stack overflow
    */
define User_overflow();
    dlocal interrupt, _disable;
    returnif(_disable _bitst _DISABLE_STKCHECKS);
    _disable _biset _DISABLE_STKCHECKS -> _disable;
    Bad_err_interrupt(interrupt) -> interrupt;
    Alloc_user_space(@@(w)[_2:1e12])
enddefine;

define Set_standard_char_devs();
    if testdef pop_charin_device then
        weakref[pop_charin_device] dev_in
                                -> weakref[pop_charin_device] charin_dev
    endif;
    dev_out -> charout_dev;
    dev_err -> charerr_dev
enddefine;

define lconstant Write_err_mess(printf_str, mess);
    lvars   printf_str, mess;
    dlocal  interrupt = Bad_err_interrupt(interrupt),
            cucharout, cucharerr,
            weakref[pop_charin_device]  charin_dev,
            charout_dev,
            charerr_dev,
        ;

    Set_standard_char_devs();
    charerr ->> cucharerr -> cucharout;

    if isdevice(popdeverr) then
        printf((), printf_str);
        sysflush(popdeverr);
    endif;

    mishap(0, mess)
enddefine;


#_IF DEF UNIX

define System_error(_sig, _fault_code, _pc, _fault_addr);
    lvars ms, _sig, _fault_code, _pc, _fault_addr, _nbytes;

    if  _sig == _:SIG_EMT
    and _nonzero(_extern __pop_malloc_exhausted:data!(int) ->> _nbytes) then
        ;;; malloc block allocated for Poplog system calls ran out of space
        _0 -> _extern __pop_malloc_exhausted:data!(int);
        chain(_pint(_nbytes), 1,
                    'malloc: INSUFFICIENT MEMORY FOR INTERNAL POPLOG USE',
                    'sys-malloc:mem-nomore',
                    mishap)
    endif;

    if _sig == _:SIG_SEGV then
        ;;; segmentation violation
#_IF DEF SUNOS or DEF MIPS or DEF OSF1 or DEF NCR or DEF AIX or DEF DGUX or DEF LINUX
        ;;; _fault_addr is reliable
        'serr: MEMORY ACCESS VIOLATION (see above)' -> ms;
        if
  #_IF DEF AIX      ;;; stupid AIX doesn't have etext
        (_:LOWEST_CODE_ADDRESS <=@(w) _pc and _pc <@(w) _16:20000000)
        or
  #_ELSEIF DEF LOWEST_CODE_ADDRESS
        (_:LOWEST_CODE_ADDRESS <=@(w) _pc and _pc <@(w) _extern etext)
        or
  #_ENDIF
        (_LOWEST_POP_ADDRESS <=@(w) _pc and _pc <@(w) _open_seg_free_ptr)
        then
            ;;; _pc within range of POPLOG code
            if _fault_addr@(w.t->vpage) == _userhi
#_IF DEF DGUX
                or _fault_code == _2
#_ENDIF
            then
                ;;; assume userstack underflow
                chain(User_underflow)
            elseif _LOWEST_POP_ADDRESS@POPBASE <=@(w) _fault_addr
            and _fault_addr <@(w) _open_seg_free_ptr then
                ;;; could be writing to _fault_addr within POPLOG
                'serr: MEMORY ACCESS VIOLATION (attempt to alter non-writeable system structure?)'
                        -> ms
            endif
        endif;

        ;;; else print Access Violation message
        Write_err_mess(
            _fault_code, _fault_addr, _pc,
            '\n<<<<<<< Access Violation: PC = %x, Addr = %x, Code = %d >>>>>>>\n\n',
            ms
            );
        return
#_ELSE
        ;;; else just assume SEGV is user stack underflow
        chain(User_underflow)
#_ENDIF
    endif;

    'serr: SYSTEM ERROR (see above)' -> ms;
#_IF DEF VAX and DEF BERKELEY
    if _sig == _:SIG_BUS then
        ;;; bus error -- could be writing non-writeable structure
        'serr: SYSTEM ERROR (attempt to alter non-writeable system structure?)'
                -> ms
    endif;
#_ENDIF

    Write_err_mess(
        _pc, _sig,
        '\n\t<<<<<<< System Error: Signal = %d, PC = %x >>>>>>>\n\n',
        ms
        )
enddefine;


#_ELSEIF DEF VMS

define System_error(_sig, _fault_code, _pc, _fault_addr, _sigarray);
    lvars ms, _sig, _fault_code, _pc, _fault_addr, _sigarray;
    'serr: SYSTEM ERROR (see above)' -> ms;

    if _sig == _:'SS$_ACCVIO' then
        ;;; access violation

        'serr: MEMORY ACCESS VIOLATION (see above)' -> ms;
        if _LOWEST_POP_ADDRESS <=@(w) _pc and _pc <@(w) _open_seg_free_ptr then
            ;;; _pc within range of POPLOG code
            if _fault_addr@(w.t->vpage) == _userhi then
                ;;; assume userstack underflow
                chain(User_underflow)
            elseif _LOWEST_POP_ADDRESS@POPBASE <=@(w) _fault_addr
            and _fault_addr <@(w) _open_seg_free_ptr
            and _fault_code == _4
            then
                ;;; could be writing to _fault_addr within POPLOG
                'serr: MEMORY ACCESS VIOLATION (attempt to alter non-writeable system structure?)'
                        -> ms
            endif
        endif;
    endif;

    ;;; use VMS $putmsg to produce error message
    _extern sys\$putmsg(_sigarray, , ,) -> ;

    Write_err_mess('', ms)
enddefine;


#_ELSEIF DEF WIN32

define System_error(_sig, _exn_code, _pc, _fault_addr, _rw_flag);
    lvars _sig, _exn_code, _pc, _fault_addr, _rw_flag;
    if _sig == _:SIG_SEGV then
        ;;; access violation
        lvars msg = 'serr: MEMORY ACCESS VIOLATION (see above)';
        if _LOWEST_POP_ADDRESS <=@(w) _pc and _pc <@(w) _open_seg_free_ptr then
            ;;; _pc within range of POPLOG code
            if _fault_addr@(w.t->vpage) == _userhi then
                ;;; assume userstack underflow
                chain(User_underflow);
            elseif _nonzero(_rw_flag)
            and _LOWEST_POP_ADDRESS@POPBASE <=@(w) _fault_addr
            and _fault_addr <@(w) _open_seg_free_ptr
            then
                ;;; could be writing to _fault_addr within POPLOG
                'serr: MEMORY ACCESS VIOLATION (attempt to alter non-writeable system structure?)'
                        -> msg;
            endif;
        endif;
        Write_err_mess(
            _pc, _fault_addr,
            if _zero(_rw_flag) then 'reading from' else 'writing to' endif,
            '\n>>>>>>> Access Violation: %S address 16:%x, PC = 16:%x\n\n',
            msg);
    else
        lvars exn_msg =
            if _sig == _:SIG_ILL then
                'illegal instruction'
            elseif _sig == _:SIG_BUS then
                'datatype misalignment'
            elseif _sig == _:SIG_FPE then
                'floating point error'
            else
                nullstring
            endif;
        Write_err_mess(
            _pc, exn_msg, _exn_code,
            '\n>>>>>>> Exception: 16:%x (%S), PC = 16:%x\n\n',
            'serr: SYSTEM ERROR (see above)');
    endif;
enddefine;

#_ELSE_ERROR
#_ENDIF


;;; --- PRINTING WARNING/MISHAPS -------------------------------------------

lconstant DECLVAR_IDSTRING = 'vm-ident:name-ref-none';
lvars raise_exception_idstring;

define lconstant Message_sprintf(string, arglist) -> (string, arglist);
    lvars string, arglist;
    dlocal cucharout = identfn;
    consstring(#| sys_message_printf(string, arglist) -> arglist |#)
                        -> string
enddefine;

define sys_pr_message(count, message, idstring, severity);
    lvars   count, props, call_pdr, first, message, arglist, culprits, detail,
            severity, ismis, message_header, idstring, n, item, re_idstring,
            _sframe, _sflim, _nprinted, _next, _se1, _se2;

    dlocal  cucharout, cucharerr, charerr_dev;
    dlocal  pop_pr_quotes = false, poplineprefix;

    define lconstant Conceal_call(props);
        lvars props, prefix, l;
        lconstant conceal_prefixes =
            ['sys' 'ved' 'wved' 'xved' 'pop11_' 'subsys'];
        if isword(props) then
            returnif(props == "mishap" or props == "setpop") (true);
            fast_for prefix in conceal_prefixes do
                returnif(isstartstring(prefix, props)) (true)
            endfor
        elseif isstring(props) then
            ;;; for system procedures whose pdprops are a string of the form
            ;;; '( pathname )' for identification by popc
            returnif( (datalength(props) ->> l) fi_> 2
                        and props(1) == `(` and props(l) == `)`) (true)
        endif;
        false
    enddefine;

    if cucharerr == charerr and not(isdevice(charerr_dev)) then
        if isdevice(dev_err) then
            dev_err -> charerr_dev;
        elseif isdevice(dev_out) then
            dev_out -> charerr_dev;
        else
            ;;; no output device available
            erase -> cucharerr;
        endif;
    endif;
    cucharerr -> cucharout;

    SAVE_ERRNO(_se1, _se2);
    if _memory_is_corrupt then
        ;;; avoid constructing any pairs
        #_< writeable [0 0 0] >_# -> _free_pairs;
        if count fi_> 3 then erasenum(count fi_- 3), 3 -> count endif
    endif;
    conslist(count) -> arglist;

    Check_string(idstring);
    false ->> message_header -> detail;
    unless isstring(message) then
        unless isvector(message) and (datalength(message)->>n) fi_> 0 then
            mishap(message, 1, 'STRING OR NON-EMPTY VECTOR NEEDED')
        endunless;
        if n fi_> 1 then
            fast_subscrv(2,message) -> item;
            item -> if isinteger(item) then detail else message_header endif;
            if n fi_> 2 then
                fast_subscrv(3,message) -> item;
                item -> if isinteger(item) then detail else message_header endif;
            endif
        endif;
        fast_subscrv(1,message) -> message
    endunless;

    Check_string(message);
    unless _memory_is_corrupt then
        pop_message_trans(message, idstring) -> message
    endunless;

    if message_header then
        Check_string(message_header)
    else
        if _memory_is_corrupt then 'FATAL ERROR -'
        elseif severity == `I` then 'NOTE -'
        elseif severity == `W` then 'WARNING -'
        else 'MISHAP -'
        endif -> message_header
    endif;
    1 -> n;             ;;; number of newlines
    if detail then
        if isinteger(detail) then
            detail fi_>> 4 -> n;
            detail fi_&& 16:F -> detail
        endif;
        Check_integer_range(detail, 0, 5)
    else
        if severity == `I` or severity == `W` then 4 else 5 endif -> detail
    endif;
    if detail fi_< pop_message_min_detail then
        pop_message_min_detail -> detail
    endif;

        5 -> detail;

    if not(_memory_is_corrupt) and severity /== `I`
    and testdef vedusewindows and weakref vedusewindows then
        weakref[vedusewindows] wved_mishap_reset()
    endif;

    /* PRINT MESSAGE */
    fast_repeat n times cucharout(`\n`) endrepeat;
    #_< conspair(';;;\s', false) >_# -> poplineprefix;

    sys_syspr(message_header);
    unless message_header = nullstring then cucharout(`\s`) endunless;
    RESTORE_ERRNO(_se1, _se2);

    if detail fi_>= 1 then
        sys_message_printf(message, arglist) -> culprits;   ;;; unused args
        listlength(culprits) -> count
    endif;
    cucharout(`\n`);

    /* PRINT INVOLVING */
    if detail fi_>= 2
    and count fi_> 0 then
        printf('INVOLVING:\s\s');
        true -> pop_pr_quotes;
        applist(culprits, spr);
        cucharout(`\n`);
        false -> pop_pr_quotes
    endif;

    /* PRINT FILENAME */
    if detail fi_>= 3
    and testdef popfilename and weakref popfilename then
        printf(weakref[popfilename] poplinenum, weakref popfilename,
                        'FILE\s\s\s\s\s:\s\s%P\s\s\sLINE NUMBER:\s\s%P\n')
    endif;

    /* PRINT COMPILING */
    if detail fi_>= 4
    and testdef sysCOMPILE then
        weakref[sysCOMPILE] Vm$-Pr_compiling_list('COMPILING:\s\s')
    endif;

    /* PRINT DOING */
        printf('PRINT DOING\n');
    if true /* detail fi_>= 5
    and (not(pop_mishap_doing_lim) or pop_mishap_doing_lim fi_> 0) */ then
        raise_exception_idstring -> re_idstring;
        false -> first;
        0 ->> count -> _nprinted;
        _caller_sp_flush() -> _sframe;
        _call_stack_seg_hi -> _sflim;
        printf('DOING\s\s\s\s:\s\s');
        repeat
            if _sframe == _sflim then
                if _sflim == _call_stack_hi then
                    if _in_external_control then
                        printf('<Outer External Calls>\s')
                    endif;
                    quitloop
                endif;
                ;;; next callstack seg
                _sframe!SF_NEXT_SEG_HI -> _sflim;
                _sframe!SF_NEXT_SEG_SP -> _sframe;
                printf('<External Calls>\s')
            endif;
            _CHECKINTERRUPT;
            if _nprinted == pop_mishap_doing_lim then
                if _nprinted /== 0 then appdata('...', cucharout) endif;
                quitloop
            endif;
            _sframe!SF_OWNER -> call_pdr;
            recursive_front(call_pdr!PD_PROPS) -> props;
            if props and not(Conceal_call(props)) then
                pr(props);
                unless first then props -> first endunless
            elseif popsyscall then
                if props or call_pdr >=@(w) _system_end then
                    if call_pdr == sys_raise_exception then
                        printf('\n\t');
                        pr(props);
                        cucharout(`(`);
                        pr(re_idstring);
                        printf(')\s');
                        _sframe!(csword){
                            Dlocal_frame_offset(ident raise_exception_idstring,
                                call_pdr, false)} -> re_idstring;
                        goto NEXT
                    else
                        pr(props)
                    endif
                elseif isinteger(popsyscall) then
                    printf(call_pdr, '%x')
                else
                    goto NEXT
                endif
            else
                goto NEXT
            endif;
            _nprinted fi_+ 1 -> _nprinted;
            1 -> count;
            while (_nextframe(_sframe) ->> _next) <@(csword) _sflim
            and _next!SF_OWNER == call_pdr do
                _CHECKINTERRUPT;
                _next -> _sframe;
                count fi_+ 1 -> count
            endwhile;
            if count fi_> 1 then
                cucharout(`(`);
                cucharout(`*`);
                sys_syspr(count);
                cucharout(`)`);
            endif;
            cucharout(`\s`);

        NEXT:
            _nextframe(_sframe) -> _sframe
        endrepeat;

        cucharout(`\n`)
    endif;

    returnif(_memory_is_corrupt or severity == `I`);

    if idstring = DECLVAR_IDSTRING then
        if popwarnings and arglist /== [] then
            fast_front(arglist) :: popwarnings -> popwarnings
        endif
    endif;

    ;;; Add abbreviated error information to popmishaps and/or popmessages
    popmishaps and severity /== `W` -> ismis;
    if popmessages or ismis then
        Message_sprintf(message, arglist) -> (message, );
        [%  message_header, message,
            'INVOLVING' :: culprits,
            if testdef popfilename and weakref popfilename then
                'FILE: ' sys_>< weakref popfilename sys_>< ' LINE: '
                sys_>< weakref[popfilename] poplinenum
            endif,
            unless pop_mishap_doing_lim == 0 then
                'DOING : ', if first then sys_>< first endif
            endunless
        %],
        ;;; assign to popmishaps if neccesary
        if ismis then ->> popmishaps endif;
        ;;; and then assign to popmessages if necessary
        -> if popmessages then popmessages else endif;
    endif
enddefine;

define sysprwarning =
    sys_pr_message(% 1, {'%DECLARING VARIABLE %P' '' 16:01},
                            DECLVAR_IDSTRING, `W` %)
enddefine;
;;;
vars procedure prwarning = sysprwarning;

define sysprmishap(count, message);
    lvars count, message;
    sys_pr_message( if isinteger(count) then count, message
                    else destlist(message), count
                    endif, nullstring, `E`)
enddefine;
;;;
vars procedure prmishap = sysprmishap;

define sys_pr_exception(count, message, idstring, severity);
    lvars   count, message, idstring, severity, list;

    if severity == `W` and prwarning /== sysprwarning
    and is_poplog_item(prwarning) and idstring = DECLVAR_IDSTRING then
        ;;; call prwarning for backward compatibility
        prwarning(/*word*/);
        return
    elseif (severity /== `I` and severity /== `W`)
    and prmishap /== sysprmishap and is_poplog_item(prmishap) then
        ;;; error -- call prmishap for backward compatibility
        conslist(count) -> list;
        if isvector(message) then subscrv(1,message) -> message endif;
        prmishap(Message_sprintf(message, list));
        return
    endif;

    ;;; drop thru for standard print
    chain(count, message, idstring, severity, sys_pr_message)
enddefine;
;;;
vars procedure pop_pr_exception = sys_pr_exception;

define sys_exception_final(/* count, message, idstring, severity */)
                                with_nargs 4;
    pop_pr_exception();
    true
enddefine;
;;;
vars procedure pop_exception_final = sys_exception_final;


lvars
    last_chance_final   = false,
    _final_sframe   = _NULL,
    _final_sflim    = _NULL,
    flushing_dev    = false,
;

define Last_chance_exception_final();
    dlocal  last_chance_final = Bad_err_interrupt(interrupt), pr = sys_syspr;
    sys_pr_message();
    true
enddefine;

define sys_exception_handler(count, message, idstring, severity);
    lvars   count, message, idstring, severity, procedure p, is_error,
            _sframe, _offs;
    dlocal  interrupt, pr, _final_sframe, _final_sflim;

    ;;; in case of error while flushing a device
    define lconstant flush_dev(dev_id, std_dev);
        lvars dev = fast_idval(dev_id), dev_id, std_dev;
        if dev == flushing_dev then
            ;;; error flushing it -- replace with standard dev
            std_dev ->> dev -> fast_idval(dev_id);
            returnif(dev == flushing_dev)
        endif;
        if isdevice(dev) then
            dlocal flushing_dev = dev;
            sysflush(dev)
        endif
    enddefine;

    severity /== `I` and severity /== `W` -> is_error;

    if is_error then
        ;;; _inhibit_gc is Sysgarbage when inside it
        unless _inhibit_gc == Sysgarbage then false -> _inhibit_gc endunless;

        false -> weakref pop_first_setpop;

        unless _memory_is_corrupt then
            Clear_ast_queue(false);
            _CHECKINTERRUPT
        endunless;

        if pop_exit_ok then
            testdef popdevin and isdevice(weakref popdevin)
                and systrmdev(weakref popdevin) == true -> pop_exit_ok;
        endif;
        flush_dev(ident charout_dev, dev_out);
        if testdef poprawdevout then
            flush_dev(ident weakref[poprawdevout] raw_dev_out,
                            weakref[poprawdevout] raw_dev_out_undef)
        endif;

        unless is_poplog_item(pr) then sys_syspr -> pr endunless;
        unless is_poplog_item(interrupt) then setpop -> interrupt endunless;
    endif;

    repeat
        if last_chance_final then
            last_chance_final -> p
        elseif _final_sframe == _NULL then
            ;;; first call of sys_exception_handler
            _caller_sp_flush() -> _final_sframe;
            _call_stack_seg_hi -> _final_sflim;
            pop_exception_final -> p
        else
            ;;; search for next saved value of pop_exception_final
            repeat
                if (_final_sframe ->> _sframe) == _final_sflim then
                    if _sframe == _call_stack_hi then
                        ;;; no handlers wanted to know -- call the default
                        dlocal last_chance_final = Last_chance_exception_final;
                        sys_exception_final -> p;
                        quitloop
                    endif;
                    _sframe!SF_NEXT_SEG_HI -> _final_sflim;
                    _sframe!SF_NEXT_SEG_SP ->> _sframe -> _final_sframe
                endif;
                _nextframe(_sframe) -> _final_sframe;
                ;;; returns additive offset if id is a local, zero if not
                if _nonzero(Dlocal_frame_offset(ident pop_exception_final,
                                _sframe!SF_OWNER, false) ->> _offs) then
                    _sframe!(csword){_offs} -> p;   ;;; saved val
                    quitloop
                endif
            endrepeat
        endif;

        ;;; call handler
        unless is_poplog_item(p) then sys_exception_final -> p endunless;
        quitif(p((), count, message, idstring, severity))
    endrepeat;

    flush_dev(ident charerr_dev, dev_err);
    returnunless(is_error) (true);

    interrupt -> p;
    Bad_err_interrupt(interrupt) -> interrupt;
    p();
    setpop()
enddefine;
;;;
vars procedure pop_exception_handler = sys_exception_handler;


lvars
    _handler_sframe = _NULL,
    _handler_sflim  = _NULL,
;

define sys_raise_exception(count, message, severity);
    lvars   count, message, idstring, severity, procedure p, _sframe, _offs,
            _special_seg = _NULL;
    dlocal  raise_exception_idstring, _handler_sframe, _handler_sflim;

    if isinteger(count) then
        ;;; count, mess
        nullstring -> raise_exception_idstring
    elseif isstring(message) then
        ;;; count, mess, idstring_or_false
        ((), count, message) -> (count, message, raise_exception_idstring)
    else
        ;;; mess, itemlist
        destlist(message), count -> (count, message);
        nullstring -> raise_exception_idstring
    endif;

    if _memory_is_corrupt then
        ;;; in the middle of GC -- treat this as fatal
        if pop_exception_handler == fast_sysexit
        or last_chance_final == fast_sysexit then
            ;;; already tried to print the error -- give up
            fast_sysexit()
        else
            false -> pop_mishap_doing_lim;
            1 -> popsyscall;
            sys_syspr -> pr;
            Last_chance_exception_final -> last_chance_final;
            fast_sysexit ->> interrupt -> pop_exception_handler;
            sys_exception_handler(count, message, raise_exception_idstring, `E`)
        endif
    endif;

    if _curr_heap_seg /== _NULL
    and _curr_heap_seg!SEG_FLAGS _bitst _M_SEG_SPECIAL then
        ;;; using special segment (e.g fixed or save/restore seg) -- reset
        ;;; to the saved value
        _curr_heap_seg -> _special_seg;
        Set_curr_heap_seg(_saved_curr_heap_seg)
    endif;

    repeat
        if _handler_sframe == _NULL then
            ;;; first call of sys_raise_exception
            _caller_sp_flush() -> _handler_sframe;
            _call_stack_seg_hi -> _handler_sflim;
            pop_exception_handler -> p
        else
            ;;; search for next saved value of pop_exception_handler
            repeat
                if (_handler_sframe ->> _sframe) == _handler_sflim then
                    if _sframe == _call_stack_hi then
                        ;;; no handlers wanted to know -- call the default
                        sys_exception_handler -> p;
                        quitloop
                    endif;
                    _sframe!SF_NEXT_SEG_HI -> _handler_sflim;
                    _sframe!SF_NEXT_SEG_SP ->> _sframe -> _handler_sframe
                endif;
                _nextframe(_sframe) -> _handler_sframe;
                ;;; returns additive offset if id is a local, zero if not
                if _nonzero(Dlocal_frame_offset(ident pop_exception_handler,
                                _sframe!SF_OWNER, false) ->> _offs) then
                    _sframe!(csword){_offs} -> p;   ;;; saved val
                    quitloop
                endif
            endrepeat
        endif;

        ;;; call handler
        unless is_poplog_item(p) then sys_exception_handler -> p endunless;
        if p((), count, message, raise_exception_idstring, severity) then
            ;;; handled
            quitif(severity == `R` or severity == `W` or severity == `I`);
            ;;; normal return not allowed for error
            mishap(raise_exception_idstring, message, 2,
                    'ILLEGAL RETURN FROM EXCEPTION HANDLER',
                                    'exception-illret:control')
        endif
    endrepeat;

    if _special_seg /== _NULL then
        _curr_heap_seg -> _saved_curr_heap_seg;     ;;; resave
        Set_curr_heap_seg(_special_seg)
    endif
enddefine;
;;;
define mishap = sys_raise_exception(% `E` %) enddefine;


define Syserr_mishap(mess);
    lvars mess;
    chain('%' sys_>< mess sys_>< ' (%M)', mishap)
enddefine;

define Readerr_mishap(dev, _retcount, mess);
    lvars dev, _retcount, mess;
    chain(mess, dev!D_FULL_NAME, 2, if _neg(_retcount) then '%%P (%M)'
                                    else '%%P (premature end-of-file)'
                                    endif, mishap)
enddefine;

define Dlexpr_stackerr();
    mishap(0, 'STACK UNDERFLOW/OVERFLOW IN DLOCAL EXPRESSION');
    setpop()
enddefine;

define Array$-Sub_error(item);
    lvars item;
    mishap(item, 1, 'INVALID ARRAY SUBSCRIPT',
            if isinteger(item) then 'array-subscr:type-intrange'
            else 'array-subscr:type-integer'
            endif)
enddefine;

    ;;; go_on arg is left on stack
define Go_on_outrange =
    mishap(%1, 'go_on: INTEGER OUT OF RANGE', 'go_on:type-intrange'%)
enddefine;

define Record_needed(/*item, key*/) with_nargs 2;
    lvars string, dw = ()!K_DATAWORD!W_STRING, _n, _c;
    dw sys_>< ' NEEDED' -> string;
    ;;; (saves using lowertoupper)
    _pint(string!V_LENGTH) -> _n;
    until _n == 0 do
        fast_subscrs(_n, string) -> _c;
        if `a` fi_<= _c and _c fi_<= `z` then
            _c fi_- 32 -> fast_subscrs(_n, string)
        endif;
        _n fi_- 1 -> _n
    enduntil;
    mishap((), 1, string, ':type-' sys_>< dw)
enddefine;

define Inline_checkr_procedure();
    unless isprocedure(dup()) then
        mishap((), 1, 'PROCEDURE NEEDED (e.g. assigning to procedure identifier)',
                    ':type-procedure')
    endunless
enddefine;

define Inline_checkr_integer();
    unless isinteger(dup()) then
        mishap((), 1, 'INTEGER NEEDED (e.g. for go_on)', ':type-integer')
    endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec 31 2004
    Added the documentation below for changes involving ERRNO. macro

--- Aaron Sloman and Waldek Hebisch 29 Jun 2003

    Introduced the following active variable and its updater:

        define active DO_ERRNO_VAL();

        define updaterof active DO_ERRNO_VAL(_x);

    The use of 'errno' which has been deprecated for some time is no longer
    supported. So the old use is simulated using these two procedures defined
    in $popexternlib/c_core.c (thanks to Waldek Hebisch):
        int get_libc_errno(void)
        int set_libc_errno(int x)

    In $popsrc/unixdefs.ph this macro definition is no longer used
        lconstant macro _ERRNO = [_extern errno:data!(int)];

    Instead it is defined thus, to invoke the above active variable.
        lconstant macro _ERRNO = [DO_ERRNO_VAL];

    This also requires a change to LIB unix_sockets

--- Robert Duncan, Jan  6 2000
        Various changes to guard against mishaps which occur during system
        setup, before the standard I/O devices have been created.
--- Julian Clinton, Jun 16 1998
        Modified DG/UX case for user underflow checking.
--- Robert Duncan, Jun 05 1998
        Added case for DG/UX in System_error
--- John Gibson, May 22 1997
        Added flush_dev in sys_exception_handler to stop it trying to
        re-flush a device if an exception occurs while trying to flush
        the device.
--- John Gibson, Feb 14 1997
        String16 changes
--- Robert Duncan, Dec  3 1996
        Added Save_errno_apply
--- John Gibson, Oct 25 1996
        Added include for vmsdefs.ph
--- Robert Duncan, Sep 10 1996
        Added case for NCR in System_error
--- John Gibson, Jun 11 1996
        Fixed algorithm for searching up stack frames in sys_raise_exception
        and sys_exception_final.
        Made sys_pr_exception remove new-style printf % field specifiers
        before calling prmishap.
--- John Gibson, Apr 13 1996
        Added pop_message_min_detail. Replaced sysio*message with %M etc.
--- John Gibson, Apr  1 1996
        Changed declaring variable id-string, and added some other mishap
        id-strings.
--- John Gibson, Mar  4 1996
        Changed sys_exception_handler so that it calls all local definitions
        of pop_exception_final. Default pop_exception_final is
        sys_exception_final, which just calls pop_pr_exception and returns
        true; default pop_pr_exception is sys_pr_exception.
--- John Gibson, Feb 20 1996
        Changed sys_pr_message to use new poplineprefix
--- John Gibson, Feb 19 1996
        pop_p*r_exception -> pop_exception_final
--- John Gibson, Feb  6 1996
        Installed new exception handling mechanism
--- John Gibson, Mar 14 1995
        Added test for SIG_EMT and __pop_malloc_exhausted being nonzero in
        Unix System_error.
--- John Gibson, Feb 24 1995
        Made Bad_err_interrupt return interrupt if its pdprops are sysexit
        (used by sysexit interrupt handlers)
--- Robert John Duncan, Sep  5 1994
        Added definition of System_error for Win32
--- John Gibson, Apr 12 1994
        Added call of Clear_ast_queue in mishap
--- John Gibson, Feb 22 1993
        Made Call_overflow locally set pr to sys_syspr (re: isl-fr.4490)
--- Robert John Duncan, Jun 22 1992
        Changed the sense of the conditional compilation in -System_error-
        so as not to be B*ERKELEY only
--- John Gibson, Nov 28 1991
        Stopped -mishap- assigning false -> _inhibit_gc when inside
        Sysgarbage.
--- John Gibson, Jun 11 1991
        Added 'wved' to list of concealed-call prefixes
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Apr 30 1991
        Adjustments to messages produced by System_error
--- John Gibson, Feb  9 1991
        New version of VMS -System_error-
--- John Gibson, Dec 29 1990
        Absorbed Unix Seg_violation into System_error (now called from
        Error_signal in signals.p)
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Oct 26 1990
        Replaced popdevX with pop_charX_device in appropriate places.
--- John Williams, Oct 24 1990
        Changed %p to %P in -sys*prmessage-
--- Jonathan Meyer, Oct 19 1990
        Renamed sysprmishap sys*prmessage, taking an extra arguments indicating
        what message header to print, and amount of detail. Wrote new
        sysprmishap calling sys*prmessage.
--- Aaron Sloman, Oct 12 1990
        Changed name to wved_mishap_reset
--- Aaron Sloman, Oct  7 1990
        Changed guard on call of xprmishap_reset
--- John Gibson, Sep 28 1990
        Added interrupt check in -mishap-.
--- Aaron Sloman, Sep 22 1990
        Added call of xprmishap_reset to sysprmishap
--- John Gibson, Sep 10 1990
        Removed all local settings of -prmishap- to -sysprmishap- (except
        where -mishap- itself redefines it it for a recursive error), and
        added extra -is_poplog_item- checks in -mishap- to compensate, etc.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jun  6 1990
        Revised code for -System_error- etc. charX_dev now contain
        devices, not identifiers.
--- John Gibson, Nov 14 1989
        Changed -sysprmishap- for segmented callstack.
--- John Gibson, Sep  1 1989
        -Call_overflow- now produces two different mishap messages depending
        on whether O/S imposed callstack limit reached or not.
--- John Gibson, Aug 24 1989
        Removed not(S*IGNALS) code
--- John Gibson, Aug 16 1989
        Made -mishap- assign false to -pop_first_setpop-, and not touch
        -pop_exit_ok- if already false.
--- John Gibson, May  4 1989
        Added Inline_checkr_procedure/integer.
--- John Gibson, Mar 19 1989
        Added -Record_needed-
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Feb 15 1989
        Moved Array$-Sub_error in from arrays.p
--- John Gibson, Jan 16 1989
        Added check to -Conceal_call- for system procedures whose pdprops
        are a popc identification string
 */
