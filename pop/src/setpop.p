/* --- Copyright University of Sussex 2000.  All rights reserved. ---------
 > File:           C.all/src/setpop.p
 > Purpose:
 > Author:         John Gibson et al (see revisions)
 */

;;; ----------------- SYSTEM SETUP & SETPOP --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'signals.ph'

#_IF DEF UNIX
#_INCLUDE 'unixdefs.ph'
#_ELSEIF DEF WIN32
#_INCLUDE 'win32defs.ph'
#_ENDIF

constant
        procedure (sys_reset_signal, clearstack, sys_link_tty_params)
    ;

vars
        ;;; entry procedure -- initialised by poplink
        procedure (Pop$-Entry_Procedure),

        popdirectory, popmemused, _pop_callstack_lim, _system_stack_base
    ;

weak constant
        procedure (prolog_reset, syssave,
        Pop$-Pre_Init_Restore)
    ;

section $-Sys;

constant
        procedure (Expand_open_seg, Set_userhi_to, Lock_heap_segs,
        Get_mem_break, Clear_freelists, Callstack_reset,
        Nframe_cslen, Set_call_stack_lim, Set_standard_char_devs,
        Set_curr_heap_seg, Init_signals,
        Vms_jpi_string, Vms_jpi_int, Vms_get_image_name,
        Io$-Cons_device, Io$-Get_char, Io$-Rms_parse, Io$-Rms_search,
        Io$-Cons_stddev, Explode_substring,
                dummy_procedure_callback_helper,
        ),
        image_version
    ;

vars
        procedure (Io$-Cons_device_getdev),
        _memory_is_corrupt, Dir$-current,
    ;

weak constant
        procedure (setpop_reset, Set_writeable_prop,
        Sr$-Adjust_heap_start, Sr$-Init_restore, Plog$-Area_setup
        )
    ;

weak vars
        chain_trace, Sr$- _sr_seg_entry
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys =>
                popversion, pop_system_version,
                poparglist0, poparg0, poparglist, popenvlist,
                poppid, popusername, popdirectory, popheader,
                pop_runtime, pop_runtime_actions,
                pop_first_setpop, pop_nobanner, pop_noinit, popunderx,
                setpop,
              dummy_procedure_callback_helper
            ;

constant
    image_date      = image_date,       ;;; set up by poplink
    _image_ident    = _image_ident,     ;;;     ditto
    ;

protected vars
    popversion,
    pop_system_version,
    poparglist0,
    poparg0,
    poppid,
    popusername,
    popdirectory,
    pop_runtime         = false,
    pop_runtime_actions,    ;;; set up by poplink
    ;

vars
    poparglist,
    popenvlist,

    _in_external_control = false,
    _nr_seg_entry,
    ;

weak vars
    popheader,
    pop_first_setpop,
    pop_nobanner,
    pop_noinit,
    popunderx,
    ;

    ;;; Have to do these like this, because we can't do weak
    ;;; intialisations inside vars (not at the moment, anyway).
    false   -> weakref popheader;
    true    -> weakref pop_first_setpop;
    false   -> weakref pop_nobanner;
    false   -> weakref pop_noinit;
    false   -> weakref popunderx;


lconstant macro (
    ;;; (minimum) word length for the initial open segment (i.e. the heap)
    _OPEN_SEG_NWORDS = _32000,
    );

lvars
    _setup_done = false
    ;


define Ensure_writeable(item) -> item;
    lvars item;
    if (_curr_heap_seg == _NULL or _curr_heap_seg >=@(w) _lowest_heap_seg)
    and _setup_done and testdef Set_writeable_prop then
        weakref Set_writeable_prop(item, true) ->
    endif
enddefine;


;;; --- UNIX -------------------------------------------------------------

#_IF DEF UNIX

vars
    _init_args = _NULL,                 ;;; set up in amain.s
    ;

define lconstant Get_arg_list(_address);
    lvars _address, _arg, _char;
    [%  until _zero(_address!(<b>)++ -> _address ->> _arg) do
            ;;; _arg is the address of a null terminated string
            Consstring_bptr(_arg, _-1, CSB_FIXED)
        enduntil
    %]          ;;; return list of arg strings
enddefine;

define lconstant Get_popargs();
    ;;; get the list of argument strings
    Get_arg_list(_init_args)
enddefine;

define Opsys_exit() with_nargs 1;
    _extern _exit(if () then _0 else _1 endif) ->
enddefine;


;;; --- VMS ---------------------------------------------------------------

#_ELSEIF DEF VMS


#_IF DEF SHARED_LIBRARIES

    /*  This links in the stuff in the c_callback.c shareable image
        (allows it to be a shareable image without any dangling references).
    */
define lconstant Link_in_callback();
    lvars   _upec = _extern[WEAK] pop_external_callback,
            _pec = _extern[WEAK] _pop_external_callback;
    returnif(_zero(_upec));         ;;; user interface not linked in
    if _zero(_pec) then mishap(0, 'SYSTEM ERROR IN Link_in_callback') endif;
    _pec                          -> _upec!((w));
    _extern _pop_external_flags   -> _extern[WEAK] pop_external_flags!((int));
    _extern _pop_signals_pending  -> _extern[WEAK] pop_signals_pending!((int));
    _extern _pop_disable_flags    -> _extern[WEAK] pop_disable_flags!((int));
    _extern pop_exfunc_arg        -> _extern[WEAK] pop_exfunc_closure_arg!((w));
enddefine;

#_ENDIF

    /*  This treats the command line args in the same way as VMS C,
        but in addition, @<filename> enables args to be read from
        <filename>. Also, args with wildcards are expanded.
    */
define lconstant Get_popargs();

    define lconstant Get_foreign();
        ;;; get the foreign command line into sysstring
        _extern lib\$get_foreign(   _sysstring_desc,
                                    ,
                                    ident _sysstring_len) ->
    enddefine;

    define lconstant Parse_args(char_p);
        lvars char, in_quote, procedure char_p, _count, _flags;
        lconstant macro (
            _HAD_QUOTE  = _2:1e0,
            _@_START    = _2:1e1,
            _HAS_WILD   = _2:1e2,
            _DOT_1      = _2:1e3,
            _DOT_RESET  = _2:11e3,
            );

        define lconstant Cons_arg(_count, _flags);
            lvars string, search, fname, _ptr, _lim, _count, _flags, _got_one;
            dlvars dev;
            _CLAWBACK_SAVE;

            returnif(_zero(_count)
                     and not(_flags _bitst (_HAD_QUOTE _biset _@_START)));

            consstring(_pint(_count)) -> string;
            _flags _biclear (_HAD_QUOTE _biset _DOT_RESET) -> _flags;
            returnif(_zero(_flags)) (string);

            define lconstant read_dev();
                dlocal popprompt = '@\s';
                Parse_args(procedure(); Io$-Get_char(dev) endprocedure);
                sysclose(dev)
            enddefine;

            _stklength() -> _count;

            ;;; parse doesn't seem to work with sys$input anymore -- so just
            ;;; try a straight open first
            if _flags _bitst _@_START
            and sysopen(string, 0, false, `A`) ->> dev then
                read_dev()
            else
                unless Io$-Rms_parse(string, nullstring) ->> search then
                    Syserr_mishap(string, 1, 'INVALID FILENAME ARGUMENT')
                endunless;

                false -> _got_one;
                until (Io$-Rms_search(search) ->> fname) == termin do
                    unless fname then
                        mishap(string, 1, 'ERROR IN FILENAME SEARCH')
                    endunless;
                    -> -> ;
                    Copy_sysstring() -> fname;
                    true -> _got_one;
                    if _flags _bitst _@_START then
                        sysopen(fname, 0, false, `N`) -> dev;
                        read_dev();
                    else
                        fname
                    endif
                enduntil;
                unless _got_one then
                    mishap(string, 1, 'NO MATCHING FILENAMES')
                endunless
            endif;

            Clear_freelists(false);
            _file_tab -> _file_tab_next_free;
            _file_tab_limit -> _file_tab_close_ptr;
            Get_foreign();

            _stklength() _sub _count -> _count;
            _user_sp() -> _lim;
            _lim@(w){_count} -> _ptr;
            while _ptr >@(w) _lim do
                _ptr--!(w) -> _ptr -> string;
                Clawback(string) -> _ptr!(w)
            endwhile
        enddefine;      /* Cons_arg */

        false -> in_quote, _0 -> _count, _DOT_1 -> _flags;
        `\n` -> char;
        until char == termin do
            if in_quote then
                if char == `"` and (char_p() ->> char) /== `"` then
                    false -> in_quote, nextloop
                ;;; else "" inside quotes means "
                endif;
                char;
                _count _add _1 -> _count;
                if char == `\s` or char == `\n` then
                    in_quote fi_+ 1 -> in_quote
                else
                    0 -> in_quote
                endif
            elseif char == `\\` then
                quitif((char_p() ->> char) == termin);
                char, _count _add _1 -> _count;
                (_flags _biclear _DOT_RESET) _add _DOT_1 -> _flags
            elseif char == `\s` or char == `\t` or char == `\n` then
                Cons_arg(_count, _flags);
                false -> in_quote, _0 -> _count, _DOT_1 -> _flags;
                if char == `\n` then
                    if (char_p() ->> char) == `@` then
                        quitif((char_p() ->> char) == `\n` or char == termin);
                        _flags _biset _@_START -> _flags
                    endif;
                    nextloop
                endif
            elseif char == `"` then
                0 -> in_quote;
                (_flags _biclear _DOT_RESET) _add _DOT_1 -> _flags;
                _flags _biset _HAD_QUOTE -> _flags
            elseif _zero(_count) and char == `@` then
                _flags _biset _@_START -> _flags
            else
                unless char == `.` then
                    if `A` fi_<= char and char fi_<= `Z` then
                        ;;; translate unquoted upper to lower
                        char fi_+ 32 -> char
                    elseif char == `*` or char == `%` then
                        _flags _biset _HAS_WILD  -> _flags
                    endif;
                    _flags _biclear _DOT_RESET -> _flags
                endunless;
                _flags _add _DOT_1  -> _flags;
                char, _count _add _1 -> _count
            endif;
            char_p() -> char
        enduntil;
        if in_quote then
            erasenum(in_quote), _count _sub _int(in_quote) -> _count
        endif;
        Cons_arg(_count, _flags)
    enddefine;      /* Parse_args */

    ;;; get name of image as first arg
    lvars image_name = Vms_get_image_name();
    Get_foreign();
    dlvars _num = 1, _len = _pint(_sysstring_len);
    [%  image_name,
        Parse_args( procedure();
                        if _num fi_> _len then
                            termin
                        else
                            fast_subscrs(_num, sysstring);
                            _num fi_+ 1 -> _num
                        endif
                    endprocedure )
    %]
enddefine;      /* Get_popargs */

define Opsys_exit() with_nargs 1;
    _extern sys\$exit(if () then _:'SS$_NORMAL' else _:'SS$_ABORT' endif) ->
enddefine;

#_ELSEIF DEF WIN32

constant procedure (Expand_file_pattern, Get_user_directory, Get_user_name,
        Is_environment_variable);

    /*  Interpret the command line
    */
define lconstant Get_popargs();
    dlvars _cmd = _extern pop_get_command_line();

    define lconstant parse_args(rep);
        lvars procedure rep;

        define lconstant expand_file(name);
            lvars name, dev = sysopen(name, 0, false, `N`);
            dlocal popprompt = '@\s';
            parse_args(
                procedure();
                    Io$-Get_char(dev);
                endprocedure);
            sysclose(dev);
        enddefine;

        define lconstant cons_arg(stklen, is_file, is_pattern, path_end);
            lvars stklen, is_file, is_pattern, path_end;
            _CLAWBACK_SAVE;
            lvars arg = consstring(stacklength() fi_- stklen);
            if is_file then
                expand_file(arg);
            elseif is_pattern then
                if Expand_file_pattern(arg) == 0 then
                    mishap(arg, 1, 'NO FILES FOUND');
                endif;
            else
                arg;
            endif;
            ;;; reclaim garbage
            Clear_freelists(false);
            _file_tab -> _file_tab_next_free;
            lvars _lim = _user_sp();
            lvars _count = _stklength() _sub @@(w)[_int(stklen)];
            lvars _ptr = _lim@(w){_count};
            lvars string;
            while _ptr >@(w) _lim do
                _ptr--!(w) -> _ptr -> string;
                Clawback(string) -> _ptr!(w);
            endwhile;
        enddefine;

        lvars _c, in_text = false, in_file = false, in_quotes = false,
              in_pattern = false, path_end = false;
        until (rep() ->> _c) == termin do
            if _c == `:` or _c == `\\` then
                ;;; pattern match only works for the last component in a path
                false -> in_pattern;
                stacklength() fi_+ 1 -> path_end;
            endif;
            if in_quotes then
                if _c == in_quotes then false -> in_quotes else _c endif;
            elseif _c == `\s` or _c == `\t` or _c == `\n` or _c == `\r` then
                if in_text then
                    cons_arg(in_text, in_file, in_pattern, path_end);
                    false ->> in_text ->> in_file ->> in_pattern -> path_end;
                endif;
            else
                _c;
                unless in_text then
                    stacklength() fi_- 1 -> in_text;
                    if _c == `@` then () -> in_file endif;
                endunless;
                if _c == `'` or _c == `"` then
                    () -> in_quotes;
                elseif _c == `*` or _c == `?` then
                    true -> in_pattern;
                endif;
            endif;
        enduntil;
        if in_quotes then
            mishap(cons_arg(in_text, false, false, false), 1,
                'MISSING CLOSING QUOTE');
        elseif in_text then
            cons_arg(in_text, in_file, in_pattern, path_end);
        endif;
    enddefine;

    [%  parse_args(
            procedure();
                lvars _c;
                if _zero(_cmd!(TCHAR)++ -> _cmd ->> _c) then
                    termin;
                else
                    _pint(_c);
                endif;
            endprocedure);
    %];
enddefine;

define Opsys_exit() with_nargs 1;
    _extern pop_exit(if () then _0 else _1 endif) -> ;
enddefine;

#_ELSE_ERROR
#_ENDIF


;;; -----------------------------------------------------------------------

    /*  Turn args enclosed in ( ... ) into sublists
    */
define lconstant Make_sublists(list, top);
    lvars list, top, pair, last = false, s;
    fast_for pair on list do
        fast_front(pair) -> s;
        if s = '(' then
            Make_sublists(fast_back(pair), false)
                                    -> (fast_front(pair), fast_back(pair))
        elseif s = ')' then
            if top then
                mishap(0, 'UNMATCHED ")" IN PROCESS ARGUMENTS')
            endif;
            return( if last then
                        [] -> fast_back(last);
                        list
                    else
                        []
                    endif, fast_back(pair) )
        endif;
        pair -> last
    endfor;
    unless top then
        mishap(0, 'UNMATCHED "(" IN PROCESS ARGUMENTS')
    endunless
enddefine;

    /*  Process special command line args beginning with `%`
        Called before executing entry procedure (Setup_system below)
        and after restoring a saved image (Do_Restore in sr_sys.p).

        N.B. This procedure should not create any structures
    */
define Process_percent_args();
    lvars arg, rest, nort = false;

    false ->> weakref pop_nobanner ->> weakref pop_noinit -> weakref popunderx;

    while poparglist /== []
    and (fast_destpair(poparglist) -> (arg, rest),
        isstring(arg) and datalength(arg) /== 0
        and fast_subscrs(1, arg) == `%`)
    do
        if arg = '%nort' then
            true -> nort
        elseif arg = '%nobanner' then
            true -> weakref pop_nobanner
        elseif arg = '%noinit' then
            true -> weakref pop_noinit
        elseif arg = '%x' then
            if rest /== [] and islist(fast_front(rest)) then
                fast_destpair(rest) -> rest
            else
                []
            endif -> weakref popunderx
        else
            mishap(arg, 1, 'UNRECOGNISED %ARGUMENT TO PROCESS')
        endif;
        rest -> poparglist
    endwhile;

    if nort then
        if testdef popunderx and weakref popunderx then
            mishap(0, 'PROCESS ARGUMENT %x INCOMPATIBLE WITH %nort')
        endif
    elseunless pop_runtime then
        ;;; "undef" indicates that no actions have been run
        "undef" -> pop_runtime
    endif
enddefine;

    /*  Execute run-time actions if appropriate.
        Called before executing entry procedure (Setup_system below)
        and after restoring a saved image (Save in sr_sys.p).
    */
define Do_runtime_startup();

    define lconstant run_actions_list(l, top);
        lvars l, top, p;
        until null(l) do
            fast_destpair(l) -> (p, l);
            if top then l -> pop_runtime_actions endif;
            if islist(p) then
                ;;; sublist
                run_actions_list(p, false)
            else
                Check_procedure(p);
                fast_apply(p);
                true -> pop_runtime     ;;; actions have been run
            endif
        enduntil
    enddefine;

    if pop_runtime then
        ;;; do runtime actions
        chain(pop_runtime_actions, true, run_actions_list)
    endif
enddefine;


lvars procedure Setpop_setup_system;

    /*  Own encoding idents for the 5 standard devices (must be in the
        system data seg so scanned by GC).
    */
lvars
    enc_id1,
    enc_id2 = ident enc_id1,
    enc_id3 = ident enc_id2,
    enc_id4 = ident enc_id3,
    enc_id5 = ident enc_id4,
;

    /*  Also called from Callback in extern_callback.p
    */
define Setup_system(_cs_hi, _plog_nwords);
    lvars _cs_hi, _plog_nwords, _base, _lim, _plog_area;

    define lconstant Clear_open_seg();
        _system_end ->> _open_seg_base_ptr -> _open_seg_free_ptr;
        _open_seg_free_ptr@(w){_USER_SAFEGAP} -> _userlim;
        Lock_heap_segs();
        ;;; clear existing files from table
        _file_tab -> _file_tab_next_free;
        _file_tab_limit -> _file_tab_close_ptr;
#_IF DEF UNIX
        false -> Dir$-current;
#_ENDIF
    enddefine;

    ;;; treat mishaps as fatal
    true -> _memory_is_corrupt;

    ;;; causes immediate exit if setpop called again
    #_< Opsys_exit(%false%) >_# -> Setpop_setup_system;
    setpop -> interrupt;

#_IF DEF UNIX
  #_IF DEF AIX
    _nonzero(_extern _pop_needs_cache_flush()) -> _cache_flush_needed;
  #_ENDIF
    Init_signals();
    ;;; minimum amount that (external-use) malloc should request from O/S
    _2:1e16 -> _extern __pop_malloc_min_alloc:data!(int);
#_ELSEIF DEF WIN32
    ;;; set up heap, signals, etc.
    _extern pop_init() -> ;
#_ENDIF

    _cs_hi ->> _system_stack_base ->> _call_stack_hi -> _call_stack_seg_hi;

    ;;; Nominal limit for callstack length -- this allows 2**14 stack frames
    ;;; of a procedure with 3 lvars.
    Nframe_cslen(_2:1e14) -> _pop_callstack_lim;
    ;;; Set the actual limit.
    Set_call_stack_lim();

    ;;; Allocate segment table and set up no-restore segment from bss mem
    ;;; allocated in c_core.c.
    (_extern __pop_nr_seg:data)@(b.r->w) -> _seg_table_next_free;
    _seg_table_next_free@(struct SEG)[_256] -> _seg_table_lim;
    _seg_table_lim@~POPBASE -> _base;

    if testdef syssave and not(_in_external_control) then
        ;;; Make seg table entry for save/restore area
        ;;; (1000 words is sufficient)
        lconstant _SR_SEG_NWORDS = _1000;
        _seg_table_next_free -> weakref[syssave] Sr$- _sr_seg_entry;
        Add_seg_entry(_base, _base, @@(w)[_SR_SEG_NWORDS], _M_SEG_SPECIAL);
        _base@(w)[_SR_SEG_NWORDS] -> _base
    endif;

    ;;; Make seg table entry for no-restore segment.
    _seg_table_next_free -> _nr_seg_entry;
    (_extern __pop_nr_seg:data)@(b)
                [_extern __pop_nr_seg_nbytes:data!(int)]@(b.t->w) -> _lim;
    Add_seg_entry(_base, _base, @@(w){_lim, _base@POPBASE}, _M_SEG_SPECIAL);
    ;;; Where ordinary seg entries start.
    _seg_table_next_free -> _seg_table;

    ;;; Set up the open-segment heap, including the userstack (for which at
    ;;; the moment we only have a temporary one).
    Get_mem_break() -> _base;   ;;; remember end

    if testdef prolog_reset and _in_external_control then
        ;;; need to allocate the prolog area from ordinary mem
        if _zero(_plog_nwords) then _2:1e12 -> _plog_nwords endif;
        ##(w){ @@(vpage)[_plog_nwords | w.r] | vpage} -> _plog_nwords;
        _base -> _plog_area;
        _base@(w)[_plog_nwords] -> _base
    endif;

    _base@~POPBASE -> _system_end;
    unless Set_userhi_to(_base@(w){_USER_SAFEGAP}, false) then
        setpop()    ;;; exit
    endunless;
    _userhi -> _user_sp();
    Clear_open_seg();
    ;;; Expand to initial size
    unless Expand_open_seg(_OPEN_SEG_NWORDS, _4096) then
        setpop()    ;;; exit
    endunless;

    ;;; Set up the prolog area
    if testdef prolog_reset then
        weakref[prolog_reset] Plog$-Area_setup(_plog_area, _plog_nwords)
    endif;

    ;;; Then make the no-restore seg the current seg
    Set_curr_heap_seg(_nr_seg_entry);

    false -> _memory_is_corrupt;

    ;;; --- Can start using heap structures from this point ----------

    ;;; From here down to ********, structures are allocated in the
    ;;; no-restore segment. All the variables initialised must not be
    ;;; restored in sysrestore (so, the latter has them all as locals).

    ;;; Allocate an area for 5 standard devices (in, out, err, raw in,
    ;;; raw out). The outer device records for these are allocated by
    ;;; Init_device using Cons_device_getdev, which normally uses
    ;;; Get_store, but which is redefined here to allocate from the
    ;;; following area; this guarantees that the addresses of all standard
    ;;; devices are the same when restoring a saved image.

    dlvars _devrecs = Get_store(@@(struct DEVICE)[_5]), _nxt_devrec = _devrecs,
            enc_id = enc_id5;
    lvars save_getdev = Io$-Cons_device_getdev;

    define dlocal Io$-Cons_device_getdev() /* -> (own_encoding_id, dev) */;
        enc_id;             ;;; return next encoding id
        fast_idval(enc_id) -> enc_id;
        _nxt_devrec;        ;;; return next one
        _nxt_devrec@(struct DEVICE)++ -> _nxt_devrec
    enddefine;

    ;;; pop_system_version is NOT dlocal to sysrestore, and so this string
    ;;; must have the same address every time (thus it must come
    ;;; immediately after the fixed-size space for the device records).
    image_version sys_>< image_date ->> popversion -> pop_system_version;

#_IF DEF UNIX

    ;;; create device records for standard devices
    lvars _tty_in = false;
    [%  Sys_cons_device('std_err', false, 1, false, 2, false) ->> dev_err,
        Sys_cons_device('std_out', false, 1, false, 1, false) ->> dev_out,
        if testdef popdevin then
            Sys_cons_device('std_in', false, 0, false, 0, false)
                                            ->> weakref[popdevin] dev_in;
            systrmdev(weakref popdevin) -> _tty_in
        endif
    %].sys_link_tty_params;

    if (testdef poprawdevin or testdef poprawdevout)
    and (_tty_in or systrmdev(popdevout)) then
        lvars fd = if _tty_in then 0 else 1 endif;
        [%  if testdef poprawdevin then
                Sys_cons_device('raw_dev_in', false, 0, true, fd, false)
                                    ->> weakref[poprawdevin] raw_dev_in
            endif,
            if testdef poprawdevout then
                Sys_cons_device('raw_dev_out', false, 1, true, fd, false)
                                    ->> weakref[poprawdevout] raw_dev_out
            endif
        %].sys_link_tty_params
    endif;

    if testdef poppid then
        Uint_->_pint(_extern getpid()) -> weakref poppid
    endif;

    ;;; get the list of environment strings
    Get_arg_list(_extern environ:data!(((b)))) -> popenvlist;

  #_IF DEF SYSTEM_V or DEF HPUX
    systranslate('LOGNAME')-> popusername;
  #_ELSE
    systranslate('USER') -> popusername;
  #_ENDIF

    systranslate('HOME') -> popdirectory;


#_ELSEIF DEF VMS

    lvars _tty_in = false;
    syscreate('sys$error:', 1, false) -> dev_err;
    syscreate('sys$output:',1,false) -> dev_out;
    if testdef popdevin then
        sysopen('sys$input:', 0, false) -> weakref[popdevin] dev_in;
        systrmdev(weakref popdevin) -> _tty_in
    endif;

    if (testdef poprawdevin or testdef poprawdevout)
    and (_tty_in or systrmdev(popdevout)) then
        lvars f = if _tty_in then 'sys$input:' else 'sys$output:' endif;
        if testdef poprawdevin then
            sysopen(f, 0, true) -> weakref[poprawdevin] raw_dev_in
        endif;
        if testdef poprawdevout then
            sysopen(f, 1, true) -> weakref[poprawdevout] raw_dev_out
        endif
    endif;

    if testdef poppid then
        weakref[poppid] Uint_->_bigint(Vms_jpi_int(_:'JPI$_PID'))
                                                    -> weakref poppid
    endif;

    Vms_jpi_string(_:'JPI$_USERNAME', false) -> popusername;
    systranslate('sys$login:') -> popdirectory;
  #_IF DEF SHARED_LIBRARIES
    Link_in_callback();
  #_ENDIF


#_ELSEIF DEF WIN32

    Io$-Cons_stddev('std_err', 2, false) -> dev_err;
    Io$-Cons_stddev('std_out', 1, false) -> dev_out;
    if testdef popdevin then
        Io$-Cons_stddev('std_in', 0, false) -> weakref[popdevin] dev_in;
    endif;
    if testdef poprawdevout then
        Io$-Cons_stddev('std_out', 1, true)
            -> weakref[poprawdevout] raw_dev_out;
    endif;
    if testdef poprawdevin then
        Io$-Cons_stddev('std_in', 0, true)
            -> weakref[poprawdevin] raw_dev_in;
    endif;

    if testdef poppid then
        weakref[poppid] Uint_->_bigint(_extern pop_get_current_process_id())
            -> weakref poppid;
    endif;

    ;;; setup registry & environment: image_version@[_9] should skip the
    ;;; '(Version\s' prefix and point at just the version number
    lvars t_version = Tchars_in(image_version, wkstring1);
    _extern pop_init_registry(_NULL, _NULL, t_version@V_TCHARS[_9]) -> ;
    _extern pop_init_environment(_NULL) -> ;
    Get_user_name() -> popusername;
    Get_user_directory() -> popdirectory;
    unless Is_environment_variable('poplib') then
        popdirectory -> systranslate('poplib');
    endunless;

#_ELSE_ERROR
#_ENDIF

    sysexit -> interrupt;

    save_getdev -> Io$-Cons_device_getdev;
    Set_standard_char_devs();

    ;;; set up poparglist0, poparg0 and poparglist
    if _in_external_control then
        []
    else
        Get_popargs()
    endif -> poparglist0;
    if poparglist0 == [] then [^nullstring] -> poparglist0 endif;

    fast_front(poparglist0) -> poparg0;
    ;;; set poparglist to start at arg1
    fast_back(poparglist0) -> poparglist;
    ;;; make sublists in poparglist
    Make_sublists(poparglist, true);

    ;;; If this procedure is defined, call it at this point. It should
    ;;; return the search directory list and default extension to be
    ;;; used for restoring saved image args.
    if testdef $-Pop$-Pre_Init_Restore and not(_in_external_control) then
        lvars default_extn, dir_list;
        weakref $-Pop$-Pre_Init_Restore() -> default_extn -> dir_list
    endif;

    ;;; ********

    ;;; switch to the open seg
    Clear_open_seg();


#_IF DEF VMS
    ;;; adjust open seg start to allow for shareable image expansion in
    ;;; saved images
    if testdef syssave and not(_in_external_control) then
        ;;; sets new value for _system_end
        weakref[syssave] Sr$-Adjust_heap_start();
        Clear_open_seg()
    endif;
#_ENDIF

    true -> _setup_done;
    _0 -> _disable;

    ;;; Now if this procedure is defined, try to restore saved image args
    ;;; with the dir list and extn it returned.
    ;;; (no return if successful)
    if testdef $-Pop$-Pre_Init_Restore and not(_in_external_control) then
        weakref [$-Pop$-Pre_Init_Restore]
                                Sr$-Init_restore(dir_list, default_extn)
    endif;

    ;;; no saved image(s) to restore -- normal startup

#_IF DEF UNIX
    ;;; handle signals
    sys_reset_signal();
#_ENDIF
    if testdef prolog_reset then weakref prolog_reset() endif;
    0 -> popmemused;

    ;;; read special params
    Process_percent_args();
    ;;; do run-time actions if appropriate
    Do_runtime_startup();

    ;;; set up -- make this return false if called again by setpop
    #_< identfn(%false%) >_# -> Setpop_setup_system;

    true
enddefine;      /* Setup_system */

define lvars Setpop_setup_system();
        Get_mem_break() -> _user_base;
    chain(_nextframe(_caller_sp_flush()), _0, Setup_system)
enddefine;

define setpop();
    _DISABLE_INTERRUPTS -> _disable;      ;;; disable interrupts etc

    ;;; This sets up system initially
    if Setpop_setup_system() then
        ;;; system setup complete -- hand over to entry procedure
        $-Pop$-Entry_Procedure()    ;;; exit if this returns

    else
        Clear_ast_queue(false);
        ;;; If not at top, unwind callstack (or at least, try to; dlocal
        ;;; exit code may take over control).
        if _caller_sp() <@(csword) _call_stack_hi then
            ;;; chains setpop after unwinding (except when
            ;;; _in_external_control is true -- in this case the Callback
            ;;; exit action just returns _ABNORMEXIT to the external code)
            Callstack_reset(false)
        endif;

        ;;; now at top
        if testdef setpop_reset then
            weakref setpop_reset(true, true)
        endif
    endif;

    sysexit()
enddefine;

define dummy_procedure_callback_helper();
   lvars _dummy1 = 666, _dummy2 = 42;
   _extern printf('%d, %d\n', _dummy1, _dummy2);
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jan  6 2000
        Set _memory_is_corrupt during the early setup phase
--- Robert Duncan, Oct 21 1999
        Win32: changed setting of popdirectory (again)
--- Robert Duncan, Jul 28 1998
        Win32 changes: uses new procedure Is_environment_variable
--- John Gibson, Apr  4 1997
        Changed Cons_device_getdev to return own encoding id as well
        as device record.
--- John Gibson, Mar  7 1997
        Replaced use of Get*_nt_string with Consstring_bptr
--- Robert Duncan, Jan 29 1997
        Modifications to Win32 code for UNICODE compilation
--- John Gibson, Aug 28 1996
        Added _nr_seg_entry
--- Robert Duncan, Jul 24 1996
        Win32 changes: new initialisation for registry and environment
--- Robert Duncan, Jun 20 1996
        Moved Win32 initialisation call into Setup_system
--- Robert Duncan, May 16 1996
        Win32 changes: fixed assignment to popdirectory in case where
        %HOMEDRIVE%%HOMEPATH% is not set, e.g. on Windows 95
--- Robert Duncan, Apr 19 1996
        Win32 changes: much simplified setup for standard I/O devices,
        using new Io$-Cons_stddev
--- Robert John Duncan, Jan  8 1996
        Win32 changes: code for file expansion moved to Expand_file_pattern
        defined elsewhere and Cons_device now takes an extra argument to
        indicate overlapped I/O.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Mar 15 1995
        Added Unix assignment to __pop_malloc_min_alloc in Setup_system
--- John Gibson, Mar 14 1995
        Removed M*alloc_exhausted (now done with a signal)
--- John Gibson, Mar  4 1995
        Changed !++ access type in Get_arg_list to <b> (meaning C
        pointer-to-byte, which on Alpha OSF1 is 64 bits)
--- John Gibson, Nov 29 1994
        Added Link_in_callback for Alpha
--- John Gibson, Nov 16 1994
        Split the processing of %args off from Do_runtime_startup into
        Process_percent_args
--- Robert John Duncan, Sep  5 1994
        Added initialisation code for Win32
--- John Gibson, May 24 1994
        Sys$-Io$-Cons*_device -> Sys_cons_device for Unix.
--- John Gibson, Apr 12 1994
        Added call of Clear_ast_queue in setpop
--- John Gibson, Mar 16 1994
        Removed call to Pw*m$-Se*tup -- now called by Pw*m$-Runtime_startup
        so that it's done after restoring saved images as well
--- John Gibson, Dec  7 1993
        Fixed bugs in VMS Parse_args
--- John Gibson, Nov 12 1993
        Changes to Setup_system etc to allow it to be called from Callback
        in extern_callback.p when called from controlling external code.
--- John Gibson, Oct  1 1992
        pop_runtime_actions now declared as an 'incremental' identifier
        (actions through sys_runtime_apply are now supported in core system).
        Code for running actions now allows sublists of procedures
        in pop_runtime_actions.
--- Robert John Duncan, Aug  7 1992
        Removed last change (SunOS bug now fixed)
--- Simon Nichols, Mar  3 1992
        Changes to support SunOS dynamic linking (temporarily flagged by
        SUNOS*_DYNAMIC).
--- John Gibson, Aug 20 1991
        Changed so that system exits if Entry_Procedure returns
--- John Gibson, May 23 1991
        Added -Do_runtime_startup-, including processing of % args.
--- John Gibson, Apr 15 1991
        Added call to -Adjust_heap_start- for VMS
--- John Gibson, Apr 12 1991
        Removed xpopsetup (should never have been put in, and would never
        get called anyway since popunderx would never be true at that stage).
--- John Gibson, Mar 28 1991
        Setup_system changed so that no-restore segment is now bss memory
        in c_core.c (allocation of Unix malloc memory also moved to c_core.c
        and done by malloc itself). (These two changes mean that in VMS there
        is no data intervening between the end of shareable images and the
        start of the heap.)
--- John Gibson, Jan  3 1991
        Replaced _extern S*IGNAL with _extern _pop_sigaction
--- John Gibson, Nov 30 1990
        Added code to call cpu id check procedure
--- John Gibson, Nov 20 1990
        Fixed problem with VMS pw*m and standard devices
--- John Gibson, Nov 14 1990
        Removed clearstack() from setpop
--- John Gibson, Oct 27 1990
        -setpop- now just calls -setpop_reset- with args
--- Aaron Sloman, Oct 12 1990
        Changed guard on xpopsetup
--- Aaron Sloman, Sep 25 1990
        Added call of -xpopsetup-
--- John Gibson, Sep  1 1990
        Added -Ensure_writeable-
--- John Gibson, Aug 30 1990
        Added -Make_sublists- to turn poparglist args enclosed in ( ... )
        into sublists.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul 24 1990
        Fixed problem with the memory position of the string in
        -pop_system_version-.
--- John Williams, Jul  2 1990
        Added new variable -pop_s*etpop_compiler-
--- John Gibson, Jun  6 1990
        Added local -Cons_device_getdev- in -Setup_system- to allow
        standard devices to be in fixed positions.
--- John Gibson, May 13 1990
        Changed name of malloc run-out-memory routine to -M*alloc_exhausted-
--- John Gibson, Mar  8 1990
        Removed call to C _skyinit (no longer needed, because the Sky setup
        is done automatically by 'fswitch' C library float routines).
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Nov  9 1989
        Removed p*opinicom and made -popdirectory- be
        systranslate('sys$login:') in VMS.
            Added initialisation of _call_stack_seg_hi.
--- John Gibson, Oct 18 1989
        Got rid of s*etpop_handlers mechanism (no longer needed, since
        VED im stuff uses dlocal expressions to trap unwinding of
        callstack).
--- John Gibson, Sep  8 1989
        Made -setpop- assign false to weakref chain_trace before calling
        setpop handler.
--- John Gibson, Sep  1 1989
        (1) Initial callstack limit computed with -Nframe_cslen-.
        (2) VMS code uses -Vms_jpi_int- and -Vms_jpi_string-.
--- John Gibson, Aug 28 1989
        Removed call of S*et_std_in_tty.
--- John Gibson, Aug 10 1989
        Reorganised initial processing of args and handling of
        saved images. Also unprotected -poparglist-.
--- John Gibson, Aug  8 1989
        Enhanced the VMS version of -Get_popargs- so that (a) wildcarded
        arguments are expanded like a Unix shell and (b) an arg of the form
        @<filename> is substituted with args read from <filename>.
        (Overcomes the limited length of the DCL command line, and enables
        any number of args to be given.)
            Also now allows \ to escape characters, etc.
--- John Gibson, May  5 1989
        Instead of constant entry procedure $-Pop$-Main, variable
        procedure $-Pop$-Entry_Procedure is now called; this variable is
        initialised by poplink to a user-specified constant procedure,
        or to $-Pop$-Main by default.
--- Roger Evans, Mar 20 1989
        Added poparg0
--- John Gibson, Mar 20 1989
        Added _OPEN_SEG_NWORDS, etc.
--- John Gibson, Feb  1 1989
        Changed -Setup_system- to allow same size external mem block for
        all Unix systems.
--- John Gibson, Jan 22 1989
        Removed use of -apply- in -setpop-
--- John Gibson, Jul 17 1988
        Got rid of _skyinit call for Sun-4
--- Roger Evans, Jun  6 1988
        Added extra argument to Cons*_device
--- John Gibson, Apr  9 1988
        Setpop reset stuff to setpop_reset.p
--- John Gibson, Mar 25 1988
        -pop_default_section- now an active variable.
        Weakref'ed section references
--- John Gibson, Mar  9 1988
        Weakref'ed prolog stuff
--- John Gibson, Feb 24 1988
        userstack_stack into section Sys
--- John Gibson, Feb 15 1988
        Incorporated syssetup.p into this file -- syssetup.p now
        redundant.
--- John Gibson, Feb 15 1988
        Weakref'ed pw*m references
 */
