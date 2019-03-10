/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/src/devio.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ------------------ DEVICE I/O (UNIX) --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'unix_tty.ph'
#_INCLUDE 'signals.ph'

constant
        procedure (syssleep, sys_send_signal, sys_signal_handler),
        _locc
    ;

vars
        procedure (pop_timeout, pop_file_write_error),
        pop_timeout_secs, pop_buffer_charout, poppid
    ;

weak constant
        procedure (sys_input_waiting, sys_clear_input)
    ;

section $-Sys;

constant
        procedure (Eq__String8, Add_file_tab_entry, Abnormal_sysexit,
        App_open_devs, Timed_wait_apply),
        no_device_encoding
    ;

vars
        _vfork_child
    ;

weak constant
        procedure (Xt$-Xpt_read_wait)
    ;

endsection;

section $-Sys$-Io;

constant
        procedure (Init_device, No_write, Prompt, Opencreate,
        Kill_device, Cons_iobuffer)
    ;

weak constant
        procedure (Test_input, Clear_input)
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys$-Io =>    set_process_entry_term,
                        Sys_cons_device, Sys_fd_open_check;

lvars
    _terminal_io_done = _0
    ;


;;; --- TTY PARAMETER MANIPULATION ---------------------------------------

    /*  Set a terminal's parameters corresponding to arg3
    */
define lconstant Tty_param_init(ttyparams, arg3);
    lvars ttyparams, arg3;

#_IF DEF USE_TERMIOS or DEF USE_TERMIO
    lvars _termio = ttyparams@TTP_SGTTYB;
    if arg3 then
        _termio!TMIO_IFLAG
            _biclear (_TTY_ICRNL _biset _TTY_IXOFF
                      _biset _TTY_IXANY _biset _TTY_INPCK _biset _TTY_PARMRK
                      _biset _TTY_IUCLC _biset _TTY_INLCR)
            _biset   _TTY_IXON _biset _TTY_ISTRIP
            -> _termio!TMIO_IFLAG;
        _termio!TMIO_OFLAG
            _biclear (_TTY_OPOST _biset _TTY_ONLCR _biset _TTY_OCRNL
                      _biset _TTY_BSDLY _biset _TTY_VTDLY _biset _TTY_FFDLY
                      _biset _TTY_TAB3)
            -> _termio!TMIO_OFLAG;
        _termio!TMIO_LFLAG
            _biclear (_TTY_XCASE _biset _TTY_ICANON _biset _TTY_ECHO
                      _biset _TTY_ECHOE _biset _TTY_ECHOK _biset _TTY_NOFLSH)
            _biset    _TTY_ISIG
            -> _termio!TMIO_LFLAG;
  #_IF DEF _CC_VLNEXT
        _POSIX_VDISABLE -> _termio!TMIO_CC[_CC_VLNEXT];
  #_ENDIF
        ;;; set MIN & TIME
        _1 -> _termio!TMIO_CC[_CC_VMIN];
        _0 -> _termio!TMIO_CC[_CC_VTIME];

    else
        _termio!TMIO_IFLAG
            _biclear (_TTY_IGNBRK _biset _TTY_PARMRK _biset _TTY_INPCK
                      _biset _TTY_INLCR _biset _TTY_IUCLC _biset _TTY_IXANY
                      _biset _TTY_IXOFF)
            _biset   (_TTY_BRKINT _biset _TTY_IGNPAR _biset _TTY_ISTRIP
                      _biset _TTY_IXON _biset _TTY_ICRNL)
            -> _termio!TMIO_IFLAG;
        _termio!TMIO_OFLAG
            _biclear (_TTY_OLCUC _biset _TTY_OCRNL _biset _TTY_ONOCR
                      _biset _TTY_ONLRET _biset _TTY_OFILL _biset _TTY_OFDEL)
            _biset   (_TTY_OPOST _biset _TTY_ONLCR)
            -> _termio!TMIO_OFLAG;
        _termio!TMIO_LFLAG
            _biclear (_TTY_XCASE _biset _TTY_ECHONL _biset _TTY_NOFLSH)
            _biset (_TTY_ICANON _biset _TTY_ECHO _biset _TTY_ECHOE
                    _biset _TTY_ECHOK _biset _TTY_ISIG)
            -> _termio!TMIO_LFLAG;
    endif

#_ELSE
    lvars _sgttyb = ttyparams@TTP_SGTTYB;
    if arg3 then
        _sgttyb!SGT_FLAGS
            _biclear (_TTY_CRMOD _biset _TTY_ECHO _biset _TTY_RAW
                      _biset _TTY_XTABS _biset _TTY_VTDELAY
                      _biset _TTY_BSDELAY)
            _biset    _TTY_CBREAK
            -> _sgttyb!SGT_FLAGS
    else
        _sgttyb!SGT_FLAGS
            _biclear (_TTY_CBREAK _biset _TTY_RAW)
            _biset   (_TTY_CRMOD _biset _TTY_ECHO)
            -> _sgttyb!SGT_FLAGS
    endif

#_ENDIF

enddefine;      /* Tty_param_init */


define lconstant Is_terminal(_fd);
    lvars _fd;
#_IF DEF USE_TERMIOS
    lstackmem struct TERMIOS _termios;
    _nonneg(_extern[SE] tcgetattr(_fd, _termios));
#_ELSEIF DEF USE_TERMIO
    lstackmem struct TERMIO _termio;
check this ;
    _nonneg(_extern[NI, SE] ioctl(_fd, _TGETPARM, _termio));
#_ELSE
    lstackmem struct SGTTYB _sgttyb;
check this ;
    _nonneg(_extern[NI, SE] ioctl(_fd, _TGETPARM, _sgttyb));
#_ENDIF
enddefine;

    /*  Test if terminal foreground/background
    */
define lconstant In_foreground(_fd);
    lvars _fd, _pgrp;
    dlvars _pgrpterm;
#_IF DEF USE_TERMIOS
    _extern getpgrp(_0) -> _pgrp;
    _extern tcgetpgrp(_fd) -> _pgrpterm;
    _pgrpterm _slteq _0 or _pgrp == _pgrpterm;
#_ELSEIF DEF USE_BSD_TTY
    _extern getpgrp(_0) -> _pgrp;
    _extern[NI] ioctl(_fd, _TIOCGPGRP, ident _pgrpterm) -> ;
    _zero(_pgrpterm) or _pgrp == _pgrpterm
#_ELSE
    ;;; must be
    true
#_ENDIF
enddefine;

define Is_foreground_term(_fd);
    lvars _fd;
    Is_terminal(_fd) and In_foreground(_fd)
enddefine;

define Get_tty_params(dev, params);
    lvars params, dev, _fd = dev!D_FILE_DESC;
#_IF DEF USE_TERMIOS
    _extern tcgetattr(_fd, params@TTP_SGTTYB) -> ;
#_ELSEIF DEF USE_TERMIO
    _extern[NI] ioctl(_fd, _TCGETA, params@TTP_SGTTYB) -> ;
#_ELSE  ;;; USE_BSD_TTY
    _extern[NI] ioctl(_fd, _TIOCGETP, params@TTP_SGTTYB) -> ;
    _extern[NI] ioctl(_fd, _TIOCGETC, params@TTP_TCHARS) -> ;
    _extern[NI] ioctl(_fd, _TIOCLGET, params@TTP_LMWORD) -> ;
    _extern[NI] ioctl(_fd, _TIOCGLTC, params@TTP_LTCHARS) -> ;
#_ENDIF
enddefine;

define Set_tty_params(dev, params, _pmode);
    lvars unit, params, dev, _fd, _pmode;
    dev!D_UNIT_P -> unit;
    unless Eq__String8(unit!UNT_CURR_TTPARAMS, params) then
        ATT_PARM_CHANGED -> unit!UNT_TTPARM_STATUS;
        dev!D_FILE_DESC -> _fd;
#_IF DEF USE_TERMIOS
        _extern tcsetattr(_fd, _pmode, params@TTP_SGTTYB) -> ;
#_ELSEIF DEF USE_TERMIO
        _extern[NI] ioctl(_fd, _pmode, params@TTP_SGTTYB) -> ;
#_ELSE  ;;; USE_BSD_TTY
        _extern[NI] ioctl(_fd, _pmode, params@TTP_SGTTYB) -> ;
        _extern[NI] ioctl(_fd, _TIOCSETC, params@TTP_TCHARS) -> ;
        _extern[NI] ioctl(_fd, _TIOCLSET, params@TTP_LMWORD) -> ;
        _extern[NI] ioctl(_fd, _TIOCSLTC, params@TTP_LTCHARS) -> ;
#_ENDIF
    endunless;

    ;;; set the tty unit's current params field
    params -> unit!UNT_CURR_TTPARAMS
enddefine;

define Fix_dev_params(dev, _writing) -> devparams;
    lvars   tcb = dev!D_CTRL_BLK, unit, devparams, ttyparams, dev,
            _fd = dev!D_FILE_DESC, _writing;
#_IF true
#_IF DEF SIG_TTOU
    ;;; see if we are backgrounded (NB: we do this on all terminals,
    ;;; not just the controlling one - its easier and even non-controlling
    ;;; terminals fall over under Sunos 4  RE.)
    unless In_foreground(_fd) then
        ;;; allow ordinary (non-rare) output without bothering about params
        returnif(_writing and not(tcb!TCB_ARG3)) (false -> devparams);
        procedure();
            dlocal % sys_signal_handler(SIG_TTOU) % = Sys_stop_handler;
            repeat
                ;;; stop, using appropriate signal
                sys_send_signal(poppid, if _writing then SIG_TTOU
                                        else SIG_TTIN
                                        endif) -> ;
                syssleep(50);   ;;; allow signal to be sent
                ;;; restarted -- test again
                quitif(In_foreground(_fd));
                syssleep(50)    ;;; delay for possible SIG_TERM
            endrepeat
        endprocedure()
    endunless;
#_ENDIF
#_ENDIF
    ;;; now in foreground -- test whether tty init params need (re)setting
    dev!D_UNIT_P -> unit;
    unit!UNT_INIT_TTPARAMS -> ttyparams;
    if unit!UNT_TTPARM_STATUS == ATT_PARM_INVALID then
        Get_tty_params(dev, ttyparams);
        ATT_PARM_VALID -> unit!UNT_TTPARM_STATUS;
        ttyparams -> unit!UNT_CURR_TTPARAMS
    endif;

    ;;; now check dev params have been set up
    tcb!TCB_PARAMS -> devparams;
    returnif(tcb!TCB_PARAMS_SETUP);

    ;;; No -- first initialise the params string from the tty initial
    ;;; values.
    _bmove(@@(b)[ttyparams!V_LENGTH], ttyparams@V_BYTES, devparams@V_BYTES) -> ;

    ;;; now make the required alterations to the params string. Take <false>
    ;;; to mean use the initial parameters
    if tcb!TCB_ARG3 then Tty_param_init(devparams, true) endif;
    true -> tcb!TCB_PARAMS_SETUP
enddefine;

define Ensure_dev_params_set(dev, _pmode, _writing);
    lvars dev, devparams, _pmode, _writing;
    if Fix_dev_params(dev, _writing) ->> devparams then
        Set_tty_params(dev, devparams, _pmode)
    endif
enddefine;

    ;;; check the state of a terminal for a given device,
    ;;; changing it if necessary
define Set_term_state(dev, _writing);
    lvars tcb = dev!D_CTRL_BLK, dev, _writing;

    ;;; Changing this variable causes interrupted reads to re-output
    ;;; prompts
    _terminal_io_done _add _1 -> _terminal_io_done;

    ;;; quick test for state being correct
    if dev!D_UNIT_P!UNT_CURR_TTPARAMS /== tcb!TCB_PARAMS then
        Ensure_dev_params_set(dev,if tcb!TCB_PMODE then _TSETPARM_P
                                  else _TSETPARM_N
                                  endif, _writing)
    endif
enddefine;

    /*  Clear any input on a terminal
    */
define lconstant Clear_term_input(dev);
    lvars dev, buf = dev!D_IN_BUFFER;
    _0 ->> buf!BUF_COUNT ->> buf!BUF_POSITION -> buf!BUF_ENCODE_STATE;
    if In_foreground(dev!D_FILE_DESC) then
        _terminal_io_done _add _1 -> _terminal_io_done;
#_IF DEF USE_TERMIOS
        _extern tcflush(dev!D_FILE_DESC, _TCIFLUSH) -> ;
#_ELSE
        nullstring -> dev!D_UNIT_P!UNT_CURR_TTPARAMS;   ;;; force set
        ;;; TIOCFLUSH can't be used here because it throws away output
        Ensure_dev_params_set(dev, _TSETPARM_P, false)
#_ENDIF
    ;;; else forget it
    endif
enddefine;


;;; --- GLOBAL TTY OPERATIONS ----------------------------------------------

define lconstant App_tty_devs(app_p);
    dlvars procedure app_p;
    App_open_devs(  procedure(dev);
                        lvars dev;
                        if dev!D_FLAGS _bitst _M_D_TERMINAL then
                            chain(dev, app_p)
                        endif
                    endprocedure)
enddefine;      /* App_tty_devs */

define lconstant Reset_init_params(dev);
    lvars unit = dev!D_UNIT_P, dev;
    if unit!UNT_TTPARM_STATUS == ATT_PARM_CHANGED
    and In_foreground(dev!D_FILE_DESC) then
        Set_tty_params(dev, unit!UNT_INIT_TTPARAMS, _TSETPARM_P);
        ATT_PARM_VALID -> unit!UNT_TTPARM_STATUS
    endif
enddefine;

    /*  Reset all tty params to what they were on process (re)entry
        and disable async input before a stop (SIG_TSTP)
    */
define Restore_tty_devs();
    App_tty_devs(Reset_init_params)
enddefine;

    /*  Zap tty params/restore async input when restarting after a stop
    */
define Zap_tty_devs();

    define lconstant zap_params(dev);
        lvars unit = dev!D_UNIT_P, dev;
        ATT_PARM_INVALID -> unit!UNT_TTPARM_STATUS;
        nullstring -> unit!UNT_CURR_TTPARAMS
    enddefine;

    App_tty_devs(zap_params)
enddefine;

    /*  Reset all tty params to what they were on process (re)entry
    */
define set_process_entry_term();
    App_tty_devs(Reset_init_params)
enddefine;


;;; --- READING DEVICES ----------------------------------------------------

lconstant string16_ms = 'STRING16 NEEDED FOR ENCODED CHARACTER INPUT';

define lconstant Do_read(/*_bindx, buff, _nbytes,*/ dev, timclos)
                                                    /* -> _count */;
    lvars   buff, _bindx, _nbytes, _count;
    dlvars  dev, timclos, _fd = dev!D_FILE_DESC;

    define lconstant Read_error(/*dev*/);
        Syserr_mishap((), 1, 'ERROR READING DEVICE')
    enddefine;

    define lconstant Blocking(_bindx, buff, _nbytes) -> _count;
        lvars   buff, prompt, promptstring, _flags = dev!D_FLAGS,
                _count, _read_lives = _3, _bindx, _nbytes, _save_termio_done;

        ;;; If async input is enabled for _fd, turn checking off
        ;;; during the read
        dlocal 0 %  if dlocal_context fi_<= 2 then
                        _extern _pop_set_async_check(_0, _fd, _:RD_SET) ->
                    endif,
                    if dlocal_context fi_<= 2 then
                        _extern _pop_set_async_check(_1, _fd, _:RD_SET) ->
                    endif
                 %;

        if _flags _bitst _M_D_TERMINAL and _flags _bitst _M_D_TERM_PROMPT then
            Prompt(dev)
        else
            false
        endif -> promptstring;
        false -> prompt;

        while _nonzero(_read_lives) do

            if dev!D_WRITE /== No_write then
                fast_apply(dev, dev!D_FLUSH)
            elseif testdef popdevin and dev == weakref[popdevin] dev_in then
                fast_apply(dup(dev_out)!D_FLUSH)
            elseif testdef poprawdevin
            and dev == weakref[poprawdevin] raw_dev_in then
                fast_apply(dup(weakref[poprawdevin] raw_dev_out)!D_FLUSH)
            endif;

            if _flags _bitst _M_D_TERMINAL then
                Set_term_state(dev, false);
                if promptstring and not(prompt) then
                    ;;; write prompt
                    _extern[NI] write(_fd, promptstring@V_BYTES,
                                                    promptstring!V_LENGTH) -> ;
                    true -> prompt
                endif
            endif;

            ;;; any Set_term_states will add 1 to this
            _terminal_io_done -> _save_termio_done;

            ;;; use X wait to wait for input if necessary (returns false if
            ;;; can't handle it, -1 for interrupt and 1 for input waiting)
            unless testdef $-Sys$-Xt$-Xpt_read_wait
            and weakref $-Sys$-Xt$-Xpt_read_wait(_fd) == -1 then
                ;;; either can't handle it, or input waiting -- OK to read
                _extern read_popintr(_fd, buff@(w->b)[_bindx], _nbytes)
                                                            -> _count;
                if _nonneg(_count) then
                    if _flags _bitst _M_D_TERMINAL and _zero(_count)
                    and prompt then
                        ;;; write newline out after prompt followed by ^Z
                        _extern[NI] write(_fd, '\n'@V_BYTES, _1) ->
                    endif;
                    return
                elseif _ERRNO /== _:EINTR then
                    ;;; read actually failed (rather than interrupt return) so
                    ;;; we lose a life
                    _read_lives _sub _1 -> _read_lives
                endif

            ;;; else interrupted -- _ERRNO is EINTR
            endunless;

            _CHECKINTERRUPT;
            if timclos and not(fast_frozval(1,timclos)) then
                ;;; timer expired
                pop_timeout();
                return(_-1 -> _count)
            endif;

            if _terminal_io_done /== _save_termio_done then
                ;;; put out prompt again if necessary
                false -> prompt
            endif
        endwhile;

        ;;; repeated read failure
        if _flags _bitst _M_D_TERMINAL then
            ;;; do abnormal exit
            Abnormal_sysexit()
        else
            Read_error(dev)
        endif
    enddefine;      /* Blocking */

    if dev!D_FLAGS _bitst _M_D_INTERACTIVE then
        Blocking()
    else
        () -> (_bindx, buff, _nbytes);
        if dev!D_WRITE /== No_write then fast_apply(dev, dev!D_FLUSH) endif;
        repeat
            _extern read(_fd, buff@(w->b)[_bindx], _nbytes) -> _count;
            returnif(_nonneg(_count)) (_count);
            unless _ERRNO == _:EINTR then Read_error(dev) endunless
        endrepeat
    endif
enddefine;      /* Do_read */

    /*  Read record orientated file type device (disk, tape, etc)
        i.e. supply _nchars if possible
    */
define lconstant File_read(dev, _csub, userbuf, _nchars);
    lvars   buf = dev!D_IN_BUFFER, dev, userbuf, _count, _take,
            _nchars = _int(_nchars), _csub = _int(_csub) _sub _1,
            _decode;

    if fast_idval(dev!D_ENCODING_ID) ->> _decode then
        lstackmem int _ilenp, int _olenp;
        lvars _n, _decode_flags = _INPUT_INCOMPLETE;
        unless userbuf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            mishap(dev, 1, string16_ms)
        endunless;
        _decode!XP_PTR!CDFN_DECODE -> _decode;
        _nchars -> _count;
        until _zero(_count) do
            if _zero(buf!BUF_COUNT _sub buf!BUF_POSITION ->> _take) then
READMORE:
                Do_read(##BUF_START[_take], buf, buf!BUF_SIZE _sub _take,
                                                dev, false) -> _n;
                if _zero(_n) then _0 -> _decode_flags endif;    ;;; EOF
                _take _add _n ->> _take -> buf!BUF_COUNT;
                _0 -> buf!BUF_POSITION;
                returnif(_zero(_take)) (_pint(_nchars _sub _count)) ;;; eof
            endif;
            _take -> _ilenp!(int);
            _count -> _olenp!(int);
            _extern[INDIR] _decode( buf@BUF_START[buf!BUF_POSITION], _ilenp,
                                    userbuf@(w->s)[_csub], _olenp,
                                    buf@BUF_ENCODE_STATE, _decode_flags) -> ;
            _ilenp!(int) -> _take;
            buf!BUF_COUNT _sub _take -> buf!BUF_POSITION;
            if _zero(_olenp!(int) ->> _n) then
                ;;; input available, but no chars produced -- read more
                ;;; after shifting remaining bytes down
                _bmove(@@(b)[_take], buf@BUF_START[buf!BUF_POSITION], buf@BUF_START[_0]) -> ;
                _0 -> buf!BUF_POSITION;
                goto READMORE
            endif;
            _count _sub _n -> _count;
            _csub _add _n -> _csub
        enduntil;
        return(_pint(_nchars))
    endif;

    if _nchars == _1 then
        ;;; special for discin, etc
        if (buf!BUF_POSITION ->> _take) == buf!BUF_COUNT then
            Do_read(##BUF_START, buf, buf!BUF_SIZE, dev, false) -> _count;
            returnif(_zero(_count)) (0);    ;;; eof
            _count -> buf!BUF_COUNT;
            _0 -> _take
        endif;
        buf!BUF_START[_take] -> userbuf!(w->b)[_csub];
        _take _add _1 -> buf!BUF_POSITION;
        return(1)
    endif;

    _nchars -> _count;
    until _zero(_count) do
        buf!BUF_COUNT _sub buf!BUF_POSITION -> _take;
        if _zero(_take) and _count _gr buf!BUF_SIZE then
            Do_read(_csub, userbuf, _count, dev, false) -> _take;
            returnif(_zero(_take)) (_pint(_nchars _sub _count))     ;;; eof
        else
            if _zero(_take) then
                Do_read(##BUF_START, buf, buf!BUF_SIZE, dev, false) -> _take;
                returnif(_zero(_take)) (_pint(_nchars _sub _count));    ;;; eof
                _take -> buf!BUF_COUNT;
                _0 -> buf!BUF_POSITION
            endif;
            if _count _lt _take then _count -> _take endif;
            _bmove(@@(b)[_take], buf@BUF_START[buf!BUF_POSITION],
                                                userbuf@(w->b)[_csub]) -> ;
            buf!BUF_POSITION _add _take -> buf!BUF_POSITION
        endif;
        _csub _add _take -> _csub;
        _count _sub _take -> _count
    enduntil;
    _pint(_nchars)
enddefine;

    /*  Same as File_read, but ignores D_ENCODING_ID
    */
define lconstant Blockio_read(dev, _csub, userbuf, _nchars);
    lvars   buf = dev!D_IN_BUFFER, dev, userbuf, _count, _take,
            _nchars = _int(_nchars), _csub = _int(_csub) _sub _1;
    _nchars -> _count;
    until _zero(_count) do
        buf!BUF_COUNT _sub buf!BUF_POSITION -> _take;
        if _zero(_take) and _count _gr buf!BUF_SIZE then
            Do_read(_csub, userbuf, _count, dev, false) -> _take;
            returnif(_zero(_take)) (_pint(_nchars _sub _count))     ;;; eof
        else
            if _zero(_take) then
                Do_read(##BUF_START, buf, buf!BUF_SIZE, dev, false) -> _take;
                returnif(_zero(_take)) (_pint(_nchars _sub _count));    ;;; eof
                _take -> buf!BUF_COUNT;
                _0 -> buf!BUF_POSITION
            endif;
            if _count _lt _take then _count -> _take endif;
            _bmove(@@(b)[_take], buf@BUF_START[buf!BUF_POSITION],
                                                userbuf@(w->b)[_csub]) -> ;
            buf!BUF_POSITION _add _take -> buf!BUF_POSITION
        endif;
        _csub _add _take -> _csub;
        _count _sub _take -> _count
    enduntil;
    _pint(_nchars)
enddefine;

    /*  Read lines, i.e. return bytes upto a newline
    */
define lconstant Line_read(dev, _csub, userbuf, _nchars);
    lvars   buf = dev!D_IN_BUFFER, dev, userbuf, _offs, _count, _pos,
            _nchars, _csub, _movedone = false, _org_nc, _decode;

    _int(_nchars) ->> _nchars -> _org_nc;
    _int(_csub) _sub _1 -> _csub;

    if fast_idval(dev!D_ENCODING_ID) ->> _decode then
        lstackmem int _ilenp, int _olenp;
        lvars _n, _decode_flags = _INPUT_INCOMPLETE _biset _STOP_AT_NEWLINE;
        unless userbuf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            mishap(dev, 1, string16_ms)
        endunless;
        _decode!XP_PTR!CDFN_DECODE -> _decode;
        until _zero(_nchars) do
            if _zero(buf!BUF_COUNT _sub buf!BUF_POSITION ->> _count) then
READMORE:
                Do_read(##BUF_START[_count], buf, buf!BUF_SIZE _sub _count,
                                                dev, false) -> _n;
                if _zero(_n) then
                    _STOP_AT_NEWLINE -> _decode_flags       ;;; EOF
                endif;
                _count _add _n ->> _count -> buf!BUF_COUNT;
                _0 -> buf!BUF_POSITION;
                returnif(_zero(_count)) (_pint(_org_nc _sub _nchars))   ;;; eof
            endif;
            _count -> _ilenp!(int);
            _nchars -> _olenp!(int);
            _extern[INDIR] _decode( buf@BUF_START[buf!BUF_POSITION], _ilenp,
                                    userbuf@(w->s)[_csub], _olenp,
                                    buf@BUF_ENCODE_STATE, _decode_flags) -> ;
            _ilenp!(int) -> _count;
            buf!BUF_COUNT _sub _count -> buf!BUF_POSITION;
            if _zero(_olenp!(int) ->> _n) then
                ;;; input available, but no chars produced -- read more
                ;;; after shifting remaining bytes down
                _bmove(@@(b)[_count], buf@BUF_START[buf!BUF_POSITION], buf@BUF_START[_0]) -> ;
                _0 -> buf!BUF_POSITION;
                goto READMORE
            endif;
            _nchars _sub _n -> _nchars;
            _csub _add _n -> _csub;
            ;;; stop if last char decoded was a newline
            quitif(userbuf!(w->s)[_csub _sub _1] == _:`\n`)
        enduntil;
        return(_pint(_org_nc _sub _nchars))
    endif;

    repeat
        buf!BUF_POSITION -> _pos;
        buf!BUF_COUNT _sub _pos -> _count;
        while (_locc(buf@BUF_START[_pos], @@(b)[_count], _:`\n`) ->> _offs)
            == _-1 do
            ;;; no newline found - shift remaining chars to beginning and
            ;;; then try to read some more
            unless _movedone then
                unless dev!D_WRITE == No_write then
                    fast_apply(dev, dev!D_FLUSH)
                endunless;
                _bmove(@@(b)[_count], buf@BUF_START[_pos], buf@BUF_START[_0])
                                                                -> ;
                _0 ->> _pos -> buf!BUF_POSITION;
                _count -> buf!BUF_COUNT;
                true -> _movedone
            endunless;
            _pos _add _count -> _pos;
            unless _zero(buf!BUF_SIZE _sub _pos ->> _count) then
                Do_read(##BUF_START[_pos], buf, _count, dev, false) -> _count;
                buf!BUF_COUNT _add _count -> buf!BUF_COUNT
            endunless;
            if _zero(_count) then
                ;;; nothing read, can't find a newline
                --@@(b) -> _offs;
                quitloop
            endif
        endwhile;

        ##(b){_offs}++ -> _offs;        ;;; include newline
        if _movedone then _offs _add _pos -> _offs endif;
        if _nchars _lt _offs then _nchars -> _offs endif;
        buf!BUF_POSITION -> _pos;
        _bmove(@@(b)[_offs], buf@BUF_START[_pos], userbuf@(w->b)[_csub]) -> ;
        _pos _add _offs ->> _pos -> buf!BUF_POSITION;
        _nchars _sub _offs -> _nchars;
        quitif(_zero(_nchars) or _nonzero(_count) or buf!BUF_SIZE _gr _pos);
        _csub _add _offs -> _csub;
        _0 ->> buf!BUF_POSITION -> buf!BUF_COUNT
    endrepeat;
    _pint(_org_nc _sub _nchars)
enddefine;


    /*  Read interactive device (terminal or pipe)
    */
define lconstant Interact_read(dev, _csub, userbuf, _nchars);
    lvars   buf = dev!D_IN_BUFFER, dev, userbuf, _count, _n,
            _nchars = _int(_nchars), _csub = _int(_csub) _sub _1,
            _pos, _decode_flags = _INPUT_INCOMPLETE, _decode;

RESTART:
    buf!BUF_POSITION -> _pos;
    if _zero(buf!BUF_COUNT _sub _pos ->> _count) then

READMORE:
        ;;; args to Do_read
        (##BUF_START[_count], buf, buf!BUF_SIZE _sub _count, dev);
        if dev!D_FLAGS _bitst _M_D_LOGICAL_TERM
        and isinteger(pop_timeout_secs ->> _n) and _n fi_>= 0 then
            if _neg(Timed_wait_apply((), _n*1e6, Do_read) ->> _n) then
                ;;; pop_timeout returned
                goto RESTART
            endif
        else
            Do_read((), false) -> _n
        endif;
        if _zero(_n) then _0 -> _decode_flags endif;    ;;; indicate EOF
        _count _add _n ->> _count -> buf!BUF_COUNT;
        _0 ->> _pos -> buf!BUF_POSITION;
        returnif(_zero(_count)) (0)
    endif;


    if fast_idval(dev!D_ENCODING_ID) ->> _decode then
        lstackmem int _ilenp, int _olenp;
        unless userbuf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            mishap(dev, 1, string16_ms)
        endunless;
        _decode!XP_PTR!CDFN_DECODE -> _decode;
        _count -> _ilenp!(int);
        _nchars -> _olenp!(int);
        _extern[INDIR] _decode( buf@BUF_START[_pos], _ilenp,
                                userbuf@(w->s)[_csub], _olenp,
                                buf@BUF_ENCODE_STATE, _decode_flags) -> ;
        _ilenp!(int) -> _count;
        buf!BUF_COUNT _sub _count ->> _pos -> buf!BUF_POSITION;
        if _zero(_olenp!(int) ->> _n) then
            ;;; no chars produced -- read more after shifting remaining
            ;;; bytes down
            _bmove(@@(b)[_count], buf@BUF_START[_pos], buf@BUF_START[_0]) -> ;
            _0 -> buf!BUF_POSITION;
            goto READMORE
        endif;
        _pint(_n)
    else
        if _nchars _lt _count then _nchars -> _count endif;
        _bmove(@@(b)[_count], buf@BUF_START[_pos], userbuf@(w->b)[_csub]) -> ;
        _pos _add _count -> buf!BUF_POSITION;
        _pint(_count)
    endif
enddefine;


;;; --- WRITING DEVICES -----------------------------------------------------

lconstant    procedure (File_seek)
    ;

define lconstant Write_error(dev);
    lvars   dev, _errnum = _ERRNO, _flags = dev!D_FLAGS, _fd = dev!D_FILE_DESC,
            unit_n = dev!D_UNIT_N;

    _0 -> dev!D_OUT_BUFFER!BUF_POSITION;

    ;;; just return if ignoring write errors on this device's unit
    returnif(unit_n!UNT_FLAGS _bitst _:M_UNT_IGNORE_WRITE_ERR);

    if (dev == dev_out or dev == dev_err)
    and _errnum /== _:EAGAIN            ;;; unlikely to be EAGAIN, but ...
    ;;; if Is_terminal still returns true for terminal, assume it's OK
    and not(_flags _bitst _M_D_TERMINAL and Is_terminal(_fd))
    then
        ;;; Assume unrecoverable error on a standard output/error device, and
        ;;; prevent any further mishaps when trying to write it (or any
        ;;; other with the same unit)
        unit_n!UNT_FLAGS _biset _:M_UNT_IGNORE_WRITE_ERR -> unit_n!UNT_FLAGS
    endif;

    if _flags _bitst _M_D_INTERACTIVE then
        _CHECKINTERRUPT
    else
        ;;; if file-type dev then close without trying to flush.
        ;;; Then give dev to procedure pop_file_write_error
        unless unit_n!UNT_FLAGS _bitst _:M_UNT_IGNORE_WRITE_ERR then
            _extern[NI] pop_close(_fd) -> ;
            Kill_device(dev)
        endunless;
        _errnum -> _ERRNO;
        pop_file_write_error(dev)
    endif;

    Syserr_mishap(dev, 1, 'ERROR WRITING DEVICE')
enddefine;

define lconstant Do_write(/* _bindx, buf, _nbytes, */ dev);
    lvars   buf, dev, _fd = dev!D_FILE_DESC, _bindx, _nbytes, _nwrit;

    define lconstant Blocking(_bindx, buf, _nbytes);
        lvars   buf, _bindx, _nbytes, _nwrit, _write_lives = _4;

        ;;; If async output is enabled for _fd, turn checking off
        ;;; during the write
        dlocal 0 %  if dlocal_context fi_<= 2 then
                        _extern _pop_set_async_check(_0, _fd, _:WR_SET) ->
                    endif,
                    if dlocal_context fi_<= 2 then
                        _extern _pop_set_async_check(_1, _fd, _:WR_SET) ->
                    endif
                 %;

        until _zero(_nbytes) do
            _extern write(_fd, buf@(w->b)[_bindx], _nbytes) -> _nwrit;
            if _neg(_nwrit) then
                if _ERRNO == _:EINTR then
                    if buf == dev!D_OUT_BUFFER then
                        ;;; writing the dev's buffer -- set the BUF_COUNT
                        ;;; field to reflect what's already been written
                        _bindx _sub ##BUF_START -> buf!BUF_COUNT;
                        _CHECKINTERRUPT;
                        ;;; then re-get the amount after the interrupt
                        buf!BUF_COUNT -> _bindx;
                        buf!BUF_POSITION _sub _bindx -> _nbytes;
                        _bindx _add ##BUF_START -> _bindx
                    else
                        _CHECKINTERRUPT
                    endif;
                    nextloop
                else
                    ;;; write error
                    _write_lives _sub _1 -> _write_lives;
                    nextif(_nonzero(_write_lives));
                    chain(dev, Write_error)     ;;; do nothing if this returns
                endif
            else
                _nbytes _sub _nwrit -> _nbytes;
                _bindx _add _nwrit -> _bindx
            endif
        enduntil
    enddefine;

    if dev!D_FLAGS _bitst _M_D_INTERACTIVE then
        Blocking()
    else
        () -> (_bindx, buf, _nbytes);
        until _zero(_nbytes) do
            _extern write(_fd, buf@(w->b)[_bindx], _nbytes) -> _nwrit;
            if _neg(_nwrit) then
                nextif(_ERRNO == _:EINTR);
                chain(dev, Write_error)     ;;; do nothing if this returns
            else
                _nbytes _sub _nwrit -> _nbytes;
                _bindx _add _nwrit -> _bindx
            endif
        enduntil
    endif;
enddefine;

define lconstant Write_dev_buffer(dev);
    lvars dev, buf, _count;
    if dev!D_FLAGS _bitst _M_D_TERMINAL then
        ;;; 2nd arg true doesn't force params set from background
        Set_term_state(dev, true)
    endif;
    dev!D_OUT_BUFFER -> buf;
    buf!BUF_COUNT -> _count;
    Do_write(_count _add ##BUF_START, buf, buf!BUF_POSITION _sub _count, dev);
    _0 ->> buf!BUF_POSITION -> buf!BUF_COUNT
enddefine;

    /*  Buffered file
    */
define lconstant File_write(dev, _csub, userbuf, _nchars);
    lvars buf, dev, userbuf, do_write, _take, _pos, _nchars, _csub, _encode;
    if (dev!D_IN_BUFFER ->> buf)
    and buf!BUF_POSITION _lt buf!BUF_COUNT
    and dev!D_SEEK == File_seek then
        ;;; have to set file position back
        File_seek(dev, 0, 1) ->
    endif;
    dev!D_OUT_BUFFER -> buf;
    _int(_csub) _sub _1 -> _csub;
    _int(_nchars) -> _nchars;

    if fast_idval(dev!D_ENCODING_ID) ->> _encode then
        lstackmem int _ilenp, int _olenp;
        lvars _encode_flags = _BYTE_INPUT;
        _encode!XP_PTR!CDFN_ENCODE -> _encode;
        if userbuf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _0 -> _encode_flags
        endif;
        until _zero(_nchars) do
            buf!BUF_POSITION -> _pos;
            _nchars ->> _take -> _ilenp!(int);
            buf!BUF_SIZE _sub _pos -> _olenp!(int);
            _extern[INDIR] _encode( if _encode_flags _bitst _BYTE_INPUT then
                                        userbuf@(w->b)[_csub]
                                    else
                                        userbuf@(w->s)[_csub]
                                    endif, _ilenp,
                                    buf@BUF_START[_pos], _olenp,
                                    buf@BUF_ENCODE_STATE, _encode_flags) -> ;
            _ilenp!(int) -> _nchars;            ;;; number remaining
            _csub _add _take _sub _nchars -> _csub;
            _olenp!(int) _add _pos ->> _pos -> buf!BUF_POSITION;
            if _pos == buf!BUF_SIZE or _nonzero(_nchars) then
                Write_dev_buffer(dev)
            endif
        enduntil;
        return
    endif;

    if _nchars == _1 then
        ;;; special for discout, etc
        buf!BUF_POSITION -> _pos;
        if _pos _greq buf!BUF_SIZE then
            Write_dev_buffer(dev);
            buf!BUF_POSITION -> _pos
        endif;
        userbuf!(w->b)[_csub] -> buf!BUF_START[_pos];
        _pos _add _1 ->> _pos -> buf!BUF_POSITION;
        if _pos == buf!BUF_SIZE then Write_dev_buffer(dev) endif;
        return
    endif;

    until _zero(_nchars) do
        buf!BUF_POSITION -> _pos;
        _nchars -> _take;
        false -> do_write;
        if _pos _add _take _greq buf!BUF_SIZE then
            buf!BUF_SIZE _sub _pos -> _take;
            true -> do_write
        endif;
        _bmove(@@(b)[_take], userbuf@(w->b)[_csub], buf@BUF_START[_pos]) -> ;
        _pos _add _take -> buf!BUF_POSITION;
        _nchars _sub _take -> _nchars;
        _csub _add _take -> _csub;
        if do_write then Write_dev_buffer(dev) endif
    enduntil
enddefine;

    /*  Buffered file for block I/O
    */
define lconstant Blockio_write(dev, _bsub, userbuf, _nbytes);
    lvars buffer, dev, userbuf, do_write, _take, _pos, _nbytes, _bsub;
    if (dev!D_IN_BUFFER ->> buffer)
    and buffer!BUF_POSITION _lt buffer!BUF_COUNT
    and dev!D_SEEK == File_seek then
        ;;; have to set file position back
        File_seek(dev, 0, 1) ->
    endif;
    dev!D_OUT_BUFFER -> buffer;
    _int(_bsub) _sub _1 -> _bsub;
    _int(_nbytes) -> _nbytes;

    until _zero(_nbytes) do
        buffer!BUF_POSITION -> _pos;
        _nbytes -> _take;
        false -> do_write;
        if _pos _add _take _greq buffer!BUF_SIZE then
            buffer!BUF_SIZE _sub _pos -> _take;
            true -> do_write
        endif;
        _bmove(@@(b)[_take], userbuf@(w->b)[_bsub], buffer@BUF_START[_pos]) -> ;
        _pos _add _take -> buffer!BUF_POSITION;
        _nbytes _sub _take -> _nbytes;
        _bsub _add _take -> _bsub;
        if do_write then
            Write_dev_buffer(dev);
            if _nbytes _greq buffer!BUF_SIZE then
                ;;; don't bother to put rest through device buffer
                Do_write(_bsub, userbuf, _nbytes, dev);
                return
            endif
        endif
    enduntil
enddefine;

    /*  Write to interactive device, taking newline etc as end of record
    */
define lconstant Interact_write(dev, _csub, userbuf, _nchars);
    lvars   buf = dev!D_OUT_BUFFER, dev, userbuf, do_write,
            _take, _pos, _nchars = _int(_nchars), _bptr,
            _csub = _int(_csub) _sub _1, _encode;

    if fast_idval(dev!D_ENCODING_ID) ->> _encode then
        lstackmem int _ilenp, int _olenp;
        lvars _encode_flags = _BYTE_INPUT _biset _STOP_AT_NEWLINE;
        _encode!XP_PTR!CDFN_ENCODE -> _encode;
        if userbuf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _STOP_AT_NEWLINE -> _encode_flags
        endif;
        until _zero(_nchars) do
            buf!BUF_POSITION -> _pos;
            _nchars ->> _take -> _ilenp!(int);
            buf!BUF_SIZE _sub _pos -> _olenp!(int);
            _extern[INDIR] _encode( if _encode_flags _bitst _BYTE_INPUT then
                                        userbuf@(w->b)[_csub]
                                    else
                                        userbuf@(w->s)[_csub]
                                    endif, _ilenp,
                                    buf@BUF_START[_pos], _olenp,
                                    buf@BUF_ENCODE_STATE, _encode_flags) -> ;
            _ilenp!(int) -> _nchars;            ;;; number remaining
            _csub _add _take _sub _nchars -> _csub;
            _olenp!(int) _add _pos ->> _pos -> buf!BUF_POSITION;
            if _zero(_nchars) then
                ;;; inspect last char taken
                if _encode_flags _bitst _BYTE_INPUT then
                    userbuf!(w->b)[_csub _sub _1] -> _take
                else
                    userbuf!(w->s)[_csub _sub _1] -> _take
                endif;
                quitif(_take _greq _:`\s` and _pos _lt buf!BUF_SIZE
                        and pop_buffer_charout)
            endif;
            Write_dev_buffer(dev)
        enduntil;
        return
    endif;

    if _nchars == _1 then
        buf!BUF_POSITION -> _pos;
        if _pos _greq buf!BUF_SIZE then
            Write_dev_buffer(dev);
            buf!BUF_POSITION -> _pos
        endif;
        userbuf!(w->b)[_csub] ->> _take -> buf!BUF_START[_pos];
        _pos _add _1 ->> _pos -> buf!BUF_POSITION;
        unless _take _greq _:`\s` and _pos _lt buf!BUF_SIZE
        and pop_buffer_charout
        then
            Write_dev_buffer(dev)
        endunless;
        return
    endif;

    until _zero(_nchars) do
        buf!BUF_POSITION -> _pos;
        ;;; find newline or end
        userbuf@(w->b)[_csub] -> _bptr;
        if _nonneg(_locc(_bptr, @@(b)[_nchars], _:`\n`) ->> _take) then
            ##(b){_take}++ -> _take;
            true -> do_write
        else
            _nchars -> _take;
            false -> do_write
        endif;
        if _pos _add _take _greq buf!BUF_SIZE then
            buf!BUF_SIZE _sub _pos -> _take;
            true -> do_write
        endif;
        _bmove(@@(b)[_take], _bptr, buf@BUF_START[_pos]) -> ;
        _nchars _sub _take -> _nchars;
        _csub _add _take -> _csub;
        _pos _add _take -> buf!BUF_POSITION;
        if do_write or not(pop_buffer_charout) then Write_dev_buffer(dev) endif
    enduntil
enddefine;


;;; --- FLUSHING -------------------------------------------------------------

define lconstant File_flush(dev);
    lvars dev;
    unless _zero(dev!D_OUT_BUFFER!BUF_POSITION) then
        Write_dev_buffer(dev)
    endunless
enddefine;


;;; --- SEEKING ---------------------------------------------------------------

define lconstant File_seek(dev, _pos, _mode);
    lvars dev, buffer, _pos, _mode, _count;
    unless dev!D_WRITE == No_write then
        fast_apply(dev, dev!D_FLUSH)
    endunless;
    _int(_pos) -> _pos;
    _int(_mode) -> _mode;
    if (dev!D_IN_BUFFER ->> buffer)
    and _nonzero(buffer!BUF_COUNT _sub buffer!BUF_POSITION ->> _count) then
        ;;; _count chars in input buffer
        if _mode == _1 then
            ;;; relative to current position - set back
            _pos _sub _count -> _pos
        endif;
        _0 ->> buffer!BUF_COUNT -> buffer!BUF_POSITION
    endif;
    if _neg(_extern lseek(dev!D_FILE_DESC, _pos, _mode) ->> _pos)
    then
        Syserr_mishap(dev, 1, 'ERROR SEEKING DEVICE')
    endif;
    _pint(_pos)
enddefine;


;;; --- CLOSING --------------------------------------------------------------

define Sync_or_close(dev, _routine);
    lvars dev, _routine;
    while _neg(_extern[INDIR, SE] _routine(dev!D_FILE_DESC)) do
        if _ERRNO == _:EINTR then _CHECKINTERRUPT, nextloop endif;
        ;;; some error syncing/closing the file
        if _ERRNO == _:EDQUOT or _ERRNO == _:EFBIG
        or _ERRNO == _:ENOSPC or _ERRNO == _:EIO then
            chain(dev, Write_error)     ;;; do nothing if this returns
        endif;
        quitloop
    endwhile
enddefine;

define lconstant File_close(dev);
    lvars dev, unit_n = dev!D_UNIT_N;
    unless dev!D_WRITE == No_write then
        fast_apply(dev, dev!D_FLUSH)
    endunless;

    if dev!D_FLAGS _bitst _M_D_TERMINAL and unit_n!UNT_NDEVS == _1 then
        ;;; no more devices referring to this tty -- reset init params
        Reset_init_params(dev)
    endif;

    ;;; close- is interruptable ...
    Sync_or_close(dev, _extern pop_close);

    unit_n!UNT_NDEVS _sub _1 -> unit_n!UNT_NDEVS;
    dev!D_FLAGS _biset _M_D_CLOSED -> dev!D_FLAGS;
    -1 -> dev!D_FILE_DESC
enddefine;


;;; --- OPENING -------------------------------------------------------------

define lconstant Set_dev_unit(newdev, arg3, _isterm);
    lvars tcb, unit_n, unit_p, newdev, arg3, _isterm;
    lconstant _V0LEN = ##V_WORDS _sub ##POPBASE;

    define lconstant Get_dev_unit(newdev, _isterm) -> (unit_n, unit_p);
        lvars unit_n, unit_p, newdev, _isterm, _mode;
        dlvars _udev, _udev2, _uino, _ifmt;
        lstackmem struct STATB _statb;

        define lconstant Test_same_unit(dev);
            lvars unit = dev!D_UNIT_N, dev;
            ;;; these tests don't work with terminals if one has been
            ;;; opened as /dev/tty !!
            if  unit!UNT_MODE _bimask _STM_IFMT == _ifmt
            and DEV_T_==_VARS(unit, UNT_DEV_NUM, _udev, _udev2)
            and unit!UNT_INO_NUM == _uino
            then
                ;;; same unit - return the nonpop and pop unit structures to
                ;;; Set_dev_unit
                exitto( (unit, dev!D_UNIT_P), Set_dev_unit)
            endif
        enddefine;
        while  _neg(_extern[SE] fstat(newdev!D_FILE_DESC, _statb))
               and _ERRNO == _:EINTR do
            _CHECKINTERRUPT
        endwhile;
        DEV_T_TO_VARS(_statb, ST_DEV, _udev, _udev2);
        _statb!ST_INO -> _uino;         ;;; unix ino
        _statb!ST_MODE -> _mode;        ;;; unix mode
        _mode _bimask _STM_IFMT -> _ifmt;

        ;;; Determine whether any existing device refers to this one.
        ;;; Test_same_unit exits to Set_dev_unit if one does
        App_open_devs(Test_same_unit);

        ;;; Reach here when no device is open for this unit.
        ;;; Return new unit structures which will be in the
        ;;; D_UNIT_N and D_UNIT_P fields of any device refering to this
        ;;; unit.

        inits(_pint( ##(b)[_1|struct DEVUNIT_N] _sub ##(b)[_V0LEN|w] )),
        initv(_pint( ##(w)[_1|struct DEVUNIT_P] _sub _V0LEN ))
                    -> (unit_n, unit_p);

        ;;; init fields in nonpop string
        _0      -> unit_n!UNT_NDEVS;        ;;; 0 ref count
        VARS_TO_DEV_T(_udev, _udev2, unit_n, UNT_DEV_NUM);
        _uino   -> unit_n!UNT_INO_NUM;      ;;; unix inode
        _mode   -> unit_n!UNT_MODE;         ;;; unix mode
        _0      -> unit_n!UNT_FLAGS;        ;;; flags

        ;;; init fields in pop vector
        false   ->> unit_p!UNT_IO_TRAP[_:RD_SET]    ;;; input ready
                ->> unit_p!UNT_IO_TRAP[_:WR_SET]    ;;; output ready
                ->  unit_p!UNT_IO_TRAP[_:EX_SET];   ;;; exception

        returnunless(_isterm);
        ;;; rest are for terminals only
        nullstring  -> unit_p!UNT_CURR_TTPARAMS;
        ;;; params string for initial state of tty
        inits(_pint( ##(b)[_1|struct TTY_PARAMS] _sub ##(b)[_V0LEN|w] ))
                    -> unit_p!UNT_INIT_TTPARAMS;
        ATT_PARM_INVALID
                    -> unit_p!UNT_TTPARM_STATUS; ;;; params initialiy invalid
    enddefine;      /* Get_dev_unit */


    ;;; Get a pair of unit structures for the new device -- one for
    ;;; nonpop values and one for pop values.

    Get_dev_unit(newdev, _isterm) -> (unit_n, unit_p);
    (unit_n, unit_p) -> (newdev!D_UNIT_N, newdev!D_UNIT_P);
    unless _vfork_child then
        ;;; increase ref count for unit
        unit_n!UNT_NDEVS _add _1 -> unit_n!UNT_NDEVS
    endunless;

    returnunless(_isterm);
    ;;; Create a Terminal Control Block (a vector) for the terminal -- this
    ;;; contains the params string used for the device
    initv(_pint( ##(w)[_1|struct TTY_CTRL_BLK] _sub _V0LEN )) -> tcb;
    copy(unit_p!UNT_INIT_TTPARAMS)
                -> tcb!TCB_PARAMS;
    arg3        -> tcb!TCB_ARG3;
    false       -> tcb!TCB_PARAMS_SETUP;
    true        -> tcb!TCB_PMODE;   ;;; purge input on switching
    tcb -> newdev!D_CTRL_BLK
enddefine;      /* Set_dev_unit */


    /*  This procedure is exported.
    */
define Sys_cons_device(file, fullname, accmode, arg3, fd, _interactive) -> dev;
    lvars   dev, file, fullname, accmode, arg3, fd, _interactive,
            _fd = _int(fd), _insize, _outsize, _isterm;

    _extern[NI] fcntl(_fd, _F_SETFD, _1) -> ; ;;; set for close on exec

    Init_device() -> dev;
    _fd         -> dev!D_FILE_DESC;
    file        -> dev!D_OPEN_NAME;
    File_close  -> dev!D_CLOSE;
    if fullname then
        if fullname = nullstring or (fullname!V_BYTES[_0] /== _:`/`) then
            current_directory dir_>< fullname
        else
            copy(fullname)
        endif -> dev!D_FULL_NAME
    endif;

    Is_terminal(_fd) -> _isterm;
    Set_dev_unit(dev, arg3, _isterm);

    accmode fi_&& O_ACCESS -> accmode;
    0 ->> _insize -> _outsize;
    if _isterm then
        ;;; terminal
        _M_D_TERMINAL _biset _M_D_LOGICAL_TERM _biset _M_D_INTERACTIVE,
        unless arg3 then () _biset _M_D_TERM_PROMPT endunless -> dev!D_FLAGS;
        if accmode /== O_RDONLY then
            if arg3 then File_write else Interact_write endif -> dev!D_WRITE;
            128 -> _outsize;
            File_flush -> dev!D_FLUSH
        endif;
        if accmode /== O_WRONLY then
            if accmode == O_RDONLY and fullname then
                ;;; re-open a reading terminal for reading/writing
                ;;; to ensure that the prompt can be written
                _extern[NI] pop_close(_fd) -> ;
                Opencreate(file, fullname, O_RDWR, 0) -> dev!D_FILE_DESC
            endif;
            256 -> _insize;
            Interact_read -> dev!D_READ;
            Clear_term_input -> dev!D_CLEAR_INPUT
        endif;
        false -> arg3   ;;; terminal is always 'character' mode

    else
        ;;; anything but a terminal
        if _interactive
        or dev!D_UNIT_N!UNT_MODE _bimask _STM_IFMT == _STM_IFIFO
#_IF DEFV SYSTEM_V >= 4.0 or DEF AIX
        or _nonzero(_extern isastream(_fd))
#_ENDIF
        then
            ;;; pipe/fifo/socket etc.
            _M_D_INTERACTIVE
        else
            File_seek -> dev!D_SEEK;
            _0
        endif -> dev!D_FLAGS;
        if arg3 == "record" then "line" -> arg3 endif;
        if accmode /== O_RDONLY then
            ;;; can be written
            if arg3 and arg3 /== "line" then
                Blockio_write
            elseif dev!D_FLAGS _bitst _M_D_INTERACTIVE then
                Interact_write
            else
                File_write
            endif -> dev!D_WRITE;
            File_flush -> dev!D_FLUSH;
            512 -> _outsize
        endif;
        if accmode /== O_WRONLY then
            ;;; can be read
            if arg3 == "line" then
                ;;; a record/line at a time
                Line_read
            elseif arg3 then
                Blockio_read
            elseif dev!D_FLAGS _bitst _M_D_INTERACTIVE then
                Interact_read
            else
                File_read
            endif -> dev!D_READ;
            if dev!D_FLAGS _bitst _M_D_INTERACTIVE then
                weakref[sys_clear_input] Clear_input
            else
                erase
            endif -> dev!D_CLEAR_INPUT;
            512 -> _insize
        endif
    endif;

    if arg3 and arg3 /== "line" then
        ;;; not 'character' mode -- set D_ENCODING_ID to constant id
        ;;; containing false
        ident no_device_encoding -> dev!D_ENCODING_ID
    endif;

    if accmode /== O_WRONLY then
        weakref[sys_input_waiting] Test_input -> dev!D_TEST_INPUT
    endif;

    Cons_iobuffer(_insize) -> dev!D_IN_BUFFER;
    Cons_iobuffer(_outsize) -> dev!D_OUT_BUFFER;

    ;;; add file to file table
    ;;; Don't add the new device to the file table if we're a vfork'ed child,
    ;;; since a GC in the parent could try to close it as garbage (and thus
    ;;; close a file with the same file descriptor in the parent).
    unless _vfork_child then Add_file_tab_entry(dev) endunless
enddefine;


    /*  This procedure is exported.
    */
define Sys_fd_open_check(_result, _close_on_exec, _retry) /* -> _retry */;
    lvars _result, _close_on_exec, _retry;
    if _result fi_>= 0 then
        ;;; success -- _result is a file descriptor, set for close-on-exec
        ;;; if required
        if _close_on_exec then
            _extern[NI] fcntl(_int(_result), _F_SETFD, _1) ->
        endif;
        -1              ;;; success, don't retry

    elseif _ERRNO == _:ENFILE       ;;; File table overflow
        or _ERRNO == _:EMFILE       ;;; Too many open files
        or _ERRNO == _:ENOSR        ;;; Out of stream resources
    then
        ;;; too many files open etc
        if _retry == 1 then
            ;;; garbage collect and try again
            Sysgarbage(true, 'fopn');
            0           ;;; retry, but no further GC
        else
            -2          ;;; fd 'resource' error, don't retry
        endif

    elseif _ERRNO == _:EINTR then
        ;;; interrupted -- service and try again
        _CHECKINTERRUPT;
        _retry          ;;; unchanged

    else
        -3              ;;; other error, don't retry
    endif
enddefine;

    /*  _mode is the permissions when _oflags specifies create
    */
define Opencreate(file, fullname, _oflags, _mode) -> _fd;
    lvars   fullname, file, _oflags = _int(_oflags), _mode = _int(_mode),
            _fd, _retry = 1;
    lstackmem stackbuff _nbuf;
    repeat
        _extern[SE] open(Encode_sys(fullname,_nbuf), _oflags, _mode) -> _fd;
        quitif((Sys_fd_open_check(_pint(_fd), true, _retry) ->> _retry) fi_< 0)
    endrepeat;

    if _retry == -2 then
        ;;; fd 'resource' error, mishap now
        Syserr_mishap(file, 1, 'CAN\'T OPEN/CREATE FILE')
    endif;
enddefine;


;;; --- OTHER -----------------------------------------------------------

define Get_devio_trap(_fd, _ioset);
    dlvars _fd, _ioset;
    App_open_devs(
            procedure(dev);
                lvars dev, p;
                if (dev!D_FILE_DESC == _fd)
                and (dev!D_UNIT_P!UNT_IO_TRAP[_ioset] ->> p) then
                    exitfrom(p, Get_devio_trap)
                endif
            endprocedure);
    false
enddefine;


endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 20 1999
        Changes to fix x86 Linux "dev_t" type problem
--- John Gibson, May 15 1998
        Changed test for interactive device in Sys_cons_device
--- John Gibson, Apr  4 1997
        Replaced D_ENCODING with D_ENCODING_ID
--- John Gibson, Mar  8 1997
        Added Encode_sys in Opencreate.
--- John Gibson, Mar  5 1997
        Replaced use of sys*string in Is_terminal with lstackmem for
        appropriate struct.
--- John Gibson, Feb 25 1997
        Device encoding changes.
--- John Gibson, Sep  4 1996
        Fixes to make output blocking devices deal correctly with recursive
        use of the device inside an interrupt inside the write.
--- John Gibson, May 25 1996
        Fixed Line_read so that the whole of a line will be read if the
        user buffer is big enough (i.e. even if the device buffer is not).
--- John Gibson, Feb 27 1995
        Made Write_error set M_UNT_IGNORE_WRITE_ERR flag when called on a
        device which is standard output/error (this stops Write_error
        producing any further mishaps for that device).
        Removed _wr*ite_error_tries since this is no longer necessary (and
        didn't work anyway, owing to the behaviour of mishap in trapping
        recursive mishaps).
--- John Gibson, Feb 24 1995
        Replaced Is_back*ground_term with Is_foreground_term (which will
        return false if terminal device has been closed)
--- John Gibson, Feb  6 1995
        Fixed bug at beginning of Blockio_write where it was liable to call
        File_seek on a non-seekable input/output device (e.g.socket) opened
        with 3rd arg true.
--- John Gibson, Jul 11 1994
        Changes to Do_write for async output
--- John Gibson, May 24 1994
        Exported Cons*_device as Sys_cons_device and Fd*_open_check as
        Sys_fd_open_check.
--- John Gibson, May 23 1994
        _sys*error -> _ERRNO
--- John Gibson, May 16 1994
        Added [NI] flags to _externs that may be interruptible
--- John Gibson, May  6 1994
        Split off the result-checking part of Opencreate as Fd*_open_check.
--- John Gibson, May  5 1994
        Changes to Do_read for async input
--- John Gibson, Feb 15 1994
        Added disabling of LNEXT control character for USE_TERMIOS raw mode
        in Tty_param_init
--- John Gibson, Jan 29 1994
        Changed Opencreate to use _extern open with modern _oflags and
        _mode args.
--- John Gibson, Sep 28 1993
        Added Sync_or_close
--- Robert John Duncan, Feb  9 1993
        Changed Tty_param_init to clear all output CR/NL translations under
        termio(s) in raw mode to improve compatibility with the old BSD
        interface (still available from the ioctl libraries).
--- John Gibson, Dec  8 1992
        Xpt_read_wait now returns 0 for interrupt
--- John Gibson, Nov 20 1992
        For device not known to be interactive, Made Do_read loop and try
        read again after EINTR
--- John Gibson, Oct 12 1992
        Reworked handling of write errors to prevent potential error loops
--- Robert John Duncan, Sep 19 1992
        Changed Tty_param_init not to disable parity generation under
        termio(s)
--- John Gibson, Jun 29 1992
        Moved code from -No_fd_close- into -File_close-
--- Robert John Duncan, Jun 16 1992
        System calls {close,read} replaced by pop_{close,read} from
        "c_core.c"
--- Robert John Duncan, Jun  2 1992
        TERMIO and BSD_TTY cases no longer overlap
--- Robert John Duncan, Jun  1 1992
        Added support for termios
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, Aug  2 1991
        Changes to ensure that interrupted -write- system calls are handled
        properly.
--- John Gibson, Jun  4 1991
        Improved handling of _terminal_io_done
--- John Gibson, Feb 23 1991
        Aded test for FIFO in Cons*_device etc
--- John Gibson, Jan 15 1991
        Rewrote -Do_read- and -Do*_interact_read-, etc.
--- John Gibson, Jan  5 1991
        Changed -Do*_interact_read- to use syssettimer instead of _extern alarm.
--- John Gibson, Jan  3 1991
        Replaced R*EAD_EI with read.
--- John Gibson, Dec 12 1990
        Replaced A_TTY structure for terminals with general DEVUNIT_N/P
        structures identifying the underlying unit for any device, held
        in D_UNIT_N/P fields.
--- John Gibson, Nov 27 1990
        Added code to -File_close- to check for write errors.
--- John Gibson, Oct 26 1990
        Made flags for real terminal and logical terminal distinct.
--- John Gibson, Oct 24 1990
        -Cons*_device- changed to set up clear/test input procedures
        in devices.
--- John Gibson, Oct 22 1990
        Changed all device read/write/seek procedures to take args
        (dev, _bsub, userbuf, _nbytes), where all are pop values.
--- John Gibson, Sep 28 1990
        Added checks for EINTR in -Opencreate- and -Do_read-
--- John Gibson, Sep  1 1990
        Moved Cons_iobuffer to miscio.p
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Aug 24 1990
        Reorganised async code
--- Roger Evans, Jul  2 1990 minor bug fixes to async code
--- John Gibson, Jun  6 1990
        Change to -Cons*_device- to call -Cons_device_getdev-.
--- John Gibson, Feb 27 1990
        Added extra arg to call of -Sysgarbage-.
--- John Gibson, Jan 25 1990
        Made _vfork_child test unconditional in Unix.
 */
