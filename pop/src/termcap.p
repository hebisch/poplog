/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/src/termcap.p
 > Purpose:         Simple interface to C termcap library
 > Author:          Rob Duncan, Apr 11 1989 (see revisions)
 */

    /*  NCR's termcap/curses library screws up X when used in
        anything other than the C locale (it includes its own
        definitions of the mbstring functions).
        Who needs it anyway?
    */
#_TERMIN_IF DEF NCR

#_TERMIN_IF false

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'unix_tty.ph'

global constant procedure (
    isnumbercode,
);

global vars procedure (
    sysLCONSTANT,
    sysLVARS,
    sysLOCAL,
    sysPASSIGN,
);

section $-Sys =>
    termcap_compile,
    termcap_getentry,
    termcap_getflag,
    termcap_getnum,
    termcap_getstring,
    termcap_name,
;

constant procedure (
    Get_string,
    Io$-Fix_dev_params,
);

/*
 *  Dummy exload to incorporate termcap library
 */
exload termcap
#_IF DEF OSF1 or DEFV IRIX >= 6.0 or DEF AIX
    ['-lcurses']
#_ELSEIF DEFV IRIX >= 5.0
    ;;; names in the curses library clash with names defined in the
    ;;; graphics library libgl; since we don't use those names, linking
    ;;; statically will pull in just the names we want and minimise the
    ;;; problem
    ['-B static -lcurses -B dynamic']
#_ELSE
;;; ['-ltermcap']
        ['-lncurses']
#_ENDIF
    lconstant exload_dummy;     ;;; anything will do
endexload;


/*
 *  Terminal Delay Times
 */

;;; time in tenths of milliseconds to output a single character
;;; for each possible tty speed

#_IF DEF HPUX

lconstant TTY_OSPEED = {
    10000   ;;; B0
    2000    ;;; B50
    1333    ;;; B75
    909     ;;; B110
    746     ;;; B134
    667     ;;; B150
    500     ;;; B200
    333     ;;; B300
    167     ;;; B600
    111     ;;; B900
    83      ;;; B1200
    56      ;;; B1800
    42      ;;; B2400
    28      ;;; B3600
    21      ;;; B4800
    14      ;;; B7200
    10      ;;; B9600
    5       ;;; B19200
    3       ;;; B38400
};

#_ELSE

lconstant TTY_OSPEED = {
    10000   ;;; B0
    2000    ;;; B50
    1333    ;;; B75
    909     ;;; B110
    746     ;;; B134
    667     ;;; B150
    500     ;;; B200
    333     ;;; B300
    167     ;;; B600
    83      ;;; B1200
    56      ;;; B1800
    42      ;;; B2400
    21      ;;; B4800
    10      ;;; B9600
    5       ;;; B19200
    3       ;;; B38400
};

#_ENDIF

lconstant
    _TTY_OSPEED_MAX = datalength(TTY_OSPEED),
;

lvars
    _tty_ospeed = false,
        ;;; time to output a single character on the current terminal
    _tty_padchar = `\^@`,
        ;;; character to output to cause delays
;

;;; set_tty_ospeed:
;;;     determines the character-output time of the terminal according to
;;;     its output baud rate

define lconstant set_tty_ospeed();
    lvars params, _i = 1;
    returnif(_tty_ospeed);
    if testdef poprawdevin
    and isdevice(weakref[poprawdevin] raw_dev_out)
    and weakref[poprawdevin] raw_dev_out!D_FLAGS _bitst _M_D_TERMINAL
    and (Io$-Fix_dev_params(weakref[poprawdevin] raw_dev_out, true) ->> params)
    then
#_IF DEF USE_TERMIOS or DEF USE_TERMIO
  #_IF DEF OSF1
        params@TTP_SGTTYB!TMIO_OSPEED -> _i;
  #_ELSE
        params@TTP_SGTTYB!TMIO_CFLAG _bimask _TTY_CBAUD -> _i;
  #_ENDIF
#_ELSE
        params@TTP_SGTTYB!SGT_OSPEED -> _i;
#_ENDIF
        _pint(_i) fi_+ 1 -> _i;
        if _i fi_> _TTY_OSPEED_MAX then _TTY_OSPEED_MAX -> _i endif;
    endif;
    fast_subscrv(_i, TTY_OSPEED) -> _tty_ospeed;
enddefine;

;;; padout:
;;;     output (using procedure p) sufficient pad characters to cause a
;;;     delay _d (in tenths of milliseconds) for each of _n lines affected
;;;     where the time to output one character on the terminal is _t

define lconstant padout(_d, _n, _t, p);
    lvars procedure p, _d, _t, _n;
    ((_n fi_* _d) fi_+ (_t fi_>> 1)) fi_div _t -> _d;
    until _d == 0 do
        p(_tty_padchar);
        _d fi_- 1 -> _d;
    enduntil;
enddefine;

;;; getdelay:
;;;     decodes the delay requirement from string -s- starting at position
;;;     -i-. Returns index of first character after the delay, the delay
;;;     itself (scaled to tenths of milliseconds) and a flag to show whether
;;;     it should vary according to the number of lines affected.

define lconstant getdelay(_i, s) -> _i -> _delay -> vardelay;
    lvars s, vardelay = false, _c, _i, _delay = 0, _len = datalength(s);
    ;;; read leading digits (milliseconds)
    while _i fi_<= _len and isnumbercode(fast_subscrs(_i, s) ->> _c) do
        _delay fi_* 10 fi_+ _c fi_- `0` -> _delay;
        _i fi_+ 1 -> _i;
    endwhile;
    ;;; convert to tenths of milliseconds
    _delay fi_* 10 -> _delay;
    ;;; take account of one digit after the decimal point
    if _i fi_<= _len and _c == `.` then
        _i fi_+ 1 -> _i;
        if _i fi_<= _len and isnumbercode(fast_subscrs(_i, s) ->> _c) then
            _delay fi_+ _c fi_- `0` -> _delay;
            _i fi_+ 1 -> _i;
            ;;; ignore any other digits after the decimal point
            while _i fi_<= _len and isnumbercode(fast_subscrs(_i, s) ->> _c)
            do
                _i fi_+ 1 -> _i;
            endwhile;
        endif;
    endif;
    ;;; check for variable delay
    if _i fi_<= _len and _c == `*` then
        true -> vardelay;
        _i fi_+ 1 -> _i;
    endif;
enddefine;


/*
 *  Compiling Termcap Strings
 */

;;; termcap_compile:
;;;     compiles a termcap string capability, expanding any delay requirements
;;;     and parameter escape sequences

define termcap_compile(padding);
    lvars   s, args, nargs, padding, vardelay, output = "rawcharout",
            ansi = false, inproc = false, _c, _len, _delay, _i = 1, _n = 0;

    define lconstant error();
        mishap(0, 'ERROR COMPILING TERMCAP STRING');
    enddefine;

    define lconstant nextc();
        if _i fi_> _len then error() endif;
        fast_subscrs(_i, s);
        _i fi_+ 1 -> _i;
    enddefine;

    define lconstant startproc();
        returnif(inproc);
        unless nargs then 0 -> nargs endunless;
        sysPROCEDURE(false, if padding == true then nargs + 1 else nargs endif);
        unless isword(output) then
            sysLCONSTANT("output", "procedure");
            sysPASSIGN(output, "output" ->> output);
        endunless;
        [% fast_repeat nargs times sysNEW_LVAR() endrepeat %] -> args;
        if padding == true then
            sysLVARS("affcnt", 0), sysPOP("affcnt");
        endif;
        applist(rev(args), sysPOP);
        true -> inproc;
    enddefine;

    define lconstant flush();
        startproc();
        unless _n == 0 then
            sysPUSHQ(unless _n == 1 then consstring(_n) endunless);
            sysCALL(output);
            0 -> _n;
        endunless;
    enddefine;

    define lconstant endproc();
        returnunless(inproc);
        flush();
        sysENDPROCEDURE();
    enddefine;

    define lconstant do_pad();
        dlocal padding;
        if padding and _delay /== 0 then
            set_tty_ospeed();
            if not(vardelay) then 1 -> padding endif;
            if isinteger(padding) then
                (#| padout(_delay, padding, _tty_ospeed, identfn) |#)
                    fi_+ _n -> _n;
            else
                flush();
                sysPUSHQ(_delay), sysPUSH("affcnt"), sysPUSHQ(_tty_ospeed),
                    sysPUSH(output), sysCALLQ(padout);
            endif;
        endif;
    enddefine;

    define lvars do_dlocal();
        ;;; dlocal cucharout = output, pop_pr_radix = 10;
        sysLOCAL("cucharout"), sysPUSH(output), sysPOP("cucharout");
        sysLOCAL("pop_pr_radix"), sysPUSHQ(10), sysPOP("pop_pr_radix");
        identfn -> do_dlocal;
    enddefine;

#_IF DEF USE_TERMINFO

    /* parameter escapes, terminfo format */

    lvars label_stack = [];

    define lconstant checkc(c, l, u) -> c;
        lvars c, l, u;
        if c fi_< l or c fi_> u then error() endif;
    enddefine;

    define lconstant operator =
        newassoc([
            [`l`        ^datalength]
            [`+`        ^fi_+]
            [`-`        ^fi_-]
            [`*`        ^fi_*]
            [`/`        ^fi_div]
            [`m`        ^fi_rem]
            [`&`        ^fi_&&]
            [`|`        ^fi_||]
            [`^`        ^fi_||/&]
            [`~`        ^fi_~~]
            [`=`        %procedure; if == then 1 else 0 endif endprocedure%]
            [`>`        %procedure; if fi_> then 1 else 0 endif endprocedure%]
            [`<`        %procedure; if fi_< then 1 else 0 endif endprocedure%]
            [`!`        %procedure; if == 0 then 1 else 0 endif endprocedure%]
            [`A`        %procedure; if == 0 then ->, 0 elseif == 0 then 0 else 1 endif endprocedure%]
            [`O`        %procedure; if /== 0 then ->, 1 elseif /== 0 then 1 else 0 endif endprocedure%]
        ])
    enddefine;

    define lconstant do_escape();
        lvars c = nextc(), x;
        if c == `c` then
            ;;; %c : output top of stack
            sysCALL(output);
        elseif c == `d` then
            ;;; %d : print top of stack as ascii integer
            do_dlocal(), sysCALL("sys_syspr");
        elseif c fi_>= `0` and c fi_<= `9` then
            ;;; %nd : as above, but use given field width padded with spaces
            ` ` -> x;
            if c == `0` then
                ;;; pad with zero instead
                `0` -> x;
                checkc(nextc(), `1`, `9`) -> c;
            endif;
            checkc(nextc(), `d`, `d`) -> ;
            do_dlocal(), sysPUSHQ(c - `0`), sysPUSHQ(x), sysPUSHQ(false),
                sysPUSH("sys_syspr"), sysCALL("pr_field");
        elseif c == `p` then
            ;;; %p[1-9]: push numbered parameter
            checkc(nextc(), `1`, `9`) fi_- `0` -> x;
            sysPUSH(args(x));
            unless ansi and x fi_<= 2 then sysPUSHQ(1), sysCALL("fi_-") endunless;
        elseif c == `P` then
            ;;; %P[a-z]: pop to named argument
            consword(checkc(nextc(), `a`, `z`), 1) -> c;
            sysLVARS(c, 0), sysPOP(c);
        elseif c == `g` then
            ;;; %g[a-z]: push named argument (should already be set)
            consword(checkc(nextc(), `a`, `z`), 1) -> c;
            sysPUSH(c);
        elseif c == `'` then
            ;;; %'c' : push character constant
            sysPUSHQ(nextc());
            checkc(nextc(), `'`, `'`) -> ;
        elseif c == `{` then
            ;;; %{nn} : push decimal constant
            0 -> x;
            until (nextc() ->> c) == `}` do
                x fi_* 10 fi_+ checkc(c, `0`, `9`) fi_- `0` -> x;
            enduntil;
            sysPUSHQ(x);
        elseif operator(c) ->> x then
            sysCALLQ(x);
        elseif c == `i` then
            ;;; %i : ansi terminal; add 1 to first two parameters
            true -> ansi;
        elseif c == `?` then
            ;;; %? : start conditional
            ;;; Put marker and end-label on -label_stack-
            sysNEW_LABEL() :: (false :: label_stack) -> label_stack;
        elseif c == `t` then
            ;;; %t : `then' part of conditional
            ;;; Test top of stack; push else-label on -label_stack-
            sysPUSHQ(0), sysCALL("=="), sysIFSO(sysNEW_LABEL() ->> x);
            x :: label_stack -> label_stack;
        elseif c == `e` then
            ;;; %e : `else' part of conditional
            ;;; Goto end-label (after `then' part); plant else-label
            dest(label_stack) -> label_stack -> x;
            sysGOTO(front(label_stack));
            sysLABEL(x);
        elseif c == `;` then
            ;;; %; : end conditional
            ;;; Plant labels from -label_stack- down to marker
            while dest(label_stack) -> label_stack ->> x do
                sysLABEL(x);
            endwhile;
        elseif c == `%` then
            ;;; %% : output single `%`
            sysPUSHQ(`%`), sysCALL(output);
        else
            error();
        endif;
    enddefine;

#_ELSE

    /* parameter escapes, termcap format */

    lvars adjust = false, adjust_if_above;

    define lconstant pusharg();
        lvars lab;
        if args == [] then error() endif;
        sysPUSH(destpair(args) -> args);
        unless ansi then sysPUSHQ(1), sysCALL("fi_-") endunless;
        if adjust then
            sysPUSHS(0), sysPUSHQ(adjust_if_above), sysCALL("fi_>");
            sysIFNOT(sysNEW_LABEL() ->> lab);
            sysPUSHQ(adjust), sysCALL("fi_+");
            sysLABEL(lab);
            false -> adjust;
        endif;
    enddefine;

    define lconstant do_escape();
        lvars c = nextc();
        if c == `i` then
            ;;; %i : ansi terminal; add 1 to arguments unless already so
            true -> ansi;
        elseif c == `r` then
            ;;; %r : arguments are reversed
            rev(args) -> args;
        elseif c == `>` then
            ;;; %>cc : add adjustment to next argument if over a given limit
            nextc() -> adjust_if_above;
            nextc() -> adjust;
        elseif c == `.` then
            ;;; %. : output next argument
            pusharg(), sysCALL(output);
        elseif c == `+` then
            ;;; %+c : output next argument offset by given character
            pusharg(), sysPUSHQ(nextc()), sysCALL("fi_+"),
                sysCALL(output);
        elseif c == `d` then
            ;;; %d : output next argument as ascii integer
            do_dlocal(), pusharg(), sysCALL("sys_syspr");
        elseif isnumbercode(c) then
            ;;; %[0-9]d : as above, but use given field width
            do_dlocal(), pusharg(), sysPUSHQ(c - `0`),
            sysPUSHQ(` `), sysPUSH("false"), sysPUSH("sys_syspr"),
                sysCALL("pr_field");
        elseif c == `%` then
            ;;; %% : output single `%`
            sysPUSHQ(`%`), sysCALL(output);
        elseif c == `n` or c == `B` or c == `D` then
            ;;; %n, %B, %D : unsupported format
            consstring(`%`, c, 2),
            mishap(1, 'UNSUPPORTED TERMCAP STRING FORMAT');
        else
            error();
        endif;
    enddefine;

#_ENDIF

    ;;; pop parameters ...
    if isword(padding) or isprocedure(padding) then
        ;;; optional output procedure given
        padding -> output -> padding;
    endif;
    -> nargs -> s;
    ;;; ... and check them
    unless isboolean(padding) then Check_integer(padding, 0) endunless;
    if nargs then Check_integer(nargs, 0) endif;
    Check_string(s);

    datalength(s) -> _len;
    if nargs then startproc() endif;
#_IF not(DEF USE_TERMINFO)
    ;;; termcap: padding always given first
    getdelay(_i, s) -> _i -> _delay -> vardelay;
#_ENDIF
    while _i fi_<= _len do
        if (nextc() ->> _c) == `%` and inproc then
            flush();
            do_escape();
#_IF DEF USE_TERMINFO
        elseif _c == `$` and _i fi_<= _len and fast_subscrs(_i, s) == `<` then
            getdelay(_i fi_+ 1, s) -> _i -> _delay -> vardelay;
            checkc(nextc(), `>`, `>`) -> ;
            do_pad();
#_ENDIF
        else
            _c, _n fi_+ 1 -> _n;
        endif;
    endwhile;
#_IF not(DEF USE_TERMINFO)
    do_pad();
#_ENDIF
    endproc();
    unless inproc then consstring(_n) endunless;
enddefine;


/*
 *  Reading the Termcap File
 */

lconstant
    termcap_entry = writeable inits(1025),
        ;;; static area for termcap entry returned by _tgetent
;

lvars
    _buffp,
        ;;; var parameter for _tgetstr

;

vars
    termcap_entry_name = false,
        ;;; name of the current termcap entry, or <false> if invalid
;

define lconstant Check_termcap_id(id) -> id;
    lvars id;
    if isword(id) then
        id!W_STRING -> id;
    else
        Check_string(id);
    endif;
enddefine;

define termcap_getnum(id);
    lvars id, _res;
    Check_termcap_id(id) -> id;
    returnunless(termcap_entry_name)(false);
    if _nonneg(_extern[SE] tgetnum(id@V_BYTES) ->> _res) then
        _pint(_res);
    else
        false;
    endif;
enddefine;

define termcap_getflag(id);
    lvars id;
    Check_termcap_id(id) -> id;
    returnunless(termcap_entry_name)(false);
    _extern tgetflag(id@V_BYTES) == _1;
enddefine;

define termcap_getstring(id);
    lvars id, _buff, _len;
    Check_termcap_id(id) -> id;
    returnunless(termcap_entry_name)(false);
    Get_string(_1024) -> _buff;
    _buff@V_BYTES -> _buffp;
    if _nonzero(_extern tgetstr(id@V_BYTES, ident _buffp)) then
        ;;; _buffp points one beyond the terminating null byte in _buff
        ##(b){_buffp, _buff@V_BYTES[_1]} -> _len;
        ;;; check it hasn't overflowed (not sure what happens if it has!)
        if _len _gr _1024 then _1024 -> _len endif;
        ;;; truncate string to _len and return unused space
        _buff -> Get_string(_len);
        _buff;
    else
        false;
    endif;
enddefine;

define termcap_getentry(name);
    lvars name, s;
    if isword(name) then
        ;;; copy the string, as it'll be left in -termcap_entry_name-
        copy(name!W_STRING) -> name;
    else
        Check_string(name);
    endif;
    returnif(name = termcap_entry_name)(true);
    ;;; the tgetent call will overwrite -termcap_entry-, so invalidate it now
    false -> termcap_entry_name;
    if _extern tgetent(termcap_entry@V_BYTES, name@V_BYTES) == _1
    then
        if (termcap_getstring('pc') ->> s) and s /= nullstring then
            fast_subscrs(1, s) -> _tty_padchar;
        else
            `\^@` -> _tty_padchar;
        endif;
        name -> termcap_entry_name;
        true;
    else
        false;
    endif;
enddefine;

define active termcap_name();
    termcap_entry_name;
enddefine;

define updaterof active termcap_name(name);
    lvars name;
    unless name then
        false -> termcap_entry_name;
    elseunless termcap_getentry(name) then
        mishap(name, 1, 'UNKNOWN TERMINAL TYPE');
    endunless;
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 26 1998
        Set -lcurses for AIX
--- Robert Duncan, Aug 14 1997
        Disabled use of termcap on NCR Unix
--- Julian Clinton, Jun 21 1996
        Modified previous fix so doesn't do it for IRIX 6 (no static version)
--- Robert John Duncan, Jul 25 1995
        Change for IRIX 5: -lcurses linked statically to prevent name
        clashes with any external load of the GL graphics library
--- John Gibson, Mar  8 1995
        Added OSF1 case in set_tty_ospeed. OSF1 Library is -lcurses not
        -ltermcap.
--- John Gibson, May 23 1993
        Added dummy exload to cause incorporation of -ltermcap in link
--- Robert John Duncan, Jun  1 1992
        Changed to read the terminal output speed only when necessary
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, Sep 14 1991
        Changed -termcap_getstring- to use updater of Get_string
--- John Gibson, Dec 20 1989
        Change for new pop pointers (addition of @~POPBASE for address
        being freed with -> Get_store in -termcap_getstring-).
--- John Gibson, Dec 12 1989
        Undid change to ident.
--- Rob Duncan, Dec  6 1989
        More secure argument checking in -termcap_compile-.
        Moved call to -N*ull_end- in -termcap_getstring- to the top of the
        procedure, to ensure there are no GCs between allocating and returning
        the string.
        Extended -termcap_name- to allow <false> as an argument, so that it
        can be dlocal'd safely even when -termcap_entry_name- isn't set
--- John Gibson, Dec  6 1989
        Replaced ident with ident in system calls (for new pop pointers).
--- Rob Duncan, Nov 15 1989
        moved everything to do with string compilation (including terminal
        delays) from "vdtermcap.p" and made it into a more general, exported
        interface: -termcap_compile-;
        changed -termcap_name- to an active variable;
        changed SYSTEM_V flag to USE_TERMIO and USE_TERMINFO as appropriate;
        put everything in section $-Sys.
 */
