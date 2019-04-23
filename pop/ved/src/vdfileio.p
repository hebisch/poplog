/* --- Copyright University of Sussex 1999.  All rights reserved. ---------
 > File:            C.all/ved/src/vdfileio.p
 > Purpose:         Read in or write VED files
 > Author:          Header inserted by Aaron Sloman, May 27 1987 (see revisions)
 > Documentation:   REF *VEDPROCS
 */

;;; ------------------------ FILE I-O --------------------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedbuffername, vedpresent, $-Sys$-Save_errno_apply)
    ;

vars
        procedure (wved_window_size, wved_destroy_window),
        vedfileprops, vednamestring, vedprocwait,
    ;

section $-Sys$-Ved;

constant
        procedure (Searchlist_name, Yank_stack, Set_variables,
        Set_residual_window, Init_vedfile)
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedreadintabs, vedreadinplain, vedreadintrailspaces,
                    vedwriteoutplain, vedversions, vedcreatemode,
                    vedinitialise,

                    vedfile_line_repeater, vedreadin, vedreadfile,
                    ved_r,
                    vedfile_line_consumer, vedwriterange,
                    vedwritefiles, ved_write_current_file,
                    ved_w1, ved_w, ved_wr
                ;


vars
    vedreadintabs           = true,
    vedreadinplain          = false,
    vedreadintrailspaces    = false,
    vedwriteoutplain        = false,
    vedversions             = false,
    vedcreatemode           = false,
    procedure vedinitialise = erase,
    ;


lconstant macro (
    ;;; character attribute flags as they occur in dstring bytes
    BCMODE_COLOURNUM    = VEDCMODE_COLOURNUM    >> 16,
    BCMODE_UNDERLINE    = VEDCMODE_UNDERLINE    >> 16,
    BCMODE_BOLD         = VEDCMODE_BOLD         >> 16,
    BCMODE_ALTFONT      = VEDCMODE_ALTFONT      >> 16,
    BCMODE_BLINK        = VEDCMODE_BLINK        >> 16,
    BCMODE_ACTIVE       = VEDCMODE_ACTIVE       >> 16,
    BCMODE_SP_SIG_BITS  = VEDCMODE_SP_SIG_BITS  >> 16,
    BCMODE_SP_INSIG_BITS= VEDCMODE_SP_INSIG_BITS>> 16,

    ;;; these must act as NUL on a printer and be contiguous codes
    ATTR_CTRL0          = `\^Q`,
    ATTR_CTRL1          = `\^R`,
    ATTR_CTRL2          = `\^S`,
    ATTR_CTRL3          = `\^T`,

    Sh                  = 16:9A,
    Nt                  = 16:9B,
    Sf                  = 16:9C,
    Ss                  = 16:9D,
    );


    ;;; Translation of 4-bit values <-> control characters for embedded
    ;;; strings.
    ;;; (N.B. The original mapping 13 <-> ^Z  is now changed to 13 <-> ^^
    ;;; because Windows treats ^Z as EOF. An input ^Z still maps to 13 for
    ;;; backward compatibility.)
lconstant
    fourbit_to_ctrl = '\^A\^B\^C\^D\^E\^F\^Q\^R\^S\^U\^V\^W\^Y\^^\^\\^]' ,

    ctrl_to_fourbit = consstring(
    ;;; ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O
        16,  0,  1,  2,  3,  4,  5, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    ;;; ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_
        16,  6,  7,  8, 16,  9, 10, 11, 16, 12, 13, 16, 14, 15, 13, 16,
    32),
;


;;; --- READING ---------------------------------------------------------------

lvars
    tab_was_found   = false,
    ;

define lconstant Embedded_in(_csrc, _clim, src16, mode16) -> (count, _csrc);
    lvars   char, lasttop11 = _shift(_:`a`,_-5), count = 0, src16, mode16,
            _csrc, _clim, _required = _5, _nbits = _0, _bits,
            _testesc = true, _curr_attr = _0;

    repeat
        while _nbits _lt _required do
            ;;; read next input char representing 4 bits
            if src16 then
                returnunless(_csrc <@(s) _clim);
                _csrc!(s)++ -> (char, _csrc)
            else
                returnunless(_csrc <@(b) _clim);
                _csrc!(b)++ -> (char, _csrc)
            endif;
            returnif(char == _:ATTR_CTRL3);     ;;; finished
            if char _lt _:`\s` then ctrl_to_fourbit!V_BYTES[char] -> char endif;
            returnunless(char _lt _16);
            _shift(_bits, _4) _add char -> _bits;
            _nbits _add _4 -> _nbits
        endwhile;

        _nbits _sub _required -> _nbits;
        _shift(_bits, _negate(_nbits)) _biclear _shift(_-1, _required) -> char;

        if _required == _5 then
            if _testesc then
                if char == _0 then
                    ;;; 9-bit character follows (bit 8=1 if attribute char)
                    _9 -> _required;
                    nextloop
                elseif mode16 and char == _28 then
                    ;;; 16-bit character follows
                    _16 -> _required;
                    nextloop
                elseif char _greq _29 then
                    ;;; 1-3 to add to bits 5,6
                    ((lasttop11 _add char _sub _28) _bimask _2:11)
                        _add (lasttop11 _biclear _2:11) -> lasttop11;
                    false -> _testesc;
                    nextloop
                endif
            endif;
            ;;; else char is bits 0-4 plus lasttop11
            _shift(lasttop11, _5) _add char -> char;
            true -> _testesc
        else
            ;;; 9-bit char in which bit 8 nonzero if attributes, or
            ;;; 16-bit char
            if _required == _9 and char _bitst _16:100 then
                _5 -> _required;
                char _biclear _16:100 -> _curr_attr;
                nextloop
            else
                ;;; 8 or 16-bit char
                _5 -> _required;
                _shift(char, _-5) -> lasttop11
            endif
        endif;

        ;;; switch `.` and `/` with `{` and `|`
        if char _sub _:`{` _lteq _1 then
            char _sub (_:`{` _sub _:`.`) -> char
        elseif char _sub _:`.` _lteq _1 then
            char _add (_:`{` _sub _:`.`) -> char
        endif;

        ;;; stack char
        _pint(_shift(_curr_attr, _16) _add char);
        count fi_+ 1 -> count
    endrepeat
enddefine;      /* Embedded_in */

define :inline lconstant TRANS_IN(  T=item, T_V_FLD=item, GET_STRING,
                                    _ATTR_PTR=item, IN_CTRL_NUM=item,
                                    SPECIAL_IN=item, IS_16, TEST_CHAR16);

    define lconstant IN_CTRL_NUM(_csrc, _clim) -> (_n, _csrc);
        lvars _csrc, _lo, _hi, _n = _:-1, _clim;
        returnunless(_csrc@(T)[_1] <@(T) _clim);
        _csrc!(T)[_0] _sub _:ATTR_CTRL0 -> _lo;
        _csrc!(T)[_1] _sub _:ATTR_CTRL0 -> _hi;
        returnunless(_lo _lteq _3 and _hi _lteq _3);
        if _hi == _3 then
            _lo _add _12
        elseif _lo == _3 then
            _hi _add _9
        else
            (_hi _add _hi _add _hi) _add _lo
        endif -> _n;
        _csrc@(T)[_2] -> _csrc
    enddefine;

    define lconstant SPECIAL_IN(_char, _csrc, _clim) -> (_char, _csrc);
        lvars _csrc, _next, _char, _clim, _r, _q;
        returnunless(_csrc <@(T) _clim);
        _csrc!(T)[_0] -> _next;
        if _char == _:ATTR_CTRL1 then
            if     _next == _:`-` then
                _:`\G-` -> _char
            elseif _next == _:`|` then
                _:`\G|` -> _char
            elseif _next == _:`+` then
                _:`\G+` -> _char
            elseif _next == _:`o` then
                _:`\Go` -> _char
            elseif _next == _:`#` then
                _:`\G#` -> _char
            elseif _next == _:`.` then
                _:`\G.` -> _char
            elseif _next == _:`\s` then
                _:`\Sp` -> _char            ;;; prompt-marker space
            elseif _:ATTR_CTRL0 _lteq _next and _next _lteq  _:ATTR_CTRL3 then
                _next -> _char
            elseif _next == _:`\^U` then
                _:Sh -> _char               ;;; hair space
            else
                return
            endif;
            _csrc@(T)++ -> _csrc

        ;;;  _char == _:ATTR_CTRL2
        elseif _next == _:`\s` and _csrc@(T)[_1] <@(T) _clim
        and ((_csrc!(T)[_1] ->> _q) == _:ATTR_CTRL0 or _q == _:ATTR_CTRL1)
        then
            ;;; Format-control space or Ved no-break space
            _:Sf _add _q _sub _:ATTR_CTRL0 -> _char;
            _csrc@(T)[_2] -> _csrc

        elseif (_next == _:`-` or _next == _:`|`) and _csrc@(T)[_2] <@(T) _clim
        then
            ;;; line-drawing set
            IN_CTRL_NUM(_csrc@(T)[_1], _clim) -> (_q, );
            returnif(_q == _:-1);

            _q _div _3 -> (_r, _q);
            if _next == _:`|` then
                if _q == _2 then _3 -> _q else _3 -> _r endif
            endif;
            _shift(_q, _2) _add _r _add _:`\Gle` -> _char;
            _csrc@(T)[_3] -> _csrc
        endif
    enddefine;


    ;;; dstring(16) big enough for worst possible case (truncated at end)
    GET_STRING(_len _mult _tabval) -> string;

    string@T_V_FLD -> _cdst;
    _ATTR_PTR(string, _0) -> _abdst;
    _cdst--@(T) -> _furthest;
    buf@T_V_FLD[_int(_start)] -> _csrc;
    _csrc@(T)[_len] -> _clim;

    while _csrc <@(T) _clim do
        _csrc!(T)++ -> (char, _csrc);

        if char == _:ATTR_CTRL2 and not(_nonved) and _cdst >@(T) _furthest
        and (IN_CTRL_NUM(_csrc, _clim) -> (_ctrl, _csrc), _ctrl /== _-1)
        then
            ;;; colour/active change
            if (_ctrl ->> _colour) _bitst _8 then
                (_colour _biclear _8) _biset _:BCMODE_ACTIVE -> _colour
            endif

        elseif char == _:ATTR_CTRL3 and not(_nonved) and _cdst >@(T) _furthest
        and _csrc <@(T) _clim
        and (_csrc!(T) ->> _ctrl ->> _mode16) _lteq _:ATTR_CTRL3
        and _ctrl _greq _:ATTR_CTRL0
        and (_ctrl /== _:ATTR_CTRL3 or
            _csrc@(T)[_1] <@(T) _clim
            and (_csrc!(T)[_1] ->> _ctrl) _lteq _:ATTR_CTRL2
            and _ctrl _greq _:ATTR_CTRL0)
        then
            ;;; embedded string/word for next char -- stack subscript in main
            ;;; string, embedded string chars and the number of them
            _csrc@(T)++ -> _csrc;
            if _mode16 == _:ATTR_CTRL3 then _csrc@(T)++ -> _csrc endif;
            _pint( ##(T){_cdst, string@T_V_FLD} _add _1 );      ;;; index
            if _ctrl == _:ATTR_CTRL0 then
                ;;; special for nullstring
                0
            else
                Embedded_in(_csrc, _clim, IS_16, _mode16 == _:ATTR_CTRL3)
                                                            -> _csrc;
                if _ctrl == _:ATTR_CTRL2 then fi_~~ () endif    ;;; encodes word
            endif;
            _embed_count _add _2 -> _embed_count

        elseif char == _:`\b` and _nonved /== true then
            ;;; interpret backspace literally
            nextif(_cdst == string@T_V_FLD);
            _cdst--@(T) -> _cdst;
            _abdst--@(b) -> _abdst;
            if _count == _tabval then _1 else _count _add _1 endif -> _count

        else
            ;;; copy char across
            _0 -> _ctrl;
            unless _nonved then
                while char == _:ATTR_CTRL0 and _csrc <@(T) _clim do
                    _ctrl _add _1 -> _ctrl;
                    _csrc!(T)++ -> (char, _csrc)
                endwhile;
                if char == _:ATTR_CTRL1 or char == _:ATTR_CTRL2 then
                    SPECIAL_IN(char, _csrc, _clim) -> (char, _csrc)
                endif
            endunless;

            _0 -> _attr;
            if _cdst <=@(T) _furthest then
                ;;; already assigned to once
                _cdst!(T) -> _oldchar;
                if _oldchar == _:`_` then
                    ;;; set underline/altfont
                    _ulmode _add _ctrl -> _ulmode;
                    if _ulmode _sgr _2 then _ulmode _sub _3 -> _ulmode endif;
                    if _ulmode == _0 then
                        _ul_attr
                    elseif _ulmode == _1 then
                        _:BCMODE_ALTFONT
                    else
                        _:BCMODE_UNDERLINE _biset _:BCMODE_ALTFONT
                    endif -> _attr

                elseif char == _oldchar then
                    ;;; set bold? (this is not put out by Trans_line_out)
                    _:BCMODE_BOLD -> _attr
                endif;
                _abdst!(b) _biset _attr -> _attr

            else
                if _ctrl _bitst _1 then
                    _bbmode _bixor _:BCMODE_BOLD -> _bbmode
                endif;
                if _ctrl _bitst _2 then
                    _bbmode _bixor _:BCMODE_BLINK -> _bbmode
                endif;
                ;;; new position
                _cdst -> _furthest
            endif;

            _colour _biset _bbmode _biset _attr -> _attr;
            if char == _:`\t` or char == _:`\s` then
                _attr _biclear _:BCMODE_SP_INSIG_BITS -> _attr;
                if char == _:`\t` then
                    true -> tab_was_found;  ;;; used by vedreadfile
                    _padchar
                else
                    char
                endif
            else
                TEST_CHAR16;
                char
            endif -> _cdst!(T)++ -> _cdst;
            _attr -> _abdst!(b)++ -> _abdst;

            if _zero(_count _sub _1 ->> _count) then
                ;;; reached next tab stop
                _tabval -> _count
            elseif char == _:`\t` then
                ;;; 'reuse' the tab upto the next tab stop
                _csrc--@(T) -> _csrc
            endif
        endif
    endwhile;

    ##(T){_furthest@(T)++, string@T_V_FLD} -> _len;

    ;;; deal with trailing spaces
    while _nonzero(_len) and _furthest!(T) == _:`\s`
    and _zero(_ATTR_PTR(string, _len _sub _1)!(b)) do
        if vedreadintrailspaces then
            ;;; turn last into a trailing space
            _:`\St` -> _furthest!(T);
            quitloop
        else
            ;;; strip it
            _furthest--@(T) -> _furthest;
            _len _sub _1 -> _len
        endif
    endwhile;
enddefine;      /* TRANS_IN */


    /*  Move _len bytes read into buf across to new string, converting tabs,
        interpreting backspace, and interpreting 'overprinting' of chars
        as bold or underline etc.
    */
define lconstant Trans_line_in(_start, _len, buf, _tabval, _nonved) -> string;
    lvars   char, string, buf, embstr, _csrc, _cdst, _abdst, _clim,
            _furthest, _ulmode, _bbmode, _colour, _ctrl, _padchar, _attr,
            _tabval = _int(_tabval), _count, _oldchar, _len, _start,
            _nonved, _ul_attr, _notabs, _embed_count, _all8, _mode16;

    _checkall();
    returnif(_zero(_len)) (nullstring -> string);

    ;;; ensure enough stack space for embedded string chars
    Alloc_user_space(@@(w)[_len]);

    _tabval _bitst _16:100 -> _notabs;
    _tabval _biclear _16:100 ->> _tabval -> _count;
    if _notabs then _:`\s` else _:`\t` endif -> _padchar;
    _0 ->> _ulmode ->> _bbmode -> _colour;

    if isinteger(_nonved) then
        ;;; not a Ved file, but interpret `backspace' underlining as the
        ;;; given attributes (e.g. ved_man)
        _shift(_int(_nonved), _-16)
    else
        _:BCMODE_UNDERLINE
    endif -> _ul_attr;

    _0 -> _embed_count;
    true -> _all8;

    if buf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        TRANS_IN(s, V_SHORTS, Get_dstring16, nonmac _DSTRING16_ATTR_PTR,
                    in_ctrl_num16, special_in16, true,
                    if char _gr _:16:FF then false -> _all8 endif)
    else
        TRANS_IN(b, V_BYTES, Get_dstring, nonmac _DSTRING_ATTR_PTR,
                    in_ctrl_num8, special_in8, false, )
    endif;

    ;;; truncates dstring(16) (including truncating string16 to string if
    ;;; _all8 is true, and truncating from dstring to string if attributes
    ;;; are all zero), and return any free space.
    string -> Get_dstring(_len, _all8);

    returnif(_zero(_embed_count));

    initv(_pint(_embed_count)) -> buf;
    repeat
        _embed_count _sub _2 -> _embed_count;
        () -> _count;
        if _count fi_> 0 then
            consdstring(_count)
        elseif _count == 0 then
            nullstring
        else
            consword(fi_~~ _count)
        endif -> buf!V_WORDS[_embed_count _add _1];
        () -> buf!V_WORDS[_embed_count];        ;;; index
        quitif(_zero(_embed_count))
    endrepeat;
    buf -> vedstring_data_prop(string)
enddefine;      /* Trans_line_in */




    ;;; arguments to Readin
lconstant macro (
    STATE   = [fast_frozval(3,self)],
    START   = [fast_frozval(4,self)],
    FINISH  = [fast_frozval(5,self)],
    OVERFLOW= [fast_frozval(6,self)],
    SAVEDLINE= [fast_frozval(7,self)],
);


lconstant
    BUFFER_SIZE = 4096,         ;;; maximum read size
    FUDGE_FACTOR = 3584,        ;;; minimum we want to sysread
    ;

define lconstant Readin(dev, self, state, start, finish, overflow, savedline,
                                        buf, _tabval, _nonved);
    lvars
        dev,            ;;; device
        procedure self, ;;; closure of me
        state,          ;;; current state I'm in
        start,          ;;; position in the buffer
        finish,         ;;; top of active segment of buffer
        buf,            ;;; string buffer for read
        overflow,       ;;; cache for long lines
        savedline,      ;;; saved line following a blank line

        _tabval,        ;;; tabval || 16:100 if notabs
        _nonved,        ;;; true value if non-Ved file
        _n,             ;;; location of next newline in buffer
        _count,         ;;; bytes read by sysread
        ;

    define lconstant Find_nl(_buf, _s, _nc);
        lvars _s = _int(_s), _nc = _int(_nc), _buf;
        if _buf!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _buf@V_SHORTS[_s] ->> _s -> _buf;
            _s@(s)[_nc] -> _nc;
            while _s <@(s) _nc do
                returnif((_s!(s)++ -> _s) == _:`\n`) (##(s){_s, _buf@(s)++})
            endwhile;
            _-1
        else
            ##(b){ _locc(_buf@V_BYTES[_s], @@(b)[_nc], _:`\n`) }
        endif
    enddefine;

    if isstring(savedline) then
        savedline;
        false ->> savedline -> SAVEDLINE;

    RETURNLINE:
        if vedreadintrailspaces and not(savedline) and dup() = nullstring then
            -> ;
            true -> SAVEDLINE;      ;;; prevent self() from doing same test
            if (self() ->> savedline) == termin then
                consstring(Nt, 1)       ;;; trailing blank line at eof
            else
                nullstring;
                savedline -> SAVEDLINE  ;;; return this next time
            endif
        endif;
        return()
    endif;

    go_on state to READMORE SOMEREAD FINISHED else TERMIN;

    READMORE:
        fast_sysread(dev, start fi_+ 1, buf, BUFFER_SIZE fi_- start) -> _count;
        if _count fi_<= 0 then goto LASTLINE endif;

        ;;; got a string, extract lines
        start fi_+ _count ->> finish -> FINISH;
        if (Find_nl(buf, start, _count) ->> _n) == _-1 then
            ;;; no newline read
            if finish fi_< BUFFER_SIZE then
                ;;; buffer not filled -- just try and read some more
                finish -> start;
                goto READMORE
            else
                0 -> start;
                goto INCOMPLETE
            endif
        endif;
        ;;; found at least one newline
        _int(start) _add _n -> _n;
        if overflow then
            overflow <> substring(1, _pint(_n), buf) -> overflow;
            Trans_line_in(_0, overflow!V_LENGTH, overflow, _tabval, _nonved);
            false -> overflow
        else
            ;;; drop the first line - note the start of 0 here and above
            Trans_line_in(_0, _n, buf, _tabval, _nonved)
        endif;
        _pint(_n _add _1) -> START;
        overflow -> OVERFLOW;
        2 -> STATE;     ;;; SOMEREAD
        goto RETURNLINE;

    SOMEREAD:
        ;;; deal with second and subsequent newlines
        finish fi_- start  -> _count;
        unless _count fi_<= 0 or (Find_nl(buf, start, _count) ->> _n) == _-1
        then
            ;;; found a newline in range
            Trans_line_in(start, _n, buf, _tabval, _nonved);
            start fi_+ _pint(_n _add _1) -> START;
            goto RETURNLINE
        endunless;

    INCOMPLETE:
        ;;; deal with fragment of incomplete line
        finish fi_- start -> _count;
        start fi_+ 1 -> _n;
        0 -> start;
        if _count fi_> 0 then
            ;;; some remaining buffer to be accounted for
            if _n fi_>= FUDGE_FACTOR then
                ;;; shift remainder to start of buffer
                move_subvector(_n, buf, 1, buf, _count);
                _count -> start
            elseif overflow then
                ;;; add to already cached overflow
                overflow <> substring(_n, _count, buf) -> overflow
            else
                ;;; set up overflow string from scratch
                substring(_n, _count, buf) -> overflow
            endif
        endif;
        goto READMORE;

    LASTLINE:
        ;;; we may have a non-newline-terminated last line
        unless start == 0 then
            ;;; we have some unused bytes at the start of the buffer
            if overflow then
                ;;; append them to string cache
                overflow <> substring(1, start, buf)
            else
                ;;; no pre-existing cache
                substring(1, start, buf)
            endif -> overflow
        endunless;

        ;;; any overflow - flush it
        if overflow then
            Trans_line_in(0, overflow!V_LENGTH, overflow, _tabval, _nonved);
            vedputmessage('\{b}incomplete last line - newline added');
            vedscreenbell();
            syssleep(100);
            3 -> STATE;     ;;; FINISHED
            return
        endif;

    FINISHED:
        sysclose(dev);
        -1 -> STATE;    ;;; TERMIN

    TERMIN:
        termin
enddefine;      /* Readin */

define vedfile_line_repeater(file) -> rep;
    lvars file, dev, rep, _nonved = vedreadinplain, _tabval;
    if isinteger(file) or isboolean(file) then
        ;;; optional arg for _nonved -- may be just true to say no
        ;;; interpretation of control chars, or may be an integer to
        ;;; specify attributes to be used for `backspace' underlining
        ;;; in non-Ved files
        ((), file) -> (file, _nonved)
    endif;

    if isdevice(file) then
        file
    else
        sysopen(file, 0, false, `N`)
    endif -> dev;
    if systrmdev(dev) then
        mishap(device_open_name(dev), 1,
                    'vedfile_line_repeater: INVALID DEVICE TYPE')
    endif;

    vedindentstep fi_&& 16:FF -> _tabval;
    if not(vedreadintabs) and vednotabs then
        _tabval fi_|| 16:100 -> _tabval     ;;; no tabs flag
    endif;
    Readin(% dev, false, 1, 0, 0, false, false,
                device_init_buffer(dev,BUFFER_SIZE), _tabval, _nonved %)
                        -> rep;
    rep -> fast_frozval(2,rep);     ;;; so it can update its vars
enddefine;

define vedreadin() with_nargs 1;
    lvars sl, line, procedure rep = vedfile_line_repeater();

    ;;; allow big files to be read without running out of memory
    dlocal popmemlim = false;

    stacklength() -> sl;

    define dlocal interrupt();
        erasenum(stacklength() - sl);
        sysclose(frozval(1,rep));
        ;;; return false to indicate interrupt
        false;
        exitfrom(vedreadin)
    enddefine;

    until (rep() ->> line) == termin do
        _checkall();
        line
    enduntil;

    /* number of lines left on stack */
    stacklength() - sl
enddefine;

lconstant nfn_mess = '\{b}no filename specified';

define vars ved_r();
    lvars file, list, n, dev;
    dlocal vedargument;

    [% sys_parse_string(vedargument) %] -> list;
    if list == [] then
        vederror(nfn_mess)
    elseif back(list) == [] then
        nullstring, front(list)
    else
        front(list);     ;;; line number
        front(back(list))
    endif -> (vedargument, file);

    Searchlist_name(sysfileok(file)) -> file;
    ;;; might return a list ...
    if islist(file) then hd(file) -> file endif;

    if readable(file) ->> dev then
        vedputmessage('\{b}reading ' <> file);
        if vedreadin(dev) ->> n then
            vedputmessage(file <> (', ' sys_>< n) <> ' \{b}lines read');
            Yank_stack(n, false, false)
        elseunless vedprintingdone then
            vederror('\{b}interrupted')
        endif
    else
        vederror('\{b}cannot read ' <> file)
    endif
enddefine;


    /*  May take an optional boolean 3rd arg to say whether file is being
        put on screen. If this false for xved we don't create a window.
        name could be a list, containing [string vedfileprops subsystem]
        or consref({name, getbuffer_p})
    */
define vedreadfile(name, defaults_p) -> (new, newfile);

    lvars   newfile, dev = false, new, name, defaults_p, defaults, vec,
            getbuffer_p = false, setting_on_screen = true, w, n;
    dlocal  vedupperfile, vedlowerfile, ved_current_file;

    if isboolean(defaults_p) then
        ((), name, defaults_p) -> (name, defaults_p, setting_on_screen)
    endif;
    if isprocedure(name) then
        ;;; args in old order
        name, defaults_p -> (defaults_p, name)
    elseunless isprocedure(defaults_p) then
        mishap(defaults_p, 1, 'NON-PROCEDURE GIVEN FOR SETTING VED DEFAULTS')
    endif;

    if isref(name) then
        fast_cont(name) -> vec;
        subscrv(1,vec) -> name;
        subscrv(2,vec) -> getbuffer_p;
    endif;
    if islist(name) then dest(name) else name, [] endif -> (name, defaults);
    if isdevice(name) then
        name -> dev;
        device_open_name(dev) -> name
    endif;
    if isword(name) then copy(name!W_STRING) -> name endif;

    ;;; initialise file-structure environment with default values
    Init_vedfile() -> newfile;

    ;;; set a residual window for messages in vedupper/lowerfile while
    ;;; the file is being created (must do this before making the new
    ;;; file current).
    Set_residual_window();


    define lconstant kill_windows();
        if USEWINDOWS and wvedfreewindow then
            wved_destroy_window(wvedfreewindow);
            if wvedwindow == wvedfreewindow then false -> wvedwindow endif;
            false -> wvedfreewindow
        endif;
        nullstring -> vedmessage
    enddefine;

    define dlocal interrupt();
        clearstack();
        kill_windows();
        ;;; call external interrupt
        chainfrom(vedreadfile, procedure; interrupt() endprocedure);
    enddefine;

    define lconstant do_error(count, mess, idstring) with_props vedreadfile;
        lvars count, mess, idstring, newmess;
        nullstring ->> vedvedname -> vedargument;
        if ispair(vedupperfile) then false -> vedupperfile endif;
        if ispair(vedlowerfile) then false -> vedlowerfile endif;
        1000 ->> vedscreenline -> vedscreencolumn;
        0 -> vedscreencharmode;
        if vedinvedprocess and (ved_current_file or vedbufferlist /== [])
        and (Errmess_sprintf(count, mess, idstring, 1) ->> newmess)
        then
            unless ved_current_file then
                Setonscreen_next(false, false)
            endunless;
            vederror(newmess)
        else
            mishap(count, mess, idstring)
        endif
    enddefine;

    define dlocal pop_exception_final(count, mess, idstring, sev);
        lvars count, mess, idstring, sev;
        returnif(sev == `I` or sev == `W`) (false);
        kill_windows();
        chainfrom(count, mess, idstring, vedreadfile, do_error)
    enddefine;

    define dlocal vederror(string);
        lvars string;
        pop_exception_final(0, string, nullstring, `E`)
    enddefine;


    ;;; make it current and set user defaults, etc
    newfile -> ved_current_file;
    name ->> vedcurrent -> vedpathname;
    Set_variables(false);

    ;;; used if name is a list
    ;;; (should be generalised to allow sublists of form [<var> <value>])
    unless null(defaults) then
        dest(defaults) -> (vedfileprops, defaults);
        unless null(defaults) then
            dest(defaults) -> (w, defaults);
            if w == """ then
                hd(defaults) -> subsystem
            else
                if isword(w) then valof(w) -> w endif;
                w -> if isword(w) then
                        subsystem
                     else
                        ;;; for backward compatability
                        valof("popcompiler")
                     endif
            endif
        endunless;
    endunless;

    ;;; this will set the environment for the new file
    defaults_p();
    Set_variables(true);

    unless dev or getbuffer_p or not(veddirectory) then
        sysopen(vedcurrent, 0, false, `F`) -> dev
    endunless;

    if dev or getbuffer_p then
        ;;; allow big files to be read without running out of memory
        dlocal popmemlim = false;

        if getbuffer_p then
            ;;; call user procedure with setting_on_screen arg -- returns
            ;;; vector for vedbuffer
            getbuffer_p(setting_on_screen)

        else
            ;;; It's not clear that the reading message is particularly useful
            ;;; these days -- it shouldn't be put out at all except when
            ;;; the user is explicitly ved'ing a file. For XVed we make it
            ;;; behave like this by testing for iscaller(veddocommand).

            if setting_on_screen and vedediting
            and (not(XWINDOWS) or iscaller(veddocommand)) then
                lvars ns = vednamestring;
                vedputmessage('\{b}reading ' <> allbutfirst(locchar(`\s`,1,ns), ns))
            endif;

            0 -> vedscreencharmode;     ;;; in case of error
            false -> tab_was_found;     ;;; may be set true by Trans_line_in
            vedreadin(dev) -> n;        ;;; strings and nstrings left on stack
            if n then
                if vednotabs and vedreadintabs and tab_was_found then
                    ;;; if tabs in file then make vednotabs false, to preserve them
                    false -> vednotabs
                endif;
                consvector((), repeat 20 times nullstring endrepeat, n fi_+ 20)
            else
                ;;; read not successful
                interrupt()
            endif
        endif -> vedbuffer;
        1 ->> vedline -> vedcolumn;
        false

    else
        ;;; new file
        true
    endif -> new;

    vedusedsize(vedbuffer) -> vvedbuffersize;
    vedsetlinesize();
    nullstring -> vedmessage;
    vedinitialise(new)
enddefine;      /* vedreadfile */


;;; --- WRITING -----------------------------------------------------------


define lconstant Embedded_out(string, _cdst, dst16) -> _cdst;
    lvars   char, top11, lasttop11 = _shift(_:`a`,_-5), string, dst16, mode16,
            _csrc, _cdst, _absrc = _NULL, _clim, _bits, _nbits = _0, _nout,
            _code, _diff, _attr, _curr_attr = _0, _esclo,
            _kflags = string!KEY!K_FLAGS;

    _kflags _bitst _:M_K_STRING16 -> mode16;
    if mode16 then
        string@V_SHORTS -> _csrc;
        _csrc@(s)[string!V_LENGTH] -> _clim;
        if _kflags _bitst _:M_K_DSTRING then
            _DSTRING16_ATTR_PTR(string, _0) -> _absrc
        endif;
        _28 -> _esclo
    else
        string@V_BYTES -> _csrc;
        _csrc@(b)[string!V_LENGTH] -> _clim;
        if _kflags _bitst _:M_K_DSTRING then
            _DSTRING_ATTR_PTR(string, _0) -> _absrc
        endif;
        _29 -> _esclo
    endif;

    until _csrc == _clim do
        if _absrc /== _NULL
        and (_absrc!(b)++ -> (_attr, _absrc), _attr /== _curr_attr)
        then
            ;;; change attributes -- 5-bit 0 marker followed by 9-bit char
            _16:100 _add _attr -> _code;    ;;; 0 escape in top 5 bits
            _14 -> _nout;
            _attr -> _curr_attr;
            _absrc--@(b) -> _absrc      ;;; make same next time around

        else
            if mode16 then
                _csrc!(s)++ -> (char, _csrc)
            else
                _csrc!(b)++ -> (char, _csrc)
            endif;

            ;;; switch `.` and `/` with `{` and `|` (puts `.` and `/` in the
            ;;; same block as lowercase alpha).
            if char _sub _:`{` _lteq _1 then
                char _sub (_:`{` _sub _:`.`) -> char
            elseif char _sub _:`.` _lteq _1 then
                char _add (_:`{` _sub _:`.`) -> char
            endif;

            _0 -> _nout;
            char _bimask _2:11111 -> _code;
            _shift(char, _-5) -> top11;
            top11 _bixor lasttop11 -> _diff;
            if _zero(_diff) then
                ;;; differs from last only in bottom 5 bits -- output new 5
                ;;; bits unless these are an escape value
                if _nonzero(_code) and _code _lt _esclo then _5 -> _nout endif
            elseif _diff _lteq _2:11 then
                ;;; differs in bits 5 & 6
                _shift( ((top11 _sub lasttop11) _bimask _2:11) _add _28, _5)
                                _add _code -> _code;
                _10 -> _nout
            endif;
            if _zero(_nout) then
                if char _gr _16:FF then
                    _shift(_28, _16) _add char -> _code;
                    _21 -> _nout
                else
                    char -> _code;      ;;; 0 escape in top 5 bits
                    _14 -> _nout
                endif
            endif;
            top11 -> lasttop11
        endif;

        _shift(_bits, _nout) _add _code -> _bits;
        _nbits _add _nout -> _nbits;

        if _csrc == _clim and _nbits _bitst _3 then
            ;;; padding bits at end
            _4 _sub (_nbits _bimask _3) -> _nout;
            _shift(_bits, _nout) -> _bits;
            _nbits _add _nout -> _nbits
        endif;

        while _nbits _greq _4 do
            ;;; output next nibble as control char
            _nbits _sub _4 -> _nbits;
            _shift(_bits, _negate(_nbits)) _bimask _16:F -> _nout;
            fourbit_to_ctrl!V_BYTES[_nout] -> _nout;
            if dst16 then
                _nout -> _cdst!(s)++ -> _cdst
            else
                _nout -> _cdst!(b)++ -> _cdst
            endif
        endwhile
    enduntil
enddefine;      /* Embedded_out */


define :inline lconstant TRANS_OUT( T=item, T_V_FLD=item, BUFP, INITS,
                                    _ATTR_PTR=item, OUT_CTRL_NUM=item,
                                    SPECIAL_OUT=item, SEQ_LIM=item,
                                    CMOVE, CFILL, IS_16);

    define lconstant OUT_CTRL_NUM(_n, _cdst) -> _cdst;
        lvars _cdst, _n, _lo, _hi;
        if _n _lt _9 then
            _n _div _3
        elseif _n _lt _12 then
            _3, _n _sub _9
        else
            _n _sub _12, _3
        endif -> (_lo, _hi);
        _lo _add _:ATTR_CTRL0 -> _cdst!(T)++ -> _cdst;
        _hi _add _:ATTR_CTRL0 -> _cdst!(T)++ -> _cdst
    enddefine;

    define lconstant SPECIAL_OUT(_char, _cdst) -> _cdst;
        lvars _cdst, _char, _b, _t, _ctrl = _:ATTR_CTRL1;
        if _plain _greq _2 then
            ;;; no translation of graphics chars/prompt space
            _char -> _cdst!(T)++ -> _cdst;
            return
        endif;

        if     _char == _:`\G-` then
            _:`-` -> _char
        elseif _char == _:`\G|` then
            _:`|` -> _char
        elseif _char == _:`\G+` then
            _:`+` -> _char
        elseif _char == _:`\Go` then
            _:`o` -> _char
        elseif _char == _:`\G#` then
            _:`#` -> _char
        elseif _char == _:`\G.` then
            _:`.` -> _char
        elseif _:`\Gle` _lteq _char and _char _lteq _:`\Grt` then
            ;;; in line-drawing set
            _char _sub _:`\Gle` -> _char;
            _char _bimask _:2:11 -> _b;
            _shift(_char, _:-2) -> _t;
            if _b == _3 or _t == _3 then
                if _t == _3 then _2 -> _t else _2 -> _b endif;
                _:`|`
            else
                _:`-`
            endif -> _char;
            if _zero(_plain) then
                _:ATTR_CTRL2 -> _cdst!(T)++ -> _cdst;
                _char -> _cdst!(T)++ -> _cdst;
                OUT_CTRL_NUM(_t _mult _3 _add _b, _cdst) -> _cdst;
                return
            endif
        elseif _char == _:Sf or _char == _:Ss then
            ;;; Format-control space or Ved no-break space
            _char _sub _:Sf _add _:ATTR_CTRL0 -> _b;
            _:`\s` -> _char;
            if _zero(_plain) then
                _:ATTR_CTRL2 -> _cdst!(T)++ -> _cdst;
                _char -> _cdst!(T)++ -> _cdst;
                _b -> _cdst!(T)++ -> _cdst;
                return
            endif
        elseif _char == _:`\Sp` then
            ;;; prompt-marker space -- vvedpromptchar
            _:`\s` -> _char
        elseif _char == _:Sh then
            ;;; Ved hair space
            _:`\^U` -> _char
        elseif _char == _:`_` then
            ;;; special for embedded strings
            _:ATTR_CTRL2 -> _ctrl
        elseunless _:ATTR_CTRL0 _lteq _char and _char _lteq _:ATTR_CTRL3 then
            _char -> _cdst!(T)++ -> _cdst;
            return
        endif;

        if _zero(_plain) then _ctrl -> _cdst!(T)++ -> _cdst endif;
        _char -> _cdst!(T)++ -> _cdst
    enddefine;


    BUFP(bufpair) -> buf;
    unless buf and buf!V_LENGTH _greq _buflen then
        INITS(_pint(_buflen)) ->> buf -> BUFP(bufpair)
    endunless;
    buf@T_V_FLD -> _cdst;

    string@T_V_FLD -> _csrc;
    _csrc@(T)[string!V_LENGTH] -> _clim;
    if _kflags _bitst _:M_K_DSTRING and _zero(_plain) then
        _ATTR_PTR(string, _0)
    else
        _0 -> _attr;
        _NULL
    endif -> _absrc;

    ;;; erase trailing spaces in src (leave tabs alone)
    while _clim >@(T) _csrc do
        _clim!(T)[_:-1] -> char;
        if _plain == _1
        and (char == _:Sf or char == _:Ss or char == _:Sh or char == _:`\Sp`)
        then
            _:`\s` -> char
        endif;
        quitif(char /== _:`\s`
                or (_absrc /== _NULL and _absrc!(b)[##(T){_clim--@(T), _csrc}]
                                        _bitst _:BCMODE_SP_SIG_BITS));
        _clim--@(T) -> _clim
    endwhile;

    dvec@V_WORDS -> _dvecptr;
    if dvec then _dvecptr@(w)[dvec!V_LENGTH] else _dvecptr endif -> _dveclim;
    if _dvecptr <@(w) _dveclim then
        string@T_V_FLD[_int(_dvecptr!(w)++ -> _dvecptr) _sub _1]
    else
        _clim
    endif -> _embpos;
    _tabval -> count;

    while _csrc <@(T) _clim do
        if _csrc == _embpos then
            ;;; next character has embedded string/word -- insert it
            _:ATTR_CTRL3 -> _cdst!(T)++ -> _cdst;       ;;; start marker
            _dvecptr!(w)++ -> (item, _dvecptr);
            if isword(item) then
                _:ATTR_CTRL2 -> char;
                item!W_STRING -> item
            elseif _zero(item!V_LENGTH) then
                ;;; special for nullstring
                _:ATTR_CTRL0 -> char;
                false -> item
            else
                _:ATTR_CTRL1 -> char
            endif;
            if item!KEY!K_FLAGS _bitst _:M_K_STRING16 then
                ;;; marks string/word-16 mode
                _:ATTR_CTRL3 -> _cdst!(T)++ -> _cdst
            endif;
            char -> _cdst!(T)++ -> _cdst;
            if item then
                Embedded_out(item, _cdst, IS_16) -> _cdst;
                _:ATTR_CTRL3 -> _cdst!(T)++ -> _cdst        ;;; end marker
            endif;
            ;;; set next embedded position
            if _dvecptr <@(w) _dveclim then
                string@T_V_FLD[_int(_dvecptr!(w)++ -> _dvecptr) _sub _1]
            else
                _clim
            endif -> _embpos
        endif;

        _csrc!(T)++ -> (char, _csrc);
        if _absrc /== _NULL then _absrc!(b)++ -> (_attr, _absrc) endif;

        _1 ->> _insz -> _sz;
        false -> _special;

        if char == _:`\t` or char == _:`\s` or char == _:`\Sn`
        or char == _:`\Ss` or char == _:Sh
        or (char == _:`\St` and _plain _lt _3)
        then
            if _attr _bitst _:BCMODE_SP_SIG_BITS then
                _attr _biclear _:BCMODE_SP_INSIG_BITS
            else
                _0
            endif -> _attr;
            if _curr_bbmode _bitst _:BCMODE_BOLD then
                _attr _biset _:BCMODE_BOLD -> _attr
            endif;
            if char == _:`\t` then
                if _zero(_tabval) then
                    _:`\s` -> char
                else
                    ;;; check tab occupies correct number of cols -- if not,
                    ;;; it's corrupted, so replace it with spaces
                    count -> _n;
                    until count == _1 do
                        _csrc!(T)++ -> (char, _csrc);
                        if char /== _:`\t` then
                            ;;; corrupted
                            _csrc--@(T) -> _csrc;
                            repeat
                                _:`\s` -> _csrc--!(T) -> _csrc;
                                quitif(count == _n);
                                count _add _1 -> count
                            endrepeat;
                            if _absrc /== _NULL then _absrc--@(b) -> _absrc endif;
                            nextloop(2)     ;;; restart outer loop
                        endif;
                        count _sub _1 -> count
                    enduntil;
                    _n -> _sz;
                    if _absrc /== _NULL then _absrc--@(b)[_sz] -> _absrc endif
                endif
            elseif char == _:`\Ss` or char == _:Sh then
                ;;; Ved no-break/hair space
                true -> _special
            elseif char == _:`\St` then
                ;;; trailing space
                _:`\s` -> char
            endif
        elseif (_:`\s` _lt char and char _lt _:16:80) or char _gr _:16:A0 then
            ;;; ordinary printing char
            _0 -> _insz
        elseif char == _:Nt and _plain _lt _3 then
            ;;; trailing newline -- just ignore
            nextloop
        else
            true -> _special
        endif;

        if _zero(_plain) then
            ;;; switch colour/active if necessary
            _attr _bimask _:BCMODE_COLOURNUM -> _n;
            if _attr _bitst _:BCMODE_ACTIVE then _n _add _8 -> _n endif;
            if _n /== _curr_colour then
                ;;; change colour/active
                _:ATTR_CTRL2 -> _cdst!(T)++ -> _cdst;
                OUT_CTRL_NUM(_n, _cdst) -> _cdst;
                _n -> _curr_colour
            endif;

            ;;; switch bold/blink mode if necessary
            _attr _bimask (_:BCMODE_BOLD _biset  _:BCMODE_BLINK) -> _newbb;
            if _newbb /== _curr_bbmode then
                _newbb _bixor _curr_bbmode -> _diff;
                _0 -> _n;
                if _diff _bitst _:BCMODE_BOLD then _n _add _1 -> _n endif;
                if _diff _bitst _:BCMODE_BLINK then _n _add _2 -> _n endif;
                repeat
                    _:ATTR_CTRL0 -> _cdst!(T)++ -> _cdst;
                    quitif(_zero(_n _sub _1 ->> _n))
                endrepeat;
                _newbb -> _curr_bbmode
            endif
        endif;

        if _attr _bitst (_:BCMODE_UNDERLINE _biset _:BCMODE_ALTFONT)
        and _zero(_plain) then
            ;;; deal with other underline/altfont -- represented by
            ;;; underlining with backspaces

            define lconstant SEQ_LIM(_csrc, _clim, _absrc, _attr) -> _csrc;
                lvars _csrc, _absrc, _attr, _c, _a, _clim;
                while _csrc <@(T) _clim do
                    _csrc!(T)++ -> (_c, _csrc);
                    _absrc!(b)++ -> (_a, _absrc);
                    unless _a == _attr
                    and ((_:`\s` _lt _c and _c _lt _:16:80) or _c _gr _:16:A0)
                    then
                        return(_csrc--@(T) -> _csrc)
                    endunless
                endwhile
            enddefine;

            if _zero(_insz) then
                ;;; find seq of nonspace chars with same attributes
                ;;; (prevents names you might want to grep for being split up)
                ##(T){SEQ_LIM(_csrc, _embpos, _absrc, _attr), _csrc--@(T)}
                                                        ->> _sz -> _insz
            endif;
            _csrc--@(T) -> _csrc;
            _absrc--@(b) -> _absrc;

            CFILL(_:`_`, @@(T)[_sz], _cdst), _cdst@(T)[_sz] -> _cdst;
            CFILL(_:`\b`, @@(T)[_sz], _cdst), _cdst@(T)[_sz] -> _cdst;
            if _attr _bitst _:BCMODE_ALTFONT then
                if _attr _bitst _:BCMODE_UNDERLINE then _2 else _1 endif
            else
                _0
            endif -> _n;
            _n, _n _sub _curr_ulmode -> (_curr_ulmode, _n);
            if _n _slt _0 then _n _add _3 -> _n endif;
            until _zero(_n) do
                _:ATTR_CTRL0 -> _cdst!(T)++ -> _cdst;
                _n _sub _1 -> _n
            enduntil;

            ;;; main chars
            if char == _:`\s` then
                char -> _cdst!(T)++ ;;; original may be a tab, etc
            elseif _special then
                SPECIAL_OUT(char, _cdst)
            else
                CMOVE(@@(T)[_insz], _csrc, _cdst)
            endif -> _cdst;

            _csrc@(T)[_insz] -> _csrc;
            _absrc@(b)[_insz] -> _absrc;

            unless _zero(_tabval) then
                (count _sub _insz) _div _tabval -> (count, )
            endunless

        else
            ;;; else just copy next char across
            if _special then
                SPECIAL_OUT(char, _cdst) -> _cdst
            else
                char -> _cdst!(T)++ -> _cdst
            endif;
            count _sub _1 -> count
        endif;
        if count _slteq _0 then count _add _tabval -> count endif
    endwhile;

    ;;; add newline at end and output the line
    _:`\n` -> _cdst!(T)++ -> _cdst;

    ;;; do a chain here to ensure that any pop lvars which have been used
    ;;; for non-pop values are destroyed (since syswrite can call user code)
    chain(dev, buf, _pint(##(T){_cdst, buf@T_V_FLD}), syswrite)
enddefine;      /* TRANS_OUT */


    /*  Write out a single line string
    */
define lconstant Trans_line_out(string, dev, encoding, bufpair, _tabval, _plain);
    lvars   char, count, dvec, string, buf, bufpair, dev, item, encoding,
            _csrc, _cdst, _absrc, _clim, _n, _sz, _insz, _attr, _special,
            _dvecptr, _dveclim, _embpos, _curr_ulmode, _curr_bbmode,
            _curr_colour, _tabval, _newbb, _diff, _buflen, _kflags;

    dlvars  _plain;

    if string == termin then
        sysflush(dev, true);    ;;; flush and sync file
        sysclose(dev);
        return
    endif;

    Check_string(string);
    ;;; to allow for attribute representation, assume 8 times string
    ;;; length is needed for buffer
    _shift(string!V_LENGTH, _3) -> _buflen;

    _int(_tabval) -> _tabval;
    _int(_plain) -> _plain;

    if _zero(_plain) and vedstring_data_prop(string) ->> dvec then
        ;;; some embedded data -- check it's only strings and words
        dvec@V_WORDS -> _dvecptr;
        _dvecptr@(w)[dvec!V_LENGTH] -> _dveclim;
        while _dvecptr <@(w) _dveclim do
            _dvecptr!(w)[_1] -> item;
            if isword(item) then item!W_STRING -> item endif;
            if isstring(item) then
                _shift(item!V_LENGTH, _3) _add _buflen -> _buflen
            else
                mishap(dev, string, 2, 'VEDFILE LINE HAS NON-STRING/WORD EMBEDDED DATA')
            endif;
            _dvecptr@(w)[_2] -> _dvecptr
        endwhile
    else
        #_< {} >_# -> dvec
    endif;

    _0 ->> _curr_ulmode ->> _curr_bbmode -> _curr_colour;
    if _buflen _lt _1024 then _1024 -> _buflen endif;

    string!KEY!K_FLAGS -> _kflags;
    if _kflags _bitst _:M_K_STRING16 and not(encoding) then
        subdstring(1, _pint(string!V_LENGTH), string) -> string;
        string!KEY!K_FLAGS -> _kflags
    endif;
    if _kflags _bitst _:M_K_STRING16 then
        unless encoding then
            mishap(dev, 1, 'NO DEVICE ENCODING FOR 16-BIT VEDFILE LINE CHARS')
        endunless;

        define lconstant Sfill(_s, _soffs, _sptr);
            lvars _sptr, _soffs = _sptr@(s){_soffs}, _s;
            while _sptr <@(s) _soffs do _s -> _sptr!(s)++ -> _sptr endwhile
        enddefine;

        TRANS_OUT(s, V_SHORTS, fast_back, inits16, nonmac _DSTRING16_ATTR_PTR,
                    out_ctrl_num16, special_out16, seq_lim16, _smove, Sfill,
                    true);
    else
        TRANS_OUT(b, V_BYTES, fast_front, inits, nonmac _DSTRING_ATTR_PTR,
                    out_ctrl_num8, special_out8, seq_lim8, _bmove, _bfill,
                    false);
    endif
enddefine;      /* Trans_line_out */


    /*  The basic write procedure -- takes an output file/device file
        and an optional arg plain (which defaults to vedwriteoutplain).

        plain has the following values

            0 or <false>:   Write out encoded char attributes and
                            graphics chars as normal
            1               Don't write any special control chars, i.e.
                            no attributes, write graphics chars as their
                            default substitutes
            2               As 1, but don't interpret graphics chars either
            3 or <true>     As 2, and don't interpret trailing spaces

        Returns a line consumer.
    */
define vedfile_line_consumer(file);
    lvars file, plain = vedwriteoutplain, dev;
    if isinteger(file) or isboolean(file) then
        ((), file) -> (file, plain)
    endif;
    unless plain then
        0 -> plain
    elseif plain == true then
        3 -> plain
    endunless;

    if isdevice(file) then
        file
    else
        syscreate(file, 1, false)
    endif -> dev;
    Trans_line_out(% dev, device_encoding(dev, true), conspair(false, false),
                    if vednotabs then 0 else vedindentstep endif, plain %)
enddefine;


    /*  file is a file name or device.
        Write lines from lo to hi to the file.
    */
define vedwriterange(file, lo, hi);
    lvars file, descr, lo, hi, procedure consume;

    if isinteger(file) and isinteger(lo) then
        ;;; allow for args given in the old order, i.e. (lo, hi, file)
        ;;; (this is the wrong order because it stops you creating a closure
        ;;; for a range)
        (file, lo, hi) -> (lo, hi, file)
    endif;

    Check_integer(lo, 1), Check_integer(hi, 1);
    if lo <= vedline and vedline <= hi then vedtrimline() endif;
    min(vedusedsize(vedbuffer), hi) -> hi;

    if isinteger(vedversions) then
        dlocal pop_file_versions = vedversions;
    endif;

#_IF DEF UNIX
    if isinteger(vedcreatemode) then
        dlocal pop_file_mode = vedcreatemode;
    endif;
#_ELSEIF DEF VMS
    dlvars procedure old_final = pop_exception_final;

    define dlocal pop_exception_final(count, mess, idstring, sev);
        lvars text, list;
        if issubstring('quota', 1, mess) then
            'DISK QUOTA EXCEEDED - FILE NOT WRITTEN\n;;;\t\t(See HELP PURGE for information on saving disk space)'
                -> mess
        endif;
        chain(count, mess, idstring, sev, old_final)
    enddefine;
#_ENDIF

    vedfile_line_consumer(file) -> consume;
    if isdevice(file) then false -> file endif;

    if hi == 0 then
        ;;; buffer is empty
        if file then
            file <> '\{b}(empty)' -> descr;
            vedputmessage(descr)
        endif
    else
        if lo > hi then
            mishap(lo, hi, 2, 'IMPOSSIBLE RANGE TO WRITE')
        endif;
        if file then
            file <> (', ' sys_>< (hi - lo + 1)) <> ' lines' -> descr;
            vedputmessage('\{b}writing ' <> descr)
        endif
    endif;

    if file then
        define dlocal pop_file_write_error(dev);
            lvars dev, _s1, _s2;
            ;;; delete file, preserving error code across the call
            Save_errno_apply(device_full_name(dev), sysdelete) -> ;
            Syserr_mishap(device_open_name(dev), 1,
                            'ERROR WRITING DEVICE (file not written)')
        enddefine;
    endif;

    while lo <= hi do
        _CHECKINTERRUPT;
        ;;; write next line
        consume(Buffer_line(lo));
        lo + 1 -> lo
    endwhile;

    returnunless(file);     ;;; i.e. if given a device

    consume(termin);

    vedputmessage(descr <> ' \{b}written')

enddefine;

lconstant cbw_mess = '\{b}not a file - can\'t be written';

protected
define vars ved_write_current_file(explicit) with_props false;
    lvars explicit;
    unless veddirectory then
        false ->> vedwriteable -> vedchanged;
        if explicit then vederror(cbw_mess) endif;
        return
    endunless;
    returnunless(vedchanged);
    if explicit or vvedbuffersize fi_> 0
    ;;; if buffer empty overwrite existing file
    or sys_file_stat(vedpathname, #_<{}>_#)
    then
        vedwriterange(vedpathname, 1, max(1, vvedbuffersize))
    endif;
    false -> vedchanged
enddefine;

define vedwritefiles();
    vedappfiles(
        procedure;
            if vedwriteable or vedwriteallfiles then
                ved_write_current_file(false)
            endif
        endprocedure)
enddefine;

    ;;; write current file only (also used in vdcompile.p)
define Write_one_file with_props ved_w1;
    dlocal ved_on_status = false;
    if vedwriteable or vedwriteallfiles then
        ved_write_current_file(true)
    else
        vederror(if veddirectory then '\{b}this file can\'t be written'
                 else cbw_mess
                 endif)
    endif
enddefine;
;;;
vars procedure ved_w1 = Write_one_file;


define vars ved_w();
    if vedargument = nullstring then
        ;;; write all files
        vedwritefiles();
    else
        ;;; write current file to given name: the use of
        ;;; max(1, vvedbuffersize) forces the file to be written even
        ;;; if it's empty
        vedwriterange(sysfileok(vedargument), 1, max(1, vvedbuffersize));
    endif
enddefine;

define vars ved_wr();
    ;;; write marked range to named file
    if vedargument = nullstring then
        vederror(nfn_mess)
    elseif vvedmarklo > vvedmarkhi then
        vederror('\{b}no marked range')
    else
        vedwriterange(sysfileok(vedargument), vvedmarklo, vvedmarkhi)
    endif
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 24 1999
        Added input/output of Ved hair space char \Sh
--- John Gibson, Oct 15 1998
        Made kill_windows in vedreadfile destroy only wvedfreewindow not
        wvedwindow (which can be the window for an extant file).
--- John Gibson, Oct  2 1998
        Made call to syswrite at end of TRANS_OUT use a chain (see comment).
--- John Gibson, May  2 1997
        Removed setting of vedstatusline in vedreadfile for not(USEWINDOWS)
        (now relies on Ens*ure_statusline in vdstatus.p)
--- John Gibson, Apr 12 1997
        Rewrote vedfile I/O to cope with device encoding; also changed to
        allow embedded 16-bit data.
--- John Gibson, Jan 16 1997
        Changed to use Unix Readin for all systems (this means that
        vedreadin and vedfile_line_repeater can take a device opened in
        either line or false mode).
--- Robert Duncan, Dec  3 1996
        Changed pop_file_write_error in vedwriterange to preserve the code
        for the write error
--- John Gibson, Aug  9 1996
        Changed Trans_line_out to strip trailing special spaces in plain mode.
--- John Gibson, May 25 1996
        Fixed major bug in Unix version of Readin -- in some circumstances,
        a line over 4096 chars in length could have its beginning chopped
        off.
--- John Gibson, May 20 1996
        Changed fourbit_to_ctrl and ctrl_to_fourbit so that 13 is output as
        Ctrl-^ instead of Ctrl-Z (both map back to 13 when read in). Necessary
        to eliminate use of Ctrl-Z because Windows treats it as EOF.
--- John Gibson, May 14 1996
        Changed ved_r to use dev returned by readable instead of leaving it
        garbage. Got rid of readable from ved_write_current_file.
--- John Gibson, Mar 12 1996
        Moved file vector creation to Init_vedfile in vdfiles.p.
--- John Gibson, Feb  7 1996
        vedreadfile now defines local pop_exception_final instead of
        pr*mishap
--- John Gibson, Dec  2 1995
        Changed Readin closures so that device is first frozval. Made
        interrupt in vedreadin close the device.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff, and wveddef*aultwidth and wveddef*aultheight
--- John Gibson, Oct 19 1995
        Added writing and reading of embedded string characters as
        sequences of control chars
--- John Gibson, Jan 19 1995
        Changed vedfile_line_repeater to return a trailing blank line at eof
        as a string containing a 'trailing newline' character (16:9B) when
        vedreadintrailspaces is true. This character is simply ignored when
        writing a file.
--- John Gibson, Mar  4 1994
        Got rid of ved*getfile (replaced by vededit); moved vedopen to
        vdprocess.p
--- John Gibson, Feb  4 1994
        Made Trans_line_in test for 0 length and return nullstring
--- John Gibson, Feb  1 1994
        Added vedcreatemode (used in Unix for file create mode)
--- John Gibson, Jan 31 1994
        More changes to vedreadfile
--- John Gibson, Jan 19 1994
        Extended file-spec argument to vedreadfile, vedopen.
--- Robert John Duncan, Jan 17 1994
        Modified ved_r to use vedsearchlist.
--- John Gibson, Jan 16 1994
        Channelled whole-file writing through the single protected vars
        procedure ved_write_current_file.
--- Robert John Duncan, Jan 14 1994
        Simplified ved_w and ved_w1 to call vedwriterange directly. The
        change to ved_w fixes a bug which meant that vedpathname could get
        screwed if there was an error while writing the file
--- John Gibson, Nov 18 1993
        Rewrote the local pr*mishap in vedreadfile
--- John Gibson, Nov  2 1993
        popmemlim locally set false inside vedreadin as well as vedreadfile.
--- John Gibson, Sep 28 1993
        Took the call of extern sync out of vedwriterange, and instead made
        Trans_line_out use sysflush(dev, true) before closing the file (calls
        extern fsync in Unix).
--- Jonathan Meyer, Sep 25 1993
        Removed (unused) declaration of vedtest*search
--- John Gibson, Apr 27 1993
        Fixed treatment of defaults value in vedreadfile
--- John Gibson, Mar  5 1993
        Added encodings for Ved nobreak space (16:9D)
        and format-control space (16:9C)
--- John Gibson, Jan 12 1993
        popcom*piler -> subsystem etc
--- John Gibson, Dec 11 1992
        Replaced G_ macros with char constant syntax
--- John Gibson, Oct 20 1992
        Uses sys_parse_string
--- John Gibson, Sep 17 1992
        Fixed vedfile_line_consumer so that plain_text arg allows writing
        of graphics chars as ordinary characters (lost in change I made
        on Jul  6 previous!!)
--- Adrian Howard, Sep  8 1992
        Fixed vedreadfile so it doesn't display 'READING' message if vedediting
        is false
--- John Gibson, Aug  5 1992
        Fixed Trans_line_out so that it checks properly for a corrupted tab
        (and replaces one with spaces)
--- John Gibson, Jul  6 1992
        Fixed -SPECIAL_OUT- so that it does no translation when _plain_text
        is true.
--- John Gibson, Jun  6 1992
        Added vedreadintrailspaces
--- John Gibson, Mar 26 1992
        Added -vedreadinplain- and -vedwriteoutplain-.
--- John Gibson, Mar  7 1992
        Basic I/O procedures rewritten as -vedfile_line_repeater- and
        -vedfile_line_consumer-.
        Also allowed args to -vedwriterange- to be
        in the order (name, lo, hi) allowing a closure for a range to be
        created.
--- John Gibson, Jan 27 1992
        Changes for dstrings and file representation of attributes
--- John Gibson, Jan  6 1992
        Changed -Trans_line_in- to interpret backspace and overprinting
        of chars
--- John Gibson, Dec 20 1991
        Replaced vedscreeng*raphoff() with 0 -> vedscreencharmode
--- John Gibson, Sep 14 1991
        Commoned code for -Tab_trans- used by the 2 versions of vedreadin,
        and changed it to use updater Get_string to truncate string.
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, May 27 1991
        Corrected bug in -vedreadfile-
--- John Gibson, Apr  5 1991
        Rewrote -vedreadfile- so that the file being set up is made the
        current file for the duration of the procedure.
        Sectionised.
--- John Williams, Mar 21 1991
        -vedreadin- now complains if given a terminal
        Improved error handling in -ved_r-
--- John Gibson, Dec 11 1990
        popmemlim now an active variable
--- John Gibson, Nov  2 1990
        Some procedures into Sys$-Ved
--- Aaron Sloman, Oct  7 1990
        Replaced various uses of X*VED_LOADED with more direct tests
--- Aaron Sloman, Sep 25 1990
        Replaced Vedfreewindow with wvedfreewindow (exported)
        Changed vedinitialise to take an argument
        Introduced xveddefaults,xvedinitialise
--- Aaron Sloman, Sep  9 1990
        Made -vedreadfile- check procedure argument before setting up new
        environment.
--- Aaron Sloman, Jul 10 1990
        Changed to use -vedexit-
        Renamed -vedwriteone- as -Write_one_file-
        replaced Sys$- with $-Sys (in preparation for sectionising)
--- Aaron Sloman, Jun 26 1990
    Made -vedreadfile- call vedscreengraphoff
--- John Gibson, May 28 1990
        Changed sysopens to use new arg.
--- Robert Smith, Mar 26 1990
        Put the tabs back lost in the last change
--- James Goodlet & Robert Smith, Mar 11 1990 - rewrote -vedreadin- for
        UNIX systems to work around 512 byte/"line" sysread bug, and to
        ~40% increase speed. Ved devices should preferably be opened in
        block I/O mode now.  Also changed -vedreadfile- to open ved device
        in block I/O mode.  Now puts out warning message when reading
        incomplete last line, i.e. no newline.
--- John Williams, Feb  1 1990
        Fixed -interrupt- procedure in -vedreadin-
--- Aaron Sloman, Dec 28 1989
        Added call of new user-definable procedure vedinitialise near
        end of -vedreadfile- (Also near end of ved_name in vdfiles.p)
--- John Gibson, Dec 20 1989
        Change for new pop pointers (using @~POPBASE when freeing
        with -> Get_store() in -Trans_line_in-).
--- James Goodlet, Jul 12 1989 - -vedwriteone- now restores cursor to the
        status line if that's where it was when the function was invoked.
        This cures the bug whereby the length of the status buffer was
        being confused with that of the buffer being written.
 */
