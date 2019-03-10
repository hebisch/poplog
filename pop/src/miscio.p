/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/miscio.p
 > Purpose:         Miscellaneous I/O stuff
 > Author:          John Gibson (see revisions)
 */


#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'gctypes.ph'

section $-Sys;

global constant
        procedure (Bytevec_hashint, Unique_hash, Ensure_writeable,
        Extern$-Get_encoding_funcs)
    ;

weak global vars
        (charin_dev, $-lisp$-closed_read_ok)
    ;

endsection;

section $-Sys$-Io;

global constant
        procedure (Log_char, Is_foreground_term, No_read, No_write)
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys => popprompt, pop_default_type, poplastchar, pop_buffer_charout,
                 pop_timeout, pop_timeout_secs, pop_file_write_error,
                 pop_default_device_encoding,
                 fast_sysread, fast_syswrite, isdevice, systrmdev, termin,
                 device_key, termin_key;

vars
    popprompt           = ': ',
    pop_default_type    = '.p',
    poplastchar         = `\n`,
    pop_buffer_charout  = true,
    pop_timeout_secs    = false,
    procedure pop_timeout   = identfn,
    procedure pop_file_write_error = erase,

    ;;; Default encoding for devices with org argument /= true
    default_device_encoding = false,    ;;; N.B. not restored
    ;

constant
    ;;; Ident of this used for devs with org argument = true
    no_device_encoding  = false,
    ;

define active pop_default_device_encoding;
    default_device_encoding and default_device_encoding!XP_PROPS
enddefine;
;;;
define updaterof pop_default_device_encoding encoding_name;
    encoding_name and Extern$-Get_encoding_funcs(encoding_name)
                -> default_device_encoding
enddefine;


;;; --- TABLE OF OPEN DEVICES, ETC ----------------------------------------

    ;;; the table of open files -- allows for 256 files
constant
    _file_tab       = _INIT_NONPOP_STRUCT(w[256]),
    _file_tab_limit = _file_tab@(w)[_256],
    ;

vars
    ;;; pointer to next free entry
    _file_tab_next_free = _file_tab,

    ;;; pointer to down-growing table of garbage files to close
    _file_tab_close_ptr = _file_tab_limit,
    ;


    /*  Add device etc to table
    */
define Add_file_tab_entry(dev);
    lvars dev;
    returnif(dev <@(w) _system_end);
    if _file_tab_next_free >=@(w) _file_tab_close_ptr then
        Sysgarbage(true, 'ftbl');
        if _file_tab_next_free >=@(w) _file_tab_close_ptr then
            mishap(0, 'CAN\'T CREATE DEVICE - DEVICE TABLE IS FULL')
        endif
    endif;
    ;;; add file to file table
    dev -> _file_tab_next_free!(w)++ -> _file_tab_next_free
enddefine;

    /*  Close files in the file table -- if _all is true close all files,
        otherwise close only those in the close table
    */
define Close_filetab_files(_all);
    lvars dev, _lim = _file_tab_limit, _size, _all;
    if _all then
        ;;; put all files in the close area
        @@(w){_file_tab_next_free, _file_tab} -> _size;
        _file_tab_close_ptr@(w)-{_size} -> _file_tab_close_ptr;
        _move(_size, _file_tab, _file_tab_close_ptr) -> ;
        _file_tab -> _file_tab_next_free
    endif;

    while _file_tab_close_ptr <@(w) _lim do
        _file_tab_close_ptr!(w)++ -> (dev, _file_tab_close_ptr);
        sysclose(dev)
    endwhile
enddefine;

    /*  Apply app_p to all open devices
    */
define App_open_devs(app_p);
    lvars _entry;
    dlvars procedure app_p;

    define lconstant Try(dev);
        lvars dev;
        ;;; dev could be uninitialised dev_out, or undef weakref etc
        if isdevice(dev) and not(dev!D_FLAGS _bitst _M_D_CLOSED) then
            chain(dev, app_p)
        endif
    enddefine;

    Try(weakref popdevin);
    Try(popdevout);
    Try(popdeverr);
    Try(weakref poprawdevin);
    Try(weakref poprawdevout);

    _file_tab -> _entry;
    while _entry <@(w) _file_tab_next_free do
        Try(_entry!(w)++ -> _entry)
    endwhile;

    _file_tab_close_ptr -> _entry;
    while _entry <@(w) _file_tab_limit do
        Try(_entry!(w)++ -> _entry)
    endwhile
enddefine;      /* App_open_devs */


;;; -----------------------------------------------------------------------

define fast_sysread(/*dev, _bsub, userbuf, _nbytes*/) with_nargs 4;
    fast_apply((), _user_sp()!(w)[_3]!D_READ)
enddefine;

define fast_syswrite(/*dev, _bsub, userbuf, _nbytes*/) with_nargs 4;
    fast_apply((), _user_sp()!(w)[_3]!D_WRITE)
enddefine;


;;; --- DEVICE KEY -----------------------------------------------------

    /*  For _checkopen an integer, bits are
            2:0001  Check system device
            2:0010  Check open for reading
            2:0100  Check open for writing
            2:1000  Flush if open for writing
    */
define Check_device(_item, _checkopen);
    lvars _item, _checkopen;
    unless iscompound(_item) and _item!KEY == device_key do
        mishap(_item, 1, 'DEVICE NEEDED')
    endunless;
    returnunless(_checkopen);
    if _item!D_FLAGS _bitst _M_D_CLOSED then
        mishap(_item, 1, 'ATTEMPT TO USE CLOSED DEVICE')
    endif;
    returnunless(isinteger(_checkopen));
    _int(_checkopen) -> _checkopen;
    if _checkopen _bitst _2:0001
    and _item!D_FLAGS _bitst _M_D_USER_DEV then
        mishap(_item, 1, 'SYSTEM DEVICE NEEDED')
    endif;
    if _checkopen _bitst _2:0010 and _item!D_READ == Io$-No_read then
        mishap(_item, 1, 'READABLE DEVICE NEEDED')
    endif;
    if _checkopen _bitst _2:0100 and _item!D_WRITE == Io$-No_write then
        mishap(_item, 1, 'WRITEABLE DEVICE NEEDED')
    endif;
    if _checkopen _bitst _2:1000 and _item!D_WRITE /== Io$-No_write then
        fast_apply(_item, _item!D_FLUSH)
    endif
enddefine;

define isdevice(item);
    lvars item;
    iscompound(item) and item!KEY == device_key
enddefine;

define systrmdev(dev);
    lvars dev;
    Check_device(dev, false);
#_IF DEF UNIX
    unless dev!D_FLAGS _bitst _M_D_LOGICAL_TERM then
        false
    elseif dev!D_FLAGS _bitst _M_D_USER_DEV
    or Io$-Is_foreground_term(dev!D_FILE_DESC) then
        true
    else
        ;;; return termin for terminal wrt which we are backgrounded
        ;;; (n.b. could also be that terminal has been closed!)
        termin
    endunless
#_ELSE
    dev!D_FLAGS _bitst _M_D_LOGICAL_TERM
#_ENDIF
enddefine;

    /*  Test for (possibly weakref'ed) -dev- being a (foreground) terminal
    */
define On_line_term(dev);
    lvars dev;
    isdevice(dev) and systrmdev(dev) == true
enddefine;

define lconstant Dev_print(dev);
    lvars dev;
    Default_print(dev, dev!D_FULL_NAME or dev!D_OPEN_NAME)
enddefine;

define lconstant Dev_hash() with_nargs 1;
    _pint(Bytevec_hashint(()!D_OPEN_NAME))
enddefine;

constant
    device_key = struct KEY_R_NAFULL =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_NFULLREC,       ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "device",               ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isdevice,               ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Dev_print,              ;;; K_SYS_PRINT
        WREF Dev_print,         ;;; K_PRINT
        WREF Dev_hash,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct DEVICE)++,    ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R

        @@(int)[_16],           ;;; K_FULL_OFFS_SIZE
        =>> {%                  ;;; K_FULL_OFFS_TAB[_16]
                @@D_CTRL_BLK,
                @@D_UNIT_N,
                @@D_UNIT_P,
                @@D_OPEN_NAME,
                @@D_FULL_NAME,
                @@D_IN_BUFFER,
                @@D_OUT_BUFFER,
                @@D_READ,
                @@D_WRITE,
                @@D_FLUSH,
                @@D_SEEK,
                @@D_CLOSE,
                @@D_CLEAR_INPUT,
                @@D_TEST_INPUT,
                @@D_ENCODING_ID,
                @@D_ENCODING_OWN_ID
            %}
        %};


;;; --- TERMIN ------------------------------------------------------

constant
    termin_printstring = '<termin>';

define lconstant Termin_print() with_nargs 1;
    -> ;
    if cucharout == identfn then
        deststring(termin_printstring) ->
    else
        cucharout(termin)
    endif
enddefine;

constant
    termin_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL,          ;;; K_FLAGS
        _:GCTYPE_NONE,          ;;; K_GC_TYPE
        Rec1_getsize,           ;;; K_GET_SIZE

        "termin",               ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        nonop ==(%termin%),     ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Termin_print,           ;;; K_SYS_PRINT
        WREF Termin_print,      ;;; K_PRINT
        WREF Unique_hash,       ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %},

    termin  = struct {full F1, KEY; >->} =>> {%0, termin_key%},
    ;


;;; ----------------------------------------------------------------------

section Io;

define Prompt(dev) -> string;
    lvars dev, string = popprompt;
    if isprocedure(string) then fast_apply(string) -> string endif;
    unless isstring(string) then
        mishap(string, 1, 'PROMPT NOT A STRING')
    endunless;
    if testdef charin and weakref[charin] charin_dev == dev then
        ;;; in case there's a log file do
        appdata(string, weakref[charin] Log_char)
    endif
enddefine;


;;; --- DEFAULT DEVICE PROCEDURES ------------------------------------------

lconstant
    nr_mess = 'DEVICE NOT OPEN FOR READING',
    nw_mess = 'DEVICE NOT OPEN FOR WRITING',
    ;

lvars template = false;

    /*  This procedure is redefined for the standard devices opened
        in setpop.p
    */
define vars Cons_device_getdev() /* -> (own_encoding_id, dev) */;
    copy(ident template), Get_store(@@(struct DEVICE)++)
enddefine;


define No_read(dev, _bsub, userbuf, _nbytes) with_props '(Sys$-Io$-No_read)';
    lvars dev, _bsub, userbuf, _nbytes;
    mishap(dev, 1, nr_mess)
enddefine;

define No_write(dev, _bsub, userbuf, _nbytes) with_props '(Sys$-Io$-No_write)';
    lvars dev, _bsub, userbuf, _nbytes;
    if testdef poprawdevin and dev == weakref[poprawdevin] raw_dev_in then
        weakref[poprawdevin] raw_dev_out -> dev;
        fast_apply(dev, _bsub, userbuf, _nbytes, dev!D_WRITE)
    else
        mishap(dev, 1, nw_mess)
    endif
enddefine;

define No_seek(dev, _pos, _mode) with_props '(Sys$-Io$-No_seek)';
    lvars dev, _pos, _mode;
    mishap(dev, 1, 'CANNOT SEEK ON DEVICE')
enddefine;

define No_flush(dev) with_props '(Sys$-Io$-No_flush)';
    lvars dev;
    if testdef poprawdevin and dev == weakref[poprawdevin] raw_dev_in then
        weakref[poprawdevin] raw_dev_out -> dev;
        fast_apply(dev, dev!D_FLUSH)
    else
        mishap(dev, 1, nw_mess)
    endif
enddefine;

define No_input(dev) with_props '(Sys$-Io$-No_input)';
    lvars dev;
    mishap(dev, 1, nr_mess)
enddefine;

define Init_device() -> _dev;
    lvars (enc_id, _dev) = Cons_device_getdev();

    device_key  ->  _dev!KEY;
    _0          ->  _dev!D_FLAGS;
    _-1         ->  _dev!D_FILE_DESC;

    false       ->> _dev!D_CTRL_BLK
                ->> _dev!D_UNIT_N
                ->> _dev!D_UNIT_P
                ->> _dev!D_OPEN_NAME
                ->> _dev!D_FULL_NAME
                ->> _dev!D_IN_BUFFER
                ->  _dev!D_OUT_BUFFER;

    ;;; procedures
    No_read     ->  _dev!D_READ;
    No_write    ->  _dev!D_WRITE;
    No_seek     ->  _dev!D_SEEK;
    No_flush    ->  _dev!D_FLUSH;
    No_input    ->> _dev!D_CLEAR_INPUT
                ->  _dev!D_TEST_INPUT;
    erase       ->  _dev!D_CLOSE;

    ident default_device_encoding
                -> _dev!D_ENCODING_ID;
    enc_id      -> _dev!D_ENCODING_OWN_ID;
enddefine;

define Kill_device(dev);
    lvars dev;

    define lconstant Closed_read(dev, _bsub, userbuf, _nbytes);
        lvars dev, _bsub, userbuf, _nbytes;
        if testdef $-lisp$-closed_read_ok
        and weakref $-lisp$-closed_read_ok then
            0
        else
            mishap(dev, 1, 'ATTEMPT TO READ CLOSED DEVICE')
        endif;
    enddefine;

    define lconstant Closed_write(dev, _bsub, userbuf, _nbytes);
        lvars dev, _bsub, userbuf, _nbytes;
        mishap(dev, 1, 'ATTEMPT TO WRITE CLOSED DEVICE')
    enddefine;

    dev!D_FLAGS _biset _M_D_CLOSED -> dev!D_FLAGS;
    Closed_read     -> dev!D_READ;
    Closed_write    -> dev!D_WRITE;
    erase           -> dev!D_CLOSE
enddefine;

define Cons_iobuffer(_size) -> buffer;
    lvars buffer, _size;
    returnif(_size == 0) (false -> buffer);
    _int(_size) -> _size;
    Ensure_writeable(inits(_pint(_size _add (##BUF_START _sub ##V_BYTES))))
                                            -> buffer;
    _size -> buffer!BUF_SIZE;
    _0 ->> buffer!BUF_POSITION ->> buffer!BUF_COUNT -> buffer!BUF_ENCODE_STATE;
    ;;; these fields for VMS only
    _0 -> buffer!BUF_BLK_NUM;
    _0 -> buffer!BUF_FLAGS
enddefine;


endsection;     /* Io */


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  4 1997
        Replaced D_ENCODING with D_ENCODING_ID and D_ENCODING_OWN_ID
--- John Gibson, Mar  8 1997
        Added pop_default_device_encoding.
--- John Gibson, Feb 18 1997
        Added D_ENCODING field to device_key full table; added
        initialisations for device encoding field in Init_device.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Feb 24 1995
        Changed systrmdev to use Is_foreground_term rather than
        Is_back*ground_term (means that systrmdev will return termin if the
        terminal file descriptor has been closed)
--- John Gibson, Aug 15 1994
        Changes to _file_tab layout
--- John Gibson, Oct  7 1992
        Made all default device procedures perm constants and gave them
        string pdprops for POPC
--- John Gibson, Dec 12 1990
        Added D_UNIT_N and D_UNIT_P fields to devices
--- John Gibson, Dec  4 1990
        Added -App_open_devs-
--- John Williams, Nov 21 1990
        Temporary hack to -Closed_read- to fix Lisp problem
--- John Gibson, Oct 26 1990
        Changed -systrmdev- to use _M_D_LOGICAL_TERM.
        Changed -O*n_line_tty_input- to -On_line_term- taking a dev
        as argument.
--- John Gibson, Oct 24 1990
        Added device fields for clear/test input procedures. Commoned
        code from VMS/Unix devio.p to make -Init_device- etc.
--- John Gibson, Oct 22 1990
        Device read/write procedures now take all-pop args (same as for
        4-arg sysread/write, etc).
--- John Gibson, Sep  1 1990
        Commoned Cons_iobuffer from Unix/VMS devio.p into this file.
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Feb 27 1990
        Added extra arg to call of -Sysgarbage-.
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Feb 19 1989
        Included io.ph
 */
