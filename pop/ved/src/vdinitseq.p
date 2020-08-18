/* --- Copyright University of Sussex 2012. All rights reserved. ----------
 > File:            C.all/ved/src/vdinitseq.p
 > Purpose:         VED: setup procedures
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../src/dirs.ph'
#_INCLUDE '../../lib/include/subsystem.ph'


constant
        procedure (vedinascii, vedscreencontrol)
    ;

vars
        procedure (vedscreenscrollregionup, vedscreenscrollregiondown,
        vedscreenraw, vedscreencooked, vedscr_clear_input,
        ),
        vednokeypad, veddumbvdu, vvedscreensendidseq,
    ;

weak constant procedure (
    vedtermcapscreen,
);

section $-Sys$-Ved;

constant
        procedure (Undef_screenxy, Undef_setscrollregion,
        Default_screencursoron, Default_screenraw, Default_screencooked,
        Default_scrollvert, Default_scrollhorz, Default_scrollregion,
        Default_screencharmode, Default_screengraphtrans,
        Set_screen_width
        )
    ;

endsection;


;;; ------------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedsetupdone, vedkeymapname, vedterminalname,
                    vedfunctionkeys, vedinit, vedterminalselect,

                    vednographics, veduseterm, vedvt52select,
                    vedinescapeseq, vedtermidseq, vedinitterm, vedinitcomp,
                    vedsetup,
                    wved_setup_term_trap,
                ;


vars
    vedsetupdone        = false,
    vedkeymapname       = false,
    vedterminalname     = false,
    vedfunctionkeys     = true,

;

vars procedure (
    vedinit             = identfn,

    ;;; Called by check_term, (called by veduseterm)
    wved_setup_term_trap= identfn,
);

lvars
    terminal_ok         = false,
;


#_IF DEF UNIX

;;; Rely on TERM environment variable for determining the terminal name

vars vedterminalselect = [];

define lconstant get_term_type();
    systranslate('TERM');
enddefine;

#_ELSEIF DEF VMS

;;; Use -vedterminalselect- to query the terminal's answer-back message

lconstant answer_back = [
    ['[?63'     vedvt220]
    ['[?62'     vedvt220]
    ['[?'       vedvt100]
    ['/K'       vedvt52select]
];

vars vedterminalselect = [
    '\^[Z'  ^^answer_back
    '\^[[c' ^^answer_back
];

define lconstant get_term_type();
    false;
enddefine;

#_ELSE

;;; No assumptions

vars vedterminalselect = [];

define lconstant get_term_type();
    false;
enddefine;

#_ENDIF

;;; vednographics:
;;;     make graphics chars be displayed as ordinary chars

define vars vednographics();
    Default_screengraphtrans -> vedscreengraphtrans
enddefine;

;;; set_term_defaults:
;;;     set all the terminal control variables to default values as in
;;;     "vdsetup.p". This makes VED totally unusable; terminal
;;;     initialisation libraries must set as many of these as possible to
;;;     sensible values

define lconstant set_term_defaults();

    ;;; unset terminal OK flag

    false -> terminal_ok;

    ;;; 24 line x 80 column screen

    24 -> vedscreenlength;
    80 -> vedscreenwidth;

    ;;; no auto-wrap

    false -> vedscreenwrap;

    ;;; no initialisations needed

    nullstring -> vvedscreeninit;
    nullstring -> vvedscreenreset;

    ;;; no keypad

    true -> vednokeypad;
    nullstring -> vvedscreensetpad;
    nullstring -> vvedscreenresetpad;

    ;;; no graphics

    nullstring -> vvedscreenalpha;
    nullstring -> vvedscreengraphic;
    vednographics();

    ;;; no special char modes
    nullstring -> vvedscreencharnormal;
    nullstring -> vvedscreencharhighlight;
    nullstring -> vvedscreencharbold;
    nullstring -> vvedscreencharunderline;
    nullstring -> vvedscreencharaltfont;
    nullstring -> vvedscreencharblink;

    Default_screencharmode -> nonactive vedscreencharmode;

    ;;; no clear screen or clear line

    nullstring -> vvedscreenclear;
    nullstring -> vvedscreencleartail;

    ;;; no cursor right, cursor up
    ;;; cursor left = BS, cursor down = LF, screen left = CR

    nullstring -> vvedscreencharright;
    nullstring -> vvedscreencharup;
    `\b` -> vvedscreencharleft;
    `\n` -> vvedscreenchardown;
    `\r` -> vvedscreenscreenleft;

    ;;; scroll up = LF, no scroll down

    `\n` -> vvedscreenscrollup;
    nullstring -> vvedscreenscrolldown;

    ;;; no char/line insert/delete

    false -> vednomoveinsert;
    true -> vednocharinsert;
    nullstring -> vvedscreeninsertmode;
    nullstring -> vvedscreenovermode;
    nullstring -> vvedscreeninsertchar;
    true -> vednochardelete;
    nullstring -> vvedscreendeletechar;
    true -> vednolineinsert;
    nullstring -> vvedscreeninsertline;
    true -> vednolinedelete;
    nullstring -> vvedscreendeleteline;
    true -> veddumbvdu;

    ;;; no cursor motion

    Undef_screenxy -> vedscreenxy;

    ;;; no decent scrolling

    false -> terminal_can_scroll;
    Undef_setscrollregion -> vedsetscrollregion;

    ;;; default terminal bell

    `\^G` -> vvedscreenbell;

    ;;; default cursor on/off (just returns true)
    Default_screencursoron -> nonactive vedscreencursoron;
    ;;; other defaults
    Default_screenraw -> vedscreenraw;
    Default_screencooked -> vedscreencooked;
    Default_scrollvert -> vedscrollvert;
    Default_scrollhorz -> vedscrollhorz;
    Default_scrollregion -> vedscrollregion;


    ;;; terminal type unknown
    ;;; NB: this must be done last, since it implies that everything else
    ;;; has been done

    false ->> vedkeymapname -> vedterminalname;

enddefine;

;;; check_term:
;;;     checks that all the terminal control variables which VED needs
;;;     have been set, and if so does some extra configurations

define lconstant check_term();
    ;;; check that the terminal's been set up at all
    returnunless(vedterminalname)(false);
    ;;; avoid doing things twice
    returnif(vedterminalname == terminal_ok)(true);
    false -> terminal_ok;

    if USEWINDOWS then wved_setup_term_trap() endif;

    ;;; check that the essential controls are available
    if vvedscreencleartail = nullstring
    or vvedscreencharright = nullstring
    or vvedscreencharup = nullstring
    or vedscreenxy = Undef_screenxy
    then
        return(false);
    endif;
    ;;; Terminal OK:
    ;;; check on provision of character/line insert/delete
    if vvedscreeninsertmode /= nullstring and vvedscreenovermode /= nullstring
    or vvedscreeninsertchar /= nullstring
    then
        false ->> vednocharinsert -> veddumbvdu;
    endif;
    if vvedscreendeletechar /= nullstring then
        false ->> vednochardelete -> veddumbvdu;
    endif;
    if vvedscreeninsertline /= nullstring then
        false ->> vednolineinsert -> veddumbvdu;
    endif;
    if vvedscreendeleteline /= nullstring then
        false ->> vednolinedelete -> veddumbvdu;
    endif;
    ;;; select the best scrolling method
    if vedsetscrollregion /= Undef_setscrollregion
    and vvedscreenscrollup /= nullstring
    and vvedscreenscrolldown /= nullstring
    then
        ;;; terminal supports scrolling regions
        1 -> terminal_can_scroll;
    elseif vednolineinsert or vednolinedelete then
        ;;; no scrolling at all
        false ->> terminal_can_scroll -> vedscrollscreen;
    else
        true -> terminal_can_scroll;
    endif;
    ;;; A.S. 8 Nov 2011
    ;;; make this false by default
    ;;; EXPERIMENT -- Confirmed 25 Sep 2012
    ;;; solves problem on various xterm emulations that cannot scroll properly
    false -> terminal_can_scroll;

    ;;; check for a keypad
    if vvedscreensetpad /= nullstring
    and vvedscreenresetpad /= nullstring
    then
        false -> vednokeypad;
    endif;

    ;;; make screen width settings
    Set_screen_width();

    not(ved_on_status) -> ved_on_status;
    Set_vedcommand(false);

    vedterminalname -> terminal_ok;
    return(true);
enddefine;

;;; veduseterm:
;;;     initialises VED for a named terminal. Appropriate settings are
;;;     got either from a library file or (where possible) from termcap.
;;;     Any existing terminal settings are cleared.
;;;     Returns <true> if the setting-up worked.

define veduseterm(name);
    lvars i, root = name, name, pdr_name, fnkeys;
    if root == undef then
        ;;; reset back to undefined state
        set_term_defaults();
        ;;; VED is now unusable
        return(false);
    elseif isword(root) then
        root!W_STRING -> root;
    else
        Check_string(root);
    endif;
    ;;; terminal name may involve hyphens for appending qualifiers (like
    ;;; "vt100-w"): replace these with underscores
    if locchar(`-`, 1, root) ->> i then
        copy(root) -> root;
        repeat
            `_` -> fast_subscrs(i, root);
        quitunless(locchar(`-`, i fi_+ 1, root) ->> i);
        endrepeat;
    endif;
    ;;; clear any existing terminal settings
    if vedterminalname then set_term_defaults() endif;
    ;;; try to find a screen-setup procedure with this name,
    ;;; or failing that, try using termcap
    consword('ved' <> root <> 'screen') -> pdr_name;
    if testdef sys_autoload then weakref sys_autoload(pdr_name) -> endif;
    if isdefined(pdr_name) then
        valof(pdr_name)();
    elseif testdef vedtermcapscreen then
        weakref vedtermcapscreen(name);
    else
        return(false);
    endif;
    ;;; check the terminal's suitability
    returnunless(check_term())(false);
    ;;; load a function key file if wanted and if available
    vedfunctionkeys -> fnkeys;
    while isprocedure(fnkeys) do
        fnkeys(consword(root)) -> fnkeys;
    endwhile;
    if fnkeys then
        vedterminalname -> vedkeymapname;
        if isword(fnkeys) then fnkeys!W_STRING -> root endif;
        consword('ved' <> root <> 'keys') -> pdr_name;
        if testdef sys_autoload then weakref sys_autoload(pdr_name) -> endif;
        if isdefined(pdr_name) then
            valof(pdr_name)()
        endif;
    endif;
    ;;; done
    true;
enddefine;

;;; vedvt52select:
;;;     default setup for VT52-type terminal
;;;     (used to be -identfn- when VED was Visual 200 by default; now has
;;;     to be selected explicitly)

define vars vedvt52select();
    veduseterm("vi200") -> ;
enddefine;

;;; vedinescapeseq:
;;;     reads an answer-back sequence from the terminal

define vedinescapeseq();
    lvars   char;
    dlocal  pop_timeout_secs = 3, ved_char_in_stream = [];

    define dlocal pop_timeout();
        clearstack();
        exitfrom(false, vedinescapeseq);
    enddefine;

    define lconstant readseq(lo, hi, rep);
        lvars lo, hi, char, rep;
        while (vedinascii() -> char; lo <= char and char <= hi) do
            char;
            unless rep then return endunless;
        endwhile;
        if rep then
            char :: ved_char_in_stream -> ved_char_in_stream
        else
            pop_timeout()
        endif;
    enddefine;

    consstring (#|
        unless vedinascii() == vedescape then return(false) endunless;
        vedinascii() -> char;
        if char == `[` then
            char;
            readseq(16:30, 16:3F, true);
            readseq(16:20, 16:2F, true);
            readseq(16:40, 16:7E, false);
        elseif lmember(char, [`;` `?` `O`])
        or (16:20 <= char and char <= 16:2F)
        then
            char;
            readseq(16:20, 16:2F, true);
            readseq(16:30, 16:7E, false);
        else
            pop_timeout();
        endif;
    |#);
enddefine;

;;; vedtermidseq:
;;;     prompts the terminal for an answer-back sequence and returns the
;;;     reply as a string, or <false> if no reply

define vedtermidseq();
    vedscr_clear_input();
    vedscreencontrol(vvedscreensendidseq);
    vedscr_flush_output();
    vedinescapeseq();
enddefine;

;;; tryvedterminalselect:
;;;     uses -vedterminalselect- to determine the terminal name
;;;     (for backward compatability)

define lconstant tryvedterminalselect();
    lvars item, match_string = undef, term = false;
    returnunless(islist(vedterminalselect));
    for item in vedterminalselect do
        if isword(item) then valof(item) -> item endif;
        if isstring(item) then
            ;;; use it as the interrogation sequence
            procedure(vvedscreensendidseq);
                dlocal vvedscreensendidseq;
                vedtermidseq();
            endprocedure(item) -> match_string;
        elseif isprocedure(item) then
            ;;; run the procedure to get a match string
            item() -> match_string;
        else
            if match_string == undef then
                ;;; default to get term id seq
                vedtermidseq() -> match_string;
            endif;
            ;;; should be a 2-element list [<string> <pdr|word|string>]
            if hd(item) = match_string
            or isstring(match_string)
            and isstartstring(hd(item), match_string)
            then
                hd(tl(item)) -> term;
                quitloop;
            endif;
        endif
    endfor;
    returnunless(term);
    if isword(term) then valof(term) -> term endif;
    if isprocedure(term) then
        term(if pdnargs(term) /== 0 then match_string endif);
    else
        veduseterm(term) -> ;
    endif;
enddefine;

;;; ask_term_type:
;;;     ask the user for a terminal type, prompting with -msg-

define lconstant ask_term_type(msg) -> input;
    lvars msg, input, n;
    lconstant macro BUFLEN = 256;
    dlocal
        pop_charin_device   = popdevin,
        pop_charout_device  = popdevout,
        pop_charerr_device  = popdeverr,
        cucharout           = charout,
        cucharerr           = charerr,
        pop_pr_quotes       = false,
    ;
    unless systrmdev(popdevin) and systrmdev(popdevout) then
        mishap(0, msg);
    endunless;
    if cucharin == charin then
        if sys_input_waiting(popdevin) then
            ;;; assume it's a filename argument to the "ved" macro etc.
            device_init_buffer(popdevin, BUFLEN) -> input;
            sysread(popdevin, input, BUFLEN) -> n;
            substring(1, n, input) -> nextchar(readitem);
        endif;
    endif;
    sys_clear_input(popdevin);
    unless msg = nullstring then
        sys_syspr(msg), sys_syspr(newline);
    endunless;
    sys_syspr('Enter an alternative terminal type, or <RETURN> to abandon VED');
    sysflush(popdevout);
    procedure;
        dlocal
            popprompt = ': ',
            proglist_state = proglist_new_state(charin);
        readstringline();
    endprocedure() -> input;
    if input = nullstring then interrupt() endif;
enddefine;

;;; vedinitterm:
;;;     determine the terminal type and initialise VED to use it

define vars vedinitterm();
    lvars name, msg, done = false;
    ;;; Try -vedterminalselect- (for backward compatability)
    unless vedterminalname then
        tryvedterminalselect()
    endunless;
    if vedterminalname then
        ;;; check the terminal's suitability
        check_term() -> done;
    elseif get_term_type() ->> name then
        veduseterm(name) -> done;
    endif;
    until done do
        if vedterminalname then
            ;;; terminal name set, but doesn't check out - must be unsuitable
            'Unsuitable terminal type: ' sys_>< vedterminalname;
            ;;; clear any setting-up already done, in case of interrupts
            set_term_defaults();
        elseif name then
            ;;; terminal name not set - must be unknown
            'Unknown terminal type: ' <> name;
        else
            ;;; can't get a terminal name at all
            'Can\'t determine terminal type';
        endif -> msg;
        ;;; prompt the user for a terminal name:
        ;;; this may never return if the user chooses to abort VED
        ask_term_type(msg) -> name;
        veduseterm(name) -> done;
    enduntil;
enddefine;

;;; vedinitcomp:
;;;     compile 'vedinit' files

define vars vedinitcomp();

    define lconstant initcomp(fname);
        lvars fname;
        lconstant macro TRYCOMPILE = [weakref[pop11_compile] trycompile];
        TRYCOMPILE(POPSYS dir_>< fname) -> ;
        TRYCOMPILE(POPLIB dir_>< fname) or TRYCOMPILE(fname) ->
    enddefine;

    if testdef pop11_compile then
        initcomp('vedinit.p');
    endif
enddefine;

;;; vedsetup:
;;;     initialise VED

define vedsetup();
    lvars ss;
    dlocal poplastchar;             ;;; because -compile- doesn't !
    returnif(vedsetupdone);

    if interrupt == sysexit and subsystem then setpop -> interrupt endif;

    if subsystem and Subsystem_find(subsystem, SS_NAME) ->> ss then
        Apply_ss_procedure(ss, SS_VEDSETUP)
    endif;

    unless pop_noinit then vedinitcomp(); endunless;

    vedinitterm();

    until null(ved_runtime_actions) do
        apply(fast_destpair(ved_runtime_actions) -> ved_runtime_actions)
    enduntil;

    vedinit();                  ;;; user definable

    true -> vedsetupdone;
enddefine;


endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 8 Nov 2011 (25 Sep 2012)
    Override old settings for terminal_can_scroll: just make it false
    Added new active variable with updater in vdscroll.p
        ved_terminal_can_scroll();
    to control the previously hidden variable:
        terminal_can_scroll
    whose default value is now set false here
--- Aaron Sloman, 15 Mar 2004
        Disabled use of termcap on Linux. See $popsrc/termcap.p
--- John Gibson, May  2 1997
        Changed check_term to call Set_screen_width
--- John Gibson, Apr 12 1997
        Changed ask_term_type to use an appropriate buffer for sysread.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Williams, Aug  1 1995
        SS_VEDSETUP procedures now run by vedsetup, not vedinitcomp.
--- Robert John Duncan, Sep  5 1994
        Made non-null initial value of vedterminalselect specific to VMS
        and added a default case with no assumptions about terminal type
--- John Gibson, Mar 18 1994
        Got rid of w*ved_ved_i*nit and w*ved_ved_i*nit_comp
--- John Gibson, Jun 16 1993
        Weakened references to trycompile in vedinitcomp
--- John Williams, Jan 21 1993
        Renamed vedsetup_actions as ved_runtime_actions
--- John Williams, Jan  8 1993
        Added vedsetup_actions
--- John Gibson, Jul 24 1992
        Replaced test in vedinitcomp with XWINDOWS instead of popunderx
--- John Gibson, Jun  3 1992
        Made set_term_defaults set vedscrollregion to Default_scrollregion
--- John Gibson, Apr 21 1992
        Sectionised.
--- John Gibson, Mar  3 1992
        Removed initialisation of vedst*atusheader (not used any more).
--- John Gibson, Feb  6 1992
        Made -vednographics- assign standard graphics chars to variables,
        and assign -Default_screengraphtrans- to -vedscreengraphtrans-.
--- John Gibson, Jan 23 1992
        New graphic chars
--- John Gibson, Jan 17 1992
        Added default initialisation for -vvedscreencharaltfont-
--- John Gibson, Jan  8 1992
        Changes for dstrings
--- John Gibson, Dec 20 1991
        Added default assignments in set_term_defaults for char modes
--- John Gibson, Sep 20 1991
        Added more default assignments in set_term_defaults
--- Robert John Duncan, Sep 19 1991
        Allowed -undef- as argument to -veduseterm- meaning reset back to
        undefined state.
--- John Gibson, Jul  9 1991
        Default/undef screen procedures now in Sys$-Ved. Added default
        assignment for vedscreencursoron.
--- John Gibson, Jul  6 1991
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Williams, Oct 31 1990
        -vedsetup- now respects -pop_noinit-
--- John Williams, Oct 12 1990
        Changed -vedkeyboardname- to -vedkeymapname-
--- Aaron Sloman, Oct 12 1990
        Changed xved... to wved...
--- Aaron Sloman, Oct  8 1990
        Changed guard on xvedsetup
--- Aaron Sloman, Oct  8 1990
        Removed attempt to compile vedinitx.p from sysinitcomp
--- John Williams, Oct  5 1990
        Added -vedkeyboardname-
--- Aaron Sloman, Sep 30 1990
        vedsetup now calls xvedinitcomp and user definable xvedinit
        if popunderx true
--- Aaron Sloman, Sep 22 1990
        check_term altered to call xvedsetup
        vedinitcomp changed to try vedinitx.p in POPLIB or current dir.
        changed unless...do to unless...then
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- Rob Duncan, Jun 21 1990
        Termcap procedure now called -vedtermcapscreen-; made it a "weak"
        declaration. Made -vedterminalselect- nil rather than false, as
        this is less likely to break existing programs. Made -vednographics-
        "vars" to allow redefinition of substitute graphics characters.
        Combined all conditional compilation into one place.
--- Rob Duncan, Jun 12 1990
        Made -ask_term_type- more robust. Extended functionality of
        -vedfunctionkeys- in -veduseterm-.
--- John Gibson, Jun  6 1990
        Replaced use of charX_dev with pop_charX_device
--- Rob Duncan, Apr 25 1990
        Reinstated old value of -vedterminalselect- for VMS until we can
        work out how to get the terminal type automatically.
--- John Williams, Apr 24 1990
        -vedinitterm- no longer needs to load LIB VEDPW*MSUN explicitly.
        Moved call of Pw*m$-Setup_ved to -check_term-.
--- Rob Duncan, Feb  2 1990
        Removed all assignments to -vedstartwindow- (now an active variable
        in "vdwindows.p")
--- Rob Duncan, Jan 30 1990
        Moved -vedtermsetup- out to a library.
        Fixed -vednographics- to assign to status header rather than status
        line.
        Changed transformation of terminal name in -veduseterm-
--- John Williams, Jan 23 1990
        Added call of -check_term- to PW*M setup code in -vedinitterm-
        Re-instated 'dlocal poplastchar' in -vedsetup-
--- John Williams, Jan 19 1990
        -vedstartwindow- initialised to 12 again, but only set to
        half -vedscreenlength- if changed
--- John Williams, Jan 19 1990
        Moved compilation of vedinit files into -vedinitcomp-, called
        from -vedsetup-
        Moved PW*M stuff from -vedsetup- to -vedinitterm-
        -vedstartwindow- initialised to false, and only set to half
        -vedscreenlength- if not set by user
--- Rob Duncan, Jan 17 1990
        Renamed -vedtermsetup- to -vedinitterm- and called it from inside
        -vedsetup- (rather than the other way round).
        Changed -check_term- to set -vednocharinsert- etc. Added flag
        -terminal_ok- to prevent re-checking the same terminal on startup.
        Extended -tryvedterminalselect-.
--- Rob Duncan, Nov 15 1989
        changed -ask_term_type- again: added test of -charout_dev- and
        dlocal of -pr- and -cucharout-
--- Rob Duncan, Nov 10 1989
        changed -ask_term_type- to dlocal -proglist- and use -readstringline-
        rather than -readline-
--- Rob Duncan, Nov  7 1989 (Substantially modified)
        changed -vedtermsetup- to use -get_term_type- and -ask_term_type-
        to get hold of the terminal name: -vedterminalselect- becomes
        optional and no longer mishaps if it fails;
        added -vedusetermcap- to map a terminal name onto appropriate
        library files or the new termcap interface;
        changed -vednographics- to set graphic characters explicitly
        instead of via lib vednographic.
 */
