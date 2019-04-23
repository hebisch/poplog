/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdprocess.p
 > Purpose:         Editor Process
 > Author:          Aaron Sloman & John Gibson (see revisions)
 */

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../src/io.ph'
#_INCLUDE '../../src/process.ph'

constant
        procedure (vedsetup, vedscreenovermode,
        vedputcommand, vednextitem, vedcurrentchar, vedreadfile
        ),
        active (ved_chario_file)
    ;

vars
        procedure (vedscreenraw, vedcompilevedargument,
        ved_moveto, ved_obey, ved_search, ved_backsearch, vedrestorewindows,
        wved_raise_window, pop_default_pr_exception, pop_default_prmishap,
        ),
        vednamestring, wvedalwaysraise
    ;

section $-Sys$-Ved;

constant
        procedure (Searchlist_name, Set_basewindow)
    ;

vars
        screenline_refresh_col, screenline_tail_isclear,
        Im$-chario_file
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Ved =>
                vedveddefaults, vedinitfile, vedprocesstrap, vedcommandtrap,
                vededitortrap, vedinterrupt, vededitorexit, vedediting,
                vedcommand, vedargument, vedmessage, vedlastcommand,
                veduserdocommand, vedrefreshneeded, wvedwindowchanged,
                vedexpandchar, vedexpandchars, vedsearchlist, vedinputfocus,
                vedinvedprocess, vedprocessproc,

                wved_do_interrupt_trap, wved_mishap_reset,

                ved_pr_exception, veddointerrupt, vedsetnormalmessage,
                vedprocess_try_input, vededit, vedopen, vedhelpdefaults,
                vedexit, vedprocess, ved_get_line_filename,
                veddocommand, ved_obey, ved, ved_ved,
                handlepopescape, \^[
            ;

vars procedure (
    vedveddefaults  = identfn,
    vedinitfile     = identfn,
    vedprocesstrap  = identfn,
    vedcommandtrap  = identfn,
    vededitortrap   = identfn,
    vedinterrupt    = veddointerrupt,
    vededitorexit   = identfn,

    wved_do_interrupt_trap  = identfn,
    wved_mishap_reset       = identfn,      ;;; only used in errors.p
    );

vars
    vedediting      = false,
    vedcommand      = nullstring,   ;;; command line read in
    vedargument     = nullstring,   ;;; the argument to an assist procedure
    vedmessage      = nullstring,
    vedlastcommand  = identfn,
    veduserdocommand= false,

    vedrefreshneeded= false,        ;;; re-do whole screen, for terminals which can't easily sroll, etc.
    wvedwindowchanged = false,

    vedexpandchar = false,
    vedsearchlist = [],

    vedinputfocus   = false,
    ;

protected vars
    ;;; set true in -vedprocess- and therefore equivalent to
    ;;; iscaller(vedprocess). NOT TO BE ALTERED ANYWHERE ELSE!
    vedinvedprocess = false,
    vedprocessproc  = false,
    ;

lconstant macro SR_WEAK = [weakref[syssave]];


    /*  This version is used for library files, e.g. help, showlib
    */
define vedhelpdefaults();
    vedveddefaults();
    false ->> vedcompileable -> vedwriteable;
    ;;; So that prompt chars get converted to spaces when copied out
    `\Sp` -> vvedpromptchar
enddefine;


define Shallow_pr(item);
    ;;; for printing long data structures in a shortened form in error messages
    lvars item;
    dlocal pop_pr_level = 20;
    syspr(item);
enddefine;

    ;;; Made pop_pr_exception in Ved, unless user has set
    ;;; pop_default_pr_exception
define ved_pr_exception();
    lvars namestr = vednamestring, line = vedline;
    dlocal pr = Shallow_pr;
    sys_pr_exception();         ;;; can change the current file
    if ved_current_file and namestr /= nullstring then
        printf(line, namestr, ';;; %S, ON LINE %P\n')
    endif
enddefine;

define veddointerrupt();
    vedclearinput();
    clearstack();
    if USEWINDOWS then wved_do_interrupt_trap() endif;
    vedscreenovermode();
    pop_default_section -> current_section;
    exitto(vedprocess);
enddefine;

    /*  Set normal message at end of each Ved loop (XVed redefines this).
    */
define vars vedsetnormalmessage(flush);
    lvars flush;
    vedsetstatus(
        if ved_on_status then
            nullstring, false
        elseif datalength(vedmessage) == 0 then
            ;;; no message to print
            ;;; ensure status line redrawn if line has changed
            if USEWINDOWS and not(vedfileprops) then
                nullstring, false
            else
                vednamestring, true
            endif
        else
            vedmessage, true
        endif,
        "undef");   ;;; undef = redraw but don't flush output
    vedsetcursor();
    if flush then vedscr_flush_output() endif;
enddefine;


define lconstant Make_vedprocessproc();
    lvars proc = consproc(0, vedprocess);
    proc!PS_PERM_FLAGS _biset _:M_PS_SYSTEM -> proc!PS_PERM_FLAGS;
    proc -> vedprocessproc
enddefine;

    /*  Called from Xpt_pause and Xpt_read_wait in xt_event.p.
        N.B. It's essential that no interrupts get service between
        leaving this procedure and returning to Xpt_pause/read_wait
        --- otherwise ved events could get raised but not dealt with
        before waiting.
    */
define vedprocess_try_input();
    returnif(not(XWINDOWS) or vedinvedprocess);
    while vedinputwaiting() do
        unless isliveprocess(vedprocessproc) then
            vedsetup();
            Make_vedprocessproc()
        endunless;
        runproc(0, vedprocessproc)
    endwhile
enddefine;

define lconstant Try_get_file(filearg, defaults_p, setonscreen);
    lvars   filearg, defaults_p, new, file, list, setonscreen, string,
            vec = false;
    if isref(filearg) then
        fast_cont(filearg) -> vec;
        subscrv(1,vec)
    else
        filearg
    endif -> file;
    if islist(file) then hd(file) else file endif -> string;
    if isdevice(string) then device_open_name(string) -> string endif;
    if vedpresent(string) ->> file then
        false -> string
    else
        vedreadfile(filearg, defaults_p, setonscreen) -> (new, file);
        vec and datalength(vec) fi_>= 3 and subscrv(3,vec) -> string;
        if new and setonscreen and not(string) then
            if file(VF_DIRECTORY) then '\{b}new file '
            else '\{b}new buffer '
            endif <> file(VF_NAME) -> string
        endif
    endif;
    if setonscreen then
        chain(file, string, vedsetonscreen)
    else
        file
    endif
enddefine;

    /*  Run vedprocess in non-XVed
    */
define lconstant Run_vedprocess() with_props vededit;

    define lconstant Reset_screen();
        false -> ved_on_status;
        Set_basewindow("ved_pop");
        vededitorexit();    ;;; user definable
    enddefine;

    dlocal  vedupperfile    = false,
            vedlowerfile    = false,
            vedinputfocus   = false,
            vedediting      = true,
            0 %, Reset_screen() %,
        ;

    false -> vedprintingdone;
    ;;; Go into raw mode, and set up keypad (if possible)
    vedscreenraw();
    false -> vedrefreshneeded;
    0 -> vedscreencharmode;
    vededitortrap();

    runproc(0, vedprocessproc)
enddefine;

    /*  Main entry point for Ved.
    */
define vededit(filearg);
    lvars   filearg, setfocus = true, defaults_p = vedveddefaults, curr;
    dlocal  vedinputfocus, vedwarpcontext, popgctrace, popmemlim = false;;

    if isboolean(filearg) then
        ;;; optional boolean arg says whether to set vedinputfocus
        ((), filearg) -> (filearg, setfocus)
    endif;
    if isprocedure(filearg) then
        ;;; optional defaults procedure
        ((), filearg) -> (filearg, defaults_p)
    endif;

    unless vedsetupdone then vedsetup() endunless;
    if not(vedinvedprocess) and setfocus then
        ;;; need to get inside vedprocess (except for an output only file)
        false -> ved_on_status;
        nullstring -> vedcommand;
        ;;; first action for vedprocess is to get the file
        vededit(%filearg, defaults_p, setfocus%) :: ved_char_in_stream
                                                -> ved_char_in_stream;
        unless isliveprocess(vedprocessproc) then
            Make_vedprocessproc()
        endunless;
        if XWINDOWS then
            chain(0, vedprocessproc, runproc)
        else
            chain(Run_vedprocess)
        endif
    endif;

    false -> popgctrace;
    unless setfocus then
        false -> vedwarpcontext
    elseif wved_should_warp_mouse("ved") then
        ;;; force input focus to new file
        true -> wvedwindowchanged
    endunless;

    vedcurrentfile -> curr;
    if vedbufferlist == [] or fast_front(vedbufferlist) /== curr
    or (curr /== vedupperfile and curr /== vedlowerfile)
    then
        false -> curr
    endif;

    if Isvedfile(filearg) then      ;;; mishaps if vector but not a file
        if filearg == curr then
            if vedprintingdone then vedrestorescreen() endif
        else
            vedsetonscreen(filearg, false)
        endif
    elseif filearg = vedpathname and vedcurrentfile then
        if curr then
            if vedprintingdone then vedrestorescreen() endif
        else
            vedsetonscreen(vedcurrentfile, false)
        endif
    else
        Try_get_file(filearg, defaults_p, true)
    endif;

    if setfocus then
        chain(procedure; vedcurrentfile -> vedinputfocus endprocedure)
    endif
enddefine;

    /*  Open a file without setting on screen or (necessarily) adding
        it to vedbufferlist.
    */
define vedopen(filearg) -> file;
    lvars   list, defaults_p = vedveddefaults, filearg, file,
            add_to_list = false;
    dlocal  popgctrace = false, popmemlim = false;

    if isboolean(filearg) then
        ((), filearg) -> (filearg, add_to_list)
    endif;
    if isprocedure(filearg) then
        (/*filearg*/), filearg -> (filearg, defaults_p)
    endif;

    if Isvedfile(filearg) then
        filearg
    elseif filearg = vedpathname and vedcurrentfile then
        vedcurrentfile
    else
        Try_get_file(filearg, defaults_p, false)
    endif -> file;

    vedbufferlist -> list;
    if add_to_list and not(fast_lmember(file, list)) then
        if list == [] then
            [^file] -> vedbufferlist
        else
            until back(list) == [] do back(list) -> list enduntil;
            [^file] -> back(list)
        endif
    endif
enddefine;

;;; -----------------------------------------------------------------------

/*  vedexit(pdr)
    Exit Ved (by leaving vededit) and run pdr
    One of the main exit points for VED
    (the others are Quit_file and vedqget)

    Note that this will run Reset_screen in Run_vedprocess
    (unless XWINDOWS)
*/

define vedexit(pdr);
    lvars pdr;

    unless vedinvedprocess then chain(pdr) endunless;

    ;;; In the context where this is called vedcurrentfile may not be
    ;;; properly dlocalised as ved_current_file (due to old user code,
    ;;; not system code), so this makes sure that the globals are saved
    ;;; properly
    ved_save_file_globals();

    chainto(pdr, vedprocess,
        procedure(pdr);
            lvars pdr;

            ;;; For the same reason as above, now force setting of the globals
            ved_current_file;
            false -> ved_current_file;
            () -> ved_current_file;

            if XWINDOWS then
                Set_basewindow("ved_pop");
                chainfrom(vedprocess, pdr)
            else
                chainfrom(Run_vedprocess, pdr)
            endif
        endprocedure)
enddefine;

    /*  This is always run as a process
    */
define vedprocess();
    lvars   file, p, list;
    dlocal  vedinvedprocess = true,
            vedediting      = true,
            interrupt       = vedinterrupt,
            pop_compiler_subsystem = false,
            subsystem,      ;;; needs to be reset to what it was
            prmishap,
            pop_pr_exception,
            pop_exception_final = sys_exception_final,
            SR_WEAK pop_after_restore,
            pop_chario_trap,
            poplineprefix = false,
            ;;; use these rather than the pop_ active ones to save a bit
            ;;; of time suspending/resuming
            charin_dev, charout_dev, charerr_dev,
        ;

    define dlocal keyboard_interrupt(); interrupt() enddefine;

    define lconstant show_stack();
        lvars stack, _len = stacklength(), veddev;
        consvector(_len) -> stack;
        appdata(stack, procedure();
                            dlocal cucharout = identfn;
                            spr()
                       endprocedure);
        if vedmessage = nullstring then -> endif;  ;;; erase last space
        explode(vedmessage);
        stacklength() -> _len;
        if not(vedprintingdone)
        and (_len fi_+ datalength(vedcommand) fi_+ 14) fi_< vedscreenwidth
        then
            vedputmessage(consdstring(_len))
        else
            erasenum(_len);
            charout_dev!D_FLAGS _bitst _M_D_USER_DEV -> veddev;
            if XWINDOWS then
                unless veddev then vedscreenbell() endunless;
                ;;; try to make window visible
                wved_raise_window(charout_dev)
            endif;
            cucharout(`\n`);
            unless veddev then printf(vedmessage, '%p\n') endunless;
            pretty(stack);      ;;; pretty printer
            vedcurrentfile -> vedinputfocus
        endif
    enddefine;

    if vedcurrentfile then
        ;;; get previous version of subsystem , to prevent it being clobbered
        vedcurrentfile(VF_SUBSYSTEM) -> subsystem
    endif;

    ;;; Re-assign pop_pr_exception only if user doesn't have default set
    unless isundef(pop_default_prmishap ->> p) or p == identfn then
        ;;; Backward compatibility for prmishap
        p -> prmishap
    endunless;
    unless isundef(pop_default_pr_exception ->> p) then
        p
    else
        ved_pr_exception
    endunless -> pop_pr_exception;

    lconstant macro CHARIO_FILE = [weakref[ved_chario_file] Im$-chario_file];

    if testdef ved_chario_file and CHARIO_FILE then
        (CHARIO_FILE, false) -> (weakref ved_chario_file, CHARIO_FILE)
    endif;

    unless XWINDOWS then

        define lconstant chario_trap(dev, is_input) -> (dev, is_input);
            lvars dev, is_input;
            unless vedprintingdone or dev!D_FLAGS _bitst _M_D_USER_DEV then
                vedscreenreset()
            endunless
        enddefine;

        false -> vedprintingdone;
        if testdef syssave then
            procedure();
                unless vedprintingdone then vedrestorewindows() endunless
            endprocedure
                <> SR_WEAK pop_after_restore -> SR_WEAK pop_after_restore
        endif;
        ;;; -pop_chario_trap- is called by charin/out/err before doing anything
        chario_trap <> pop_chario_trap -> pop_chario_trap
    endunless;


    ;;; Main loop

    repeat

        vedprocesstrap();   ;;; user definable

        if wvedwindowchanged then
            if USEWINDOWS and vedwarpcontext then
                wved_set_input_focus(wvedwindow)
            endif;
            false -> wvedwindowchanged
        endif;

        ;;; reads input -- for XWINDOWS, it suspends the process if no
        ;;; input is waiting
        vedprocesschar();

        ;;; sort out which is the 'current' file
        if (vedbufferlist ->> list) == [] then
            MAXSCREENCOL -> screenline_refresh_col;
            true -> screenline_tail_isclear;
            nextif(vedinputwaiting());
            suspend_chain(0, vedprocessproc, identfn);
            nextloop
        endif;
        fast_front(list) -> file;
        if vedinputfocus /== file then
            if vedinputfocus then
                vedinputfocus -> file
            else
                file -> vedinputfocus
            endif
        endif;
        ;;; file is now the one we want to be 'current'
        if vedprintingdone then
            file -> ved_current_file
        elseif fast_front(list) /== file then
            vedsetonscreen(file, false)
        elseif ved_current_file /== file then
            if (fast_back(list) ->> list) /== [] then
                ;;; make file not be first in vedbufferlist so the second
                ;;; will get 'tidied' by vedsetonscreen
                fast_front(list) -> fast_front(vedbufferlist);
                file -> fast_front(list)
            endif;
            vedsetonscreen(file, false)
        endif;

        ;;; If last command left anything on the stack, show it
        unless ved_on_status or stacklength() == 0 then
            show_stack()
        endunless;

        ;;; ensure window is showing what it should
        if vedprintingdone then vedrestorescreen() endif;
        unless vedinputwaiting() then vedcheck() endunless;
        Clear_temp_mark();
        vedsetnormalmessage(false);     ;;; false = don't flush output

        nullstring ->> vedargument -> vedmessage;
        MAXSCREENCOL -> screenline_refresh_col;
        true -> screenline_tail_isclear;

        _CHECKINTERRUPT
    endrepeat
enddefine;      /* vedprocess */



;;; --- COMMAND INTERPRETATION -------------------------------------------

define ved_get_line_filename(terminators, trailers);
    lvars c, i, n, line, terminators, trailers;
    vedthisline() -> line;
    unless (Skipwhite(vedcolumn, line) ->> i) then
        vederror('\{b}no filename on line')
    endunless;
    0 -> n;
    fast_for i from i to vvedlinesize do
        fast_subscrs(i, line) -> c;
        quitif(strmember(c, terminators));
        c;
        n fi_+ 1 -> n
    endfor;
    if (n fi_> 0) and strmember(dup(), trailers) then
        ->;
        n fi_- 1 -> n
    endif;
    if n == 0 then vederror('\{b}no filename') endif;
    consstring(n)
enddefine;

define lconstant Otherfile();
    ;;; for ^# -- get "other" file name
    if back(vedbufferlist) == [] then
        vederror('\{b}vedexpandchar: no other Ved file')
    else
        vedbufferlist(2)(VF_PATHNAME)   ;;; name of 'other' file
    endif
enddefine;

define lconstant Endofline();
    ;;; everything to right of cursor
    if vedcolumn fi_> vvedlinesize then
        nullstring
    else
        subdstring(vedcolumn,vvedlinesize fi_- vedcolumn fi_+ 1, vedthisline())
    endif
enddefine;


define vedexpandchars = newproperty(
    [
     [`%` ^(procedure(); vedpathname endprocedure)]
     [`p` ^(procedure(); sys_fname_path(vedpathname) endprocedure)]
     [`#` ^Otherfile]
     [`e` ^Endofline]
     [`w` ^vednextitem]
     [`l` ^vedthisline]
     [`f` ^(ved_get_line_filename(% '\s\t\n\')"', '.,' %))]
    ], 10, false, "perm")
enddefine;


define veddocommand();
    lvars   action, p, _index, _size, _char, name,
            _last, _type, _n, _hasupper, from_veddo = false;
    dlocal  vedargument;

    define lconstant Expand(string);
        lvars i, c, item, string, len, found = false;
        vedtrimline();
        datalength(string) -> len;
        #|  fast_for i to len do
                fast_subscrs(i, string) ->> c;
                if c == vedexpandchar and i /== len then
                    fast_subscrs(i fi_+ 1 ->> i, string) ->> c;
                    if c == vedexpandchar then
                        true -> found;
                        ->;
                    elseif vedexpandchars(c) ->> item then
                        true -> found;
                        -> (,);
                        if isstring(item) then
                            explode(item)
                        else
                            if isword(item) then
                                valof(item)()
                            else
                                item()
                            endif;
                            dest_characters()
                        endif
                    endif
                endif
            endfor
        |#;
        if found then
            consdstring()
        else
            erasenum();
            false       ;;; no reason to change command
        endif
    enddefine;      /* Expand */

    if isref(vedcommand) then
        ;;; Command in a ref means don't muck around with the status line etc
        ;;; (used by veddo)
        fast_cont(vedcommand) -> vedcommand;
        true -> from_veddo
    endif;

    unless from_veddo then
        unless vedprintingdone then
            vedsetstatus(consdstring(vedscreenstatus_-|_mark,1), false, true)
        endunless;
        nullstring -> vedmessage
    endunless;

    ;;; expand special characters
    if vedexpandchar and strmember(vedexpandchar, vedcommand)
    and Expand(vedcommand) ->> action then
        action -> vedcommand;
        unless from_veddo then
            vedputmessage(vedcommand);
            not(ved_on_status) -> ved_on_status;
            unless vedline > 1 and vedcommand = Buffer_line(vedline - 1) then
                ;;; put a copy of translated command in command line buffer
                vedlineabove();
                vedcommand -> vedthisline();
                vednextline();
            endunless;
            not(ved_on_status) -> ved_on_status;
            nullstring -> vedmessage
        endunless
    endif;

    Trimwhite(vedcommand, datalength(vedcommand)) -> (_size, vedcommand);
    lconstant nc_mess = '\{b}no command';
    if _size == 0 then vederror(nc_mess) endif;
    ;;; Should here convert things like \^A to CTRL-A ?

    ;;; For procedures which use the whole command string
    copy(vedcommand) -> vedargument;

    ;;; deal with possible trap procedures -- reprocess command string
    unless from_veddo then
        lvars oldcommand = vedcommand;
        vedcommandtrap();
        unless vedcommand = oldcommand then
            Trimwhite(vedcommand, datalength(vedcommand)) -> (_size, vedcommand);
            if _size == 0 then vederror(nc_mess) endif;
            copy(vedcommand) -> vedargument
        endunless
    endunless;

    1 -> _index;
    fast_subscrs(1,vedcommand) -> _char;

    if strmember(_char, '/"') then
        ;;; search forward `/` for embedded, `"` non-embedded string
        ved_search();
        return
    elseif _char == `\\` or _char == `\`` then
        ;;; search backward, `\\` for embedded, `\`` non-embedded string
        ved_backsearch();
        return
#_IF DEF VMS
    elseif _char == `$` then
#_ELSE
    elseif _char == `%` or _char == `$` or _char == `!` then
#_ENDIF
        ved_obey();
        return
    elseif _char == `:` then
        if testdef vedcompilevedargument then
            weakref[pop11_compile] vedcompilevedargument()
        else
            vederror('\{b}Pop-11 compiler not loaded')
        endif;
        return
    elseif isnumbercode(_char) then
        ved_moveto();
        return
    elseif _char == `@` then
        substring(2, _size fi_- 1, vedcommand) -> vedargument;
        ved_moveto();
        return
    else
        _char,
        vedchartype(_char) -> _last;
        while (_index fi_+ 1 ->> _index) fi_<= _size
        and ((vedchartype(fast_subscrs(_index,vedcommand) ->> _char)
                ->> _type) == _last
                    or (_last == `a` and _type == `0`)
                    or _last == `_` or _type == `_`)
        do
            isuppercode(dup(_char)) or _hasupper -> _hasupper;
        endwhile;
        consstring(_index fi_- 1)
    endif -> action;

    ;;; get rid of leading spaces in the argument string
    Skipwhite(_index, vedcommand) -> _index;

    if _index then
        subdstring(_index, _size fi_- _index fi_+ 1, vedcommand)
    else
        nullstring
    endif -> vedargument;

    define lconstant Is_command(name);
        lvars name, p;
        if identprops(name) == 0 and not(isactive(name))
        and isprocedure(valof(name) ->> p) then
            p
        else
            false
        endif
    enddefine;

    ;;; if attatching 'ved_' to the action gives a procedure name, apply it
    ;;; otherwise try autoloading
    ;;; otherwise try lowercase-converted name
    ;;; otherwise try autoloading converted name (unless VMS)

    consword('ved_' sys_>< action) -> name;
    if testdef sys_autoload then weakref sys_autoload(name) -> endif;
    if not(Is_command(name) ->> p) and _hasupper then
        ;;; try mapping it to lower case
        uppertolower(name) -> name;
#_IF not(DEF VMS or DEF WIN32)
        ;;; try autoload again where case is distinct in filenames
        if testdef sys_autoload then weakref sys_autoload(name) -> endif;
#_ENDIF
        Is_command(name) -> p
    endif;
    if p then
        if isprocedure(veduserdocommand) then
            veduserdocommand(true, p)
        else
            p()
        endif;
        p -> vedlastcommand
    else
        vederror('\{b}unknown command')
    endif
enddefine;

define vars ved_obey();
    ;;; called for '$' or '%' commands. Strip off first character
    lvars char = vedargument(1);        ;;; says which shell required
    allbutfirst(1,vedargument) -> vedargument;
    if XWINDOWS then
        sysobey(vedargument, char)
    else
        vedscreenreset();
        sysobey(vedargument, char);
        vedrestorescreen()
    endif
enddefine;


;;; --- COMMAND FOR CALLING EDITOR DIRECT FROM POP11 ---------------------

define vars ved_ved();
    lvars arg, args;

    define lconstant next_file(arg);
        lvars arg;
        arg -> vedargument;
        chain(ved_ved)
    enddefine;

    vedsetup();

#_IF DEF WIN32
    ;;; filenames can include spaces, so treat vedargument as a single
    ;;; file (and not as a workbuff -- see Is_file_name in "vdfiles.p")
    if strmember(`\s`, vedargument)
    and sys_fname_path(vedargument) = nullstring
    then
        '.\\' dir_>< vedargument -> vedargument;
    endif;
#_ELSE
    ;;; assume no spaces in filenames
    [% sys_parse_string(vedargument) %] -> args;
    if args /== [] then
        [% fast_for arg in tl(args) do next_file(%arg%) endfor %]
                    nc_<> ved_char_in_stream -> ved_char_in_stream;
        hd(args);
        sys_grbg_list(args)
    else
        nullstring
    endif -> vedargument;
#_ENDIF

    ;;; only use vedsearchlist if not full path name, and not logical variable
    Searchlist_name(sysfileok(vedargument)) -> arg;
    if arg = nullstring then
        if vedvedname = nullstring then
            'temp' <> pop_default_type -> vedvedname
        endif;
        vedvedname -> arg
    else
        if islist(arg) then
            hd(arg)
        else
            arg
        endif -> vedvedname
    endif;
    chain(arg, vededit)
enddefine;

define vars syntax ved;
    readstringline() -> vedargument;
    ";" :: proglist -> proglist;
    vedputcommand('ved ' <> vedargument);
    chain(ved_ved)
enddefine;

    ;;; define <ESC> as a syntax word to print a warning.
define vars handlepopescape();
    printf('\n\^GEdit keys don\'t work outside VED');
    interrupt()
enddefine;

define macro \^[ ;      ;;; <esc>
    handlepopescape()
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov  1 1996
        Changed ved_ved for Win32 so that it allows filenames with spaces
--- John Gibson, Mar 12 1996
        Undid last change -- vvedpromptchar is now a standard local of every
        file.
--- John Gibson, Mar 11 1996
        vedhelpdefaults also localises vvedpromptchar to the current file
        before setting it to `\Sp`.
--- John Gibson, Mar  5 1996
        Added dlocal poplineprefix = false in vedprocess.
--- John Gibson, Feb 21 1996
        Fixed ved_pr_exception to use %S instead of %P for printf
--- John Gibson, Feb 10 1996
        Replaced vedpr*mishap with ved_pr_exception, and added code for
        pop_default_pr_exception etc in vedprocess.
--- John Williams, Dec 20 1995
        vedhelpdefaults sets vvedpromptchar to `\Sp` so that prompt
        characters get converted to spaces when text is copied out of
        a documentation file.
--- John Gibson, Nov  4 1995
        Exported Get*_line_filename as ved_get_line_filename
--- John Gibson, Oct  5 1995
        Set popmemlim locally false inside vededit and vedopen
--- John Gibson, Jan 28 1995
        Changed veddocommand so that it doesn't muck around with the status
        line etc if vedcommand is in a reference (used by veddo)
--- Robert John Duncan, Sep  5 1994
        Minor modification for Win32
--- John Gibson, Apr 29 1994
        Made Get*_line_filename give an error if result string is empty
--- John Gibson, Mar  7 1994
        Replaced ved*editor, ved*select and ved*getfile with new vededit.
--- John Gibson, Nov 24 1993
        Made ved_ved deal with multiple files by splitting vedargument at
        whitespaces
--- John Gibson, Jun 16 1993
        Weakened references to sys_autoload
--- John Gibson, May 27 1993
        Corrected way in which vedexit does its forced assignment to
        ved_current_file.
--- John Williams, Apr 20 1993
        Added new vedexpandchars entry for `p` (get pathname of current file)
--- John Gibson, Mar 21 1993
        Fixed show_stack so it raises the output window when result
        doesn't fit on command line.
--- John Williams, Jan 22 1993
        vedpr*mishap uses %s to print vednamestring (to avoid problems with pop_pr_quotes)
--- John Gibson, Jan 12 1993
        popcom*piler -> subsystem etc
--- John Gibson, Dec 21 1992
        Moved ved_chario_file to vddevio.p
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Mar 17 1992
        Made updater of -ved_chario_file- assign false to vedprocessproc
        if not inside it (so that it gets reconstructed the next time in
        and takes account of the new chario file).
--- John Gibson, Jan 22 1992
        Fixed -show_stack- so as not to print vedmessage into ved file
--- John Gibson, Dec 20 1991
        Replaced vedscreeng*raphoff() with 0 -> vedscreencharmode
--- John Gibson, Dec  2 1991
        Fixed -vedprocess_try_input- to keep checking after running
        ved that no firther events are waiting to be processed.
--- John Gibson, Aug 29 1991
        Renamed Xved_try_input as exported vedprocess_try_input
--- John Gibson, Jun 27 1991
        More changes to vedprocess
--- John Gibson, Jun 25 1991
        Added -ved_chario_file-
--- John Gibson, Jun 19 1991
        Added -vedsetnormalmessage-
--- John Gibson, Jun 13 1991
        Cleaned up some code in -veddocommand-
--- John Gibson, Jun 12 1991
        Rewrote -ved*editor- to run -vedprocess- as a process (-vedprocessproc-)
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Jun  4 1991
        Change to -vedpr*mishap-
--- John Gibson, Jun  3 1991
        Change to -vedprocess- to ensure window input focus set before
        quitting in XWINDOWS case
--- John Gibson, Apr  9 1991
        Rewrote -vedprocess- and -ved*editor- for xved
--- John Gibson, Apr  5 1991
        Added -vedinvedprocess- to say iscaller(vedprocess).
--- John Gibson, Mar 30 1991
        Stopped -vedprintingdone- being set true when using X windows.
--- Aaron Sloman, Mar  4 1991
    Prevented input warping on entry to and exit from VED
--- John Gibson, Jan 30 1991
        vedprocess gives "undef" arg to vedsetstatus to stop it flushing
        output (gets flushed in vedinascii)
--- Aaron Sloman, Jan 14 1991
    stopped veddointerrupt calling wved_set_input_focus
--- John Gibson, Jan  8 1991
        Removed test for -vedediting- from -chario_trap- in -vedprocess-.
--- John Gibson, Nov 28 1990
        Exported -vedinputfocus-
--- John Gibson, Nov 16 1990
        Changed vedprocess to add chario trap procedure to new variable
        pop_chario_trap
--- John Gibson, Nov  9 1990
        Tidied up ved and ved_ved
--- John Gibson, Nov  2 1990
        Moved -vedpopexit- to vdquit.p.
        Uses -Searchlist_name-
--- Aaron Sloman, Oct 31 1990
        exported vedexpandchars
--- Aaron Sloman, Oct 31 1990
        Added vedlastcommand, veduserdocommand
--- John Gibson, Oct 31 1990
        Whole file into section Sys$-Ved; moved some things to vdfiles.p,
        made some things lconstants, etc.
        Made -ved- and <esc> syntax words rather than macros.
        Added -vedinputfocus- (not exported).
--- John Gibson, Oct 22 1990
        Removed unwanted assignments in -vedpr*mishap-
--- Aaron Sloman, Oct 13 1990
        Made pop_default_pr*mishap procedure type variable
--- Aaron Sloman, Oct 12 1990
        Replaced xved with wved.
        Let pop_default_pr*mishap control vedpr*mishap
--- Aaron Sloman, Oct  8 1990
        fixed typo
--- Aaron Sloman, Oct  8 1990
        Replaced calls of XVED_LOADED.
--- John Gibson, Sep 28 1990
        Removed local settings of -interrupt- and -pr*mishap- from
        -vedpr*mishap-, also local setting of -interrupt- inside
        -veddointerrupt-.
--- Aaron Sloman, Sep 28 1990
        Put interrupt check in vedpr*mishap before making interrupt setpop
        Added vededitortrap (at request of Steve Knight)
        Slightly simplified calls to xved stuff, by using generalised
        lower level procedures that can cope with pw*m or xved
        Made veddointerrupt call wved_set_input_focus
--- Aaron Sloman, Sep 23 1990
        Added vededitorexit,
--- Aaron Sloman, Aug 10 1990
        made vedscreenreset a vars
--- Aaron Sloman, Jul 15 1990
        Made Reset_screen print a newline after vedscreenreset()
--- Aaron Sloman, Jul 10 1990
        Removed the code that prevented pointer being warped if you type
        ahead. (It still sometimes reverts to the original window if you
        type ahead after asking VED for a new file. Timing problems.)
--- Aaron Sloman, Jul 10 1990
        Gave vedprocess the task of warping cursor to new VED window if
            -wvedwindowchanged- is true, etc.
        Ensured that PW*M switches to base window whenever -ved*editor- exits
            if Vedpw*mw*arping("ved_pop") is true.
        Re-organised -ved*editor-, including "exit" actions (now using
        -dlocal-
--- Aaron Sloman and John Williams, Jul  6 1990
        Introduced -vedexit- as standard way to leave -ved*editor- cleanly
        Moved in -ved_pop- from vdcompile.p, and defined it in terms of
            -vedexit-
        Precautions taken to ensure the -popcom*piler- is restored on exit
        from VED, as required by lisp.
--- Aaron Sloman, Jul  3 1990
        ved*editor made a "vars".
        Restructured ved*editor completely, so that actions in the "top level"
        call happen in a sub-procedure with its own dlocals.
--- John Gibson, Jun 27 1990
        Guarded dlocal -vedscreenreset- in -ved*editor- with test for
        abnormal exit, and changed chain of -ved*getfile- to call (since
        dlocal -vedscreenreset- gets run otherwise)
--- Aaron Sloman, Jun 26 1990
    Made ved*editor run vedscreenreset on exit, no matter what.
--- John Williams, Mar  5 1990
        Improved -Get*_line_filename-
--- John Williams, Jan 19 1990
        -ved*editor- now calls -vedsetup-, not -vedtermsetup-
--- John Gibson, Aug 22 1989
        Replaced use of -pop_exit_ok- in -vedpopexit- with test for
        whether -popdevin- is a terminal.
--- John Gibson, Aug  3 1989
        Rewrote code for autoloading ved commands
--- John Williams, Jul 18 1989
        -ved*editor- now localises -popcom*piler- (to stop VED clobbering it)
--- Aaron Sloman, May 21 1989
    changed vedprocess so that it adds to but doesn't clobber pop_after_restore
--- Aaron Sloman, Apr 14 1989
    Changed Shallow_pr to use pop_pr_level instead of trying to be clever
--- Aaron Sloman, Apr 14 1989
    Fixed bug in Shallow_pr -- failed on non-list pairs
--- Roger Evans, Mar  7 1989
        Move IO redirection (etc) when pop_exit_ok false from
        vedpopexit to sysexit (in sysutil.p)
--- John Gibson, Feb 19 1989
        Included io.ph
--- Roger Evans, Nov 17 1988
        made vedpopexit sensitive to pop_exit_ok
--- Aaron Sloman, Aug 28 1988
        Fixed ^f in Get*_line_filename to ignore final . or ","
--- Aaron Sloman, Jul 13 1988
        Tidied up a bit
--- Aaron Sloman, Jul  9 1988
        Introduced Trimwhite and slightly tidied up
        Changed ved_backsearch, for backward search with patterns
        and also to distinguish <ENTER> ` and <ENTER> \
            See vdsearch.p
--- John Gibson, Mar 25 1988
        -pop_default_section- now an active variable
--- John Gibson, Feb 15 1988
        Added local -chario_trap- to -vedprocess- (replaces ved tests in
        -charin-, etc).
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
*/
