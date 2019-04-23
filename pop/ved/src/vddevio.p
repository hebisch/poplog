/* --- Copyright University of Sussex 2009. All rights reserved. ----------
 > File:            C.all/ved/src/vddevio.p
 > Purpose:         Ved files as I/O devices
 > Author:          John Gibson (see revisions)
*/

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../src/process.ph'

constant
        procedure (vedinsertstring, vedcursorset, vedopen,
        ved_file_change_apply, Sys$-Ved$-Redraw_status_head
        )
    ;

vars
        procedure (ved_t, ved_apply_action),
        vedprocwait, vedprocessproc,
    ;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved$-Im =>
                        vedcrinteractive,
                        vedprocswaiting, consveddevice, isveddevice,
                        vedrangerepeater, ved_end_im, ved_switchmode_im,
                        ved_chario_file
                    ;

vars
    vedcrinteractive    = true,
    chario_file         = false,    ;;; used by ved_chario_file

    localise_outdev_files = true,
    ;

lconstant macro (
    ;;; subscripts for ved device user data
    VDD_IDENTIFIER      = 1,
    VDD_INSERT_AT_EOF   = 2,
    VDD_INPUT           = 3,
    VDD_OUT_BUFF        = 4,
    VDD_OUT_POS         = 5,
    VDD_AUX_OUT_BUFF    = 6,
    VDD_AUX_OUT_POS     = 7,
    VDD_LEN             = 7,

    ;;; subscripts for waiting process record
    WP_PROMPT           = 1,
    WP_DEVICE           = 2,
    WP_PROCESS          = 3,
    WP_RESTORE_FILE     = 4,
    );

lconstant
    veddevice_identifier = 'veddevice',
    ;


;;; --- INPUT CHARACTER REPEATER --------------------------------------

define lconstant Stringsin(statepair);
    ;;; statepair contains a subscript and a list of strings.
    lvars statepair, string, strings, _i;
    fast_destpair(statepair) -> strings -> _i;
    if null(strings) then
        termin
    else
        fast_front(strings) -> string;
        if _i fi_> _pint(string!V_LENGTH) then
            ;;; end of that line, prepare for next one
            fast_back(strings) ->  fast_back(statepair);
            1 -> fast_front(statepair);
            `\n`
        else
            fast_subscrs(_i, string);
            if dup() == `\St` then ->, `\s` endif; ;;; trailing space -> space
            _i fi_+ 1 -> fast_front(statepair)
        endif
    endif
enddefine;

define vedrangerepeater(_lo, _hi) -> pdr;
    ;;; used to create character repeater for a range of current buffer
    lvars list, pdr, string, _lo, _hi, _start, _end;

    Check_integer(_lo, 1);
    Check_integer(_hi, 1);
    unless _hi fi_>= _lo then
        mishap(_lo, _hi, 2, 'NUMBERS IN WRONG ORDER')
    endunless;

    ;;; make a list of the lines (trimming tabs and trailing spaces)
    [% fast_for _lo from _lo to _hi do
        _CHECKUSER;
        veddecodetabs(Buffer_line(_lo)) -> string;
        vedusedsize(string) -> _end;
        unless (locchar_back(`\Sp`, _end, string) ->> _start) then
            0 -> _start;
        endunless;
        if _start /== 0 or _end /== _pint(string!V_LENGTH) then
            substring(_start fi_+ 1, _end fi_- _start, string);
        else
            string;
        endif;
    endfast_for %] -> list;

    Stringsin(% conspair(1, list) %) -> pdr;
    "vedrangerepeater" -> pdprops(pdr)
enddefine;


;;; --- PROCESS MECHANISM INSIDE A VED DEVICE ----------------------------

lvars
    running_process     = false,
    restore_focus       = false,
    blocked_output_devs = false,
    ;

define lconstant vdd_subscr = device_user_data <> fast_subscrv enddefine;

define vedprocswaiting();
    lvars wait_proc;
    while vedprocwait /== [] do
        hd(vedprocwait) -> wait_proc;
        returnif(isliveprocess(wait_proc(WP_PROCESS))) (true);
        tl(vedprocwait) -> vedprocwait
    endwhile;
    false
enddefine;

define lconstant Check_procs_waiting();
    unless vedprocswaiting() then
        vederror('\{b}not waiting for input')
    endunless
enddefine;

define lconstant Get_prompt() -> prompt;
    lvars prompt, len;
    hd(vedprocwait)(WP_PROMPT) -> prompt;
    datalength(prompt) -> len;
    if len == 0 then
        inits(1 ->> len) -> prompt;
    elseif prompt(len) == `\s` then
        copy(prompt) -> prompt
    else
        len fi_+ 1 -> len;
        prompt <> '\s' -> prompt
    endif;
    vvedpromptchar -> prompt(len)
enddefine;

define lconstant Block_device_output(context);
    lvars context;
    blocked_output_devs and true
enddefine;
;;;
define updaterof Block_device_output(block, context);
    lvars blocked_devs = blocked_output_devs, context;
    if block then
        unless blocked_devs then [] -> blocked_output_devs endunless;
        return
    endif;
    false -> blocked_output_devs;
    returnunless(blocked_devs and blocked_devs /== []);

    define lconstant flush_blocked(blocked_devs, context);
        lvars dev, pos, blocked_devs, context;
        dlocal pop_asts_enabled = false;
        until blocked_devs == [] do
            sys_grbg_destpair(blocked_devs) -> (dev, blocked_devs);
            vdd_subscr(VDD_AUX_OUT_POS,dev) -> pos;
            1 -> vdd_subscr(VDD_AUX_OUT_POS,dev);
            unless context == 2 then
                fast_syswrite(dev, 1, vdd_subscr(VDD_AUX_OUT_BUFF,dev),
                                            pos fi_- 1)
            endunless
        enduntil
    enddefine;

    chain(blocked_devs, context, flush_blocked)
enddefine;

    /*  Construct a user device for a ved file
    */
define consveddevice(file, mode, insert_at_eof) -> dev;
    lvars file, dev, mode, insert_at_eof;

    ;;; flush procedure for a Ved device
    define lconstant Flush(dev);
        lvars   dev, pos = vdd_subscr(VDD_OUT_POS, dev) fi_- 1;
        returnif(pos == 0);

        dlocal  veddelspaces = false, vedautowrite = false,
                popgctrace = false, vedediting,
                % Block_device_output(dlocal_context) %;

        true -> Block_device_output(1);

        if XWINDOWS and not(vedinvedprocess) then true -> vedediting endif;

        define lconstant do_flush(dev, pos);
            lvars   dev, pos, n, char, slen, buf;
            dlocal  ved_on_status = false, vedbreak = false,
                    vvedpromptchar = false;

            vedpathname -> device_full_name(dev);   ;;; ensure full name set

            if vdd_subscr(VDD_INSERT_AT_EOF, dev) then
                unless vedline fi_> vvedbuffersize
                or (vedline == vvedbuffersize and vedcolumn fi_> vvedlinesize)
                then
                    vedendfile()
                endunless
            endif;

            vdd_subscr(VDD_OUT_BUFF, dev) -> buf;
            _CHECKUSER;
            0 -> slen;
            fast_for n to pos do
                fast_subscrs(n, buf) ->> char;  ;;; stack it
                slen fi_+ 1 -> slen;
                nextunless(char == `\n`);
                vedinsertstring((), slen), vedcheck();
                0 -> slen
            endfor;
            unless slen == 0 then
                vedinsertstring((), slen), vedcheck()
            endunless;
            1 -> vdd_subscr(VDD_OUT_POS, dev);
            unless vedinvedprocess then
                vedsetcursor();
                vedscr_flush_output()
            endunless
        enddefine;

        if localise_outdev_files and vedinvedprocess then
            procedure(dev, pos);
                lvars dev, pos;
                dlocal ved_current_file;
                vededit(device_full_name(dev), false);  ;;; don't set vedinputfocus
                do_flush(dev, pos)
            endprocedure(dev, pos)
        else
            vededit(device_full_name(dev), false);  ;;; don't set vedinputfocus
            do_flush(dev, pos)
        endif
    enddefine;

    ;;; write procedure for a Ved device
    define lconstant Write(dev, bsub, userbuf, nbytes);
        lvars   dev, userbuf, bsub, nbytes, pos, char, buf, req;
        lconstant macro BUFFLEN = 128;

        define lconstant nointr_apply();
            dlocal pop_asts_enabled = false;
            fast_apply()
        enddefine;

        if blocked_output_devs then
            unless isinteger(vdd_subscr(VDD_AUX_OUT_POS,dev) ->> pos) then
                1 -> pos
            endunless;
            pos fi_+ nbytes fi_- 1 -> req;
            unless isstring(vdd_subscr(VDD_AUX_OUT_BUFF,dev) ->> buf)
            and datalength(buf) fi_>= req
            then
                nointr_apply(fi_max(req fi_+ req, BUFFLEN), inits)
                        ->> buf -> vdd_subscr(VDD_AUX_OUT_BUFF,dev)
            endunless;
            move_bytes(bsub, userbuf, pos, buf, nbytes);
            pos fi_+ nbytes -> vdd_subscr(VDD_AUX_OUT_POS,dev);
            unless fast_lmember(dev, blocked_output_devs) then
                nointr_apply(dev, blocked_output_devs, conspair)
                        -> blocked_output_devs
            endunless;
            return
        endif;

        unless isstring(vdd_subscr(VDD_OUT_BUFF,dev) ->> buf) then
            nointr_apply(BUFFLEN, inits) ->> buf
                                -> vdd_subscr(VDD_OUT_BUFF,dev)
        endunless;
        vdd_subscr(VDD_OUT_POS,dev) -> pos;
        until nbytes == 0 do
            fast_subscrs(bsub,userbuf) ->> char -> fast_subscrs(pos,buf);
            pos fi_+ 1 -> pos;
            if char == `\n` or pos fi_> BUFFLEN then
                pos -> vdd_subscr(VDD_OUT_POS,dev);
                Flush(dev);
                vdd_subscr(VDD_OUT_POS,dev) -> pos
            endif;
            bsub fi_+ 1 -> bsub;
            nbytes fi_- 1 -> nbytes
        enduntil;
        pos -> vdd_subscr(VDD_OUT_POS,dev);
        unless pos == 1 or pop_buffer_charout then Flush(dev) endunless
    enddefine;

    ;;; read procedure for a Ved device
    define lconstant Read(dev, bsub, userbuf, nbytes);
        lvars   dev, input, userbuf, proc, bsub, n, nbytes, new, _char;
        dlocal  popgctrace = false;

        define lconstant Add_waiting(prompt, dev, proc, new);
            lvars   dev, prompt, proc, new, name = device_full_name(dev);
            dlocal  popgctrace = false;

            define lconstant Add_proc(prompt, dev, proc, restfile);
                lvars   dev, prompt, proc, restfile;
                dlocal  vedautowrite = false, ved_on_status = false;
                if vdd_subscr(VDD_OUT_POS, dev) then Flush(dev) endif;
                vedpathname -> device_full_name(dev);   ;;; ensure full name set
                {% prompt, dev, proc, restfile %} :: vedprocwait
                                                    -> vedprocwait;
                `\Sp` -> vvedpromptchar;

                if vedline fi_< vvedbuffersize
                or (vedline == vvedbuffersize and vedcolumn fi_<= vvedlinesize)
                or locchar_back(vvedpromptchar, vedcolumn, Buffer_line(vedline))
                then
                    if vdd_subscr(VDD_INSERT_AT_EOF, dev) then
                        vedendfile()
                    elseif vedcolumn == 1 then
                        vedlineabove()
                    else
                        vedlinebelow()
                    endif
                endif;
                ;;; insert the prompt in the file
                vedinsertstring(Get_prompt());
                unless vedcrinteractive then
                    ;;; mark the current line if RETURN input disabled
                    Clear_temp_mark();
                    vedmarklo(); vedmarkhi()
                endunless
            enddefine;      /* Add_proc */

            (prompt, dev, proc);
            if new then
                vedinputfocus;
                vededit(name);
                Add_proc()
            else
                ved_file_change_apply((), restore_focus, Add_proc,
                            vedopen(name, true) ->> restore_focus)
            endif
        enddefine;      /* Add_waiting */


        running_process -> proc;
        if proc and isliveprocess(proc) == proc then
            ;;; being run by Pass_input. input can be a character repeater,
            ;;; an identifier or termin
            vdd_subscr(VDD_INPUT, dev) -> input;
            if isident(input) then
                sys_clear_input(dev);
                idval(input)();     ;;; e.g. ident keyboard_interrupt
                chain(dev, bsub, userbuf, nbytes, Read)
            elseif input == termin then
                ;;; eof - return 0 chars read
                sys_clear_input(dev);
                return(0)
            elseif nbytes == 0 then
                return(0)
            endif;
            unless (input() ->> _char) == termin then
                nbytes -> n;
                repeat
                    _char -> fast_subscrs(bsub, userbuf);
                    n fi_- 1 -> n;
                    returnif(n == 0 or _char == `\n`
                             or (input() ->> _char) == termin)
                                    (nbytes fi_- n);
                    bsub fi_+ 1 -> bsub
                endrepeat
            endunless;
            false
        else
            ;;; not being run by Pass_input -- cons new process upto
            ;;; (but not including) ved_apply_action
            consproc_to(stacklength, caller(iscaller(ved_apply_action)-1))
                                                        -> proc;
            proc!PS_PERM_FLAGS _biset _:M_PS_SYSTEM -> proc!PS_PERM_FLAGS;
            true
        endif -> new;

        ;;; run out of input
        suspend_chain($-Sys$-Io$-Prompt(dev), dev, proc, new, 4, proc,
                                                        Add_waiting);
        chain(dev, bsub, userbuf, nbytes, Read)
    enddefine;      /* Read */

    ;;; test input procedure for a Ved device
    define lconstant Test_input(dev);
        lvars dev, item = vdd_subscr(VDD_INPUT, dev);
        returnunless(isclosure(item) and item!PD_CLOS_PDPART == Stringsin)
                                                                (false);
        fast_frozval(1, item) -> item;
        if fast_back(item) == [] then
            false
        else
            ;;; Allow for `\n` at eol
            datalength(fast_front(fast_back(item))) fi_- fast_front(item)
                                                    fi_+ 2
        endif
    enddefine;

    ;;; clear input procedure for a Ved device
    define lconstant Clear_input(dev);
        lvars dev;
        procedure(); termin endprocedure -> vdd_subscr(VDD_INPUT, dev)
    enddefine;

    if Isvedfile(file) then
        if file == ved_current_file then vedpathname
        else file(VF_PATHNAME)
        endif -> file
    else
        Check_string(file)
    endif;
    Check_integer(mode, 0);

    lvars p_vec = {%
        mode /== 1 and {^Read ^Test_input ^Clear_input},
        mode /== 0 and {^Write ^Flush},
        false,
        erase
    %};

    consdevice(file, file, initv(VDD_LEN), 1, p_vec) -> dev;
    veddevice_identifier -> vdd_subscr(VDD_IDENTIFIER, dev);
    insert_at_eof -> vdd_subscr(VDD_INSERT_AT_EOF, dev);
    mode /== 0 and 1 -> vdd_subscr(VDD_OUT_POS, dev);
    Clear_input(dev)
enddefine;      /* consveddevice */

define isveddevice(item);
    lvars item, v;
    isdevice(item)
    and isvector(device_user_data(item) ->> v)
    and datalength(v) == VDD_LEN
    and fast_subscrv(VDD_IDENTIFIER, v) == veddevice_identifier
enddefine;


    /*  input can be a character repeater, an identifier or <termin>
    */
define lconstant Pass_input(input);
    lvars dev, input;
    dlocal  running_process, restore_focus,
            0 %, unless vedinputfocus then
                    restore_focus -> vedinputfocus
                 endunless
              %
        ;
    Check_procs_waiting();
    explode(dest(vedprocwait) -> vedprocwait) -> restore_focus
                                    -> running_process -> dev -> ;
    ved_save_file_globals();
    Redraw_status_head();
    Set_wait_cursor(true, true);
    false -> vedinputfocus;
    input -> vdd_subscr(VDD_INPUT, dev);
    runproc(0, running_process)
enddefine;


define Input_text(do_range);
    ;;; load marked range interactive file
    lvars do_range, rep, char;
    dlocal vedstatic, vedautowrite;

    define lconstant Goto_prompt();
        lvars prompt;
        Get_prompt() -> prompt;
        vedendfile();
        if vvedbuffersize fi_> 0 then
            vedcharup();
            if isendstring(prompt, vedthisline())  then
                vedtextright();
                return
            else
                vedchardown()
            endif
        endif;
        vedinsertstring(prompt)
    enddefine;

    Check_procs_waiting();
    false ->> vedstatic -> vedautowrite;
    if vvedmarkprops then
        vedmarkpush(); false -> vvedmarkprops
    endif;
    ;;; if there's no marked range, mark the current line
    unless do_range then
        vedmarklo(); vedmarkhi()
    elseunless vvedmarkhi fi_> 0 then
        vedmarklo()
    endunless;
    if vdd_subscr(VDD_INSERT_AT_EOF, hd(vedprocwait)(WP_DEVICE)) then
        ;;; if range not at end of file, copy it there
        unless vvedmarkhi fi_>= vvedbuffersize then
            Goto_prompt();
            vedrangerepeater(vvedmarklo, vvedmarkhi) -> rep;
            until (rep() ->> char) == termin do
                vedcharinsert(char)
            enduntil;
            ;;; delete the last line inserted (blank except for prompt)
            vedlinedelete();
        endunless;
        vedendfile()
    else
        ;;; move to after the marked range
        vvedmarkhi fi_+ 1 -> vedline;
        1 -> vedcolumn;
        vedsetlinesize();
    endif;
    unless vedcursorset() then vedcheck() endunless;
    Pass_input(vedrangerepeater(vvedmarklo, vvedmarkhi))
enddefine;

define Input_keyboard_intr();
    vedclearinput();
    chainfrom(ident keyboard_interrupt, vedprocesschar, Pass_input)
enddefine;

    /*  Terminate input in immediate mode, after writing <termin> in
        VED buffer
    */
define ved_end_im();
    Check_procs_waiting();
    ;;; print termin in Ved buffer
    syswrite(hd(vedprocwait)(WP_DEVICE), '<termin>\n', 9);
    Pass_input(termin)
enddefine;


    /*  This procedure is assigned to <ESC> CTRL-D in vdkeys.p
        Switch between two modes: not(vedcrinteractive)-> vedcrinteractive
        If vedcrinteractive is TRUE then in an 'interactive mode' file
        (see vedsetpop) pressing CR will send current line to compiler.
        If FALSE then CR behaves as normal, ie. calls vedcharinsert, but
        ved_lmr will send marked range to compiler.
    */
define ved_switchmode_im;
    lvars on_prompt;
    vedprocswaiting()
    and issubstring(Get_prompt(), 1, vedthisline()) -> on_prompt;
    not(vedcrinteractive) -> vedcrinteractive;
    if vedcrinteractive then
        vedputmessage('\{b}<RETURN> will input line')
    else
        if on_prompt then
            vedmarklo(); vedmarkhi();
        endif;
        vedputmessage('\{b}marked range input only')
    endif
enddefine;


;;; --- REDIRECTING STANDARD I/O TO A VED FILE ---------------------------

define active ved_chario_file;
    chario_file
enddefine;
;;;
define updaterof active ved_chario_file fname;
    lvars fname;
    if fname then
        ;;; Added A.S. 3 Aug 2009
        returnif(fname == true);
        Check_string(fname);
        returnif(chario_file = fname);
        if vedinvedprocess then
            consveddevice(fname, 2, true) ->> pop_charin_device
                                ->> pop_charout_device -> pop_charerr_device
        else
            ;;; else this should be able to use runproc to run a procedure
            ;;; inside vedprocessproc to set the devices -- until this can be
            ;;; done, we just zap vedprocessproc
            false -> vedprocessproc
        endif
    else
        returnunless(chario_file);
        if vedinvedprocess then
            suspend_chain(0, vedprocessproc,
                    procedure();
                        chain(pop_charin_device, pop_charout_device,
                                pop_charerr_device, 3, vedprocessproc, runproc)
                    endprocedure)
                -> (pop_charin_device, pop_charout_device, pop_charerr_device)
        else
            ;;; else as comment above
            false -> vedprocessproc
        endif
    endif;
    fname -> chario_file
enddefine;

endsection;     /* Sys$-Ved$-Im */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug  3 2009
        To fix bug reported by Steve Isard, add extra test to the
        updater of ved_chario_file;
--- John Gibson, Sep  4 1996
        Changes to make Ved output devices cope with being used recursively
--- John Gibson, Mar 12 1996
        vvedpromptchar now a standard local of every file
--- John Gibson, Feb 16 1996
        Introduced var localise_outdev_files to force an output device
        to localise ved_current_file.
        Interrupt procedure now inputs keyboard_interrupt
--- John Gibson, Mar  9 1994
        Rewrote Write and Flush procedures for Ved devices so that
        output is buffered.
--- John Gibson, Jul 25 1993
        Made vvedpromptchar be dlocally set false inside the anonymous
        procedure in Write, rather than in Write itself (i.e. so it gets
        set false AFTER selecting the output file!)
--- John Gibson, Apr 20 1993
        Using new is_vedfile_local, made vvedpromptchar only be set locally
        to \Sp when vedprocswaiting is true (default value of vvedpromptchar
        is now false).
--- John Gibson, Dec 21 1992
        Moved in ved_chario_file from vdprocess.p
--- John Gibson, Jul 13 1992
        Made Stringsin map a traiiing space to a space
--- John Gibson, Apr 24 1992
        Fixed test for when to call -vedcheck- in -Write-
--- John Gibson, Feb 24 1992
        Changed vvedpromptchar to be 16:9F
--- John Gibson, Dec 18 1991
        Made -consveddevice- construct processes upto below ved_apply_action
        instead of vedprocesschar
--- John Gibson, Jun 25 1991
        Exported -consveddevice- and -isveddevice-
--- John Gibson, Oct 31 1990
        -Cons_dev- rewritten to use -consdevice-, and improved using
        -vedinputfocus-.
--- John Gibson, Oct 26 1990
        Further changes to -Cons_dev-
--- John Gibson, Oct 24 1990
        Changed -Cons_dev- to put clear/test/ input procedures in device.
--- John Gibson, Oct 23 1990
        Changes to ved device procedures for new-style dev read/write
        procedure args.
--- John Gibson, Jun  5 1990
        Cleaned up code for clearing of input on ved devices
--- John Gibson, Mar  3 1990
        Made -popgctrace- locally false inside ved device procedures.
 */
