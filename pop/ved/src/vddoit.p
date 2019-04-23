/* --- Copyright University of Sussex 2009. All rights reserved. ----------
 > File:            C.all/ved/src/vddoit.p
 > Purpose:         Procedures to compile marked range
 > Author:          John Gibson, Aaron Sloman (see revisions)
 */

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../lib/include/subsystem.ph'

constant
        procedure (vedpresent, vedinsertstring, vedmarkfind, vedissubitem,
        ved_pr_exception, consveddevice)
    ;

vars
        vederrs_on_status = false,
        procedure (ved_lcp, ved_mcp, pop_default_pr_exception,
        pop_default_prmishap),
    ;

section $-Sys$-Ved;

constant
        procedure (Shallow_pr, Searchlist_name, Do_status_range,
        Im$-Input_text
        )
    ;

vars
        Im$-localise_outdev_files
    ;

endsection;


;;; -------------------------------------------------------------------

section $-Sys$-Ved =>   vedinteractive, vedpopready,
                        is_ved_lmr_stream, ved_lmr, ved_lcp,
                        vedsetpop
                    ;


vars
    vedinteractive          =  false,
    procedure vedpopready   =  setpop,

    lmr_ignore_colons       =  false,       ;;; used in vdcompile.p
    ;

lvars
    lmr_startfile,
    lmr_interrupted = false,
    ;


define lconstant Vedcompile(pop_charin_device, pop_charout_device,
                            pop_charerr_device, prexc_p, charrep, outfile);

    lvars   charrep, file, outfile, old_readline = readline, p,
            procedure prexc_p;

    dlocal  pop_charin_device, pop_charout_device, pop_charerr_device,
            cucharout   = charout,
            cucharerr   = charerr,
            pop_charerr_col, pop_charout_col,
            poplinenum, popnewline,
            vedinteractive = true,
            lmr_ignore_colons,
            pop_compiler_subsystem,
            poplineprefix = false,
            prmishap,
            Im$-localise_outdev_files = false;
        ;

    dlocal 0 %, if dlocal_context == 2 then
                    unless lmr_interrupted then
                        '\{b}interrupted' -> lmr_interrupted
                    endunless;
                    setpop_reset(false, false);
                    ;;; must have an abnormal exit here because
                    ;;; setpop_reset has cleared the stack
                    chain(identfn)
                endif %;

    define dlocal interrupt();
compile_mode :vm +pentch;
        vedpopready()
    enddefine;

    define dlocal pop_pr_exception(count, mess, idstring, sev) with_props false;
        lvars count, mess, idstring, sev, x;
        dlocal pr;
        ;;; display Pop-11 and itemiser syntax and illegal declaration errors
        ;;; etc on the status line
        if sev == `E` and vedpopready == setpop
        ;;; added A.Sloman 4 Aug 2009
        and vederrs_on_status
        and (       isendstring(':syntax',idstring)
                and (isstartstring('pop11-',idstring)
                     or isstartstring('incharitem-',idstring))
            or      issubstring(':name-',idstring)
                and (isstartstring('pop11-',idstring)
                     or isstartstring('vm-ident',idstring))
            )
        then
            Errmess_sprintf(count, mess, idstring, false) -> mess;
            vededit(lmr_startfile);
            not(ved_on_status) -> ved_on_status;
            vedlinebelow();
            vedinsertstring(mess);
            vedcharup(); vedtextright();
            not(ved_on_status) -> ved_on_status;
            mess -> lmr_interrupted;
            return
        endif;

        Shallow_pr -> pr;
        prexc_p(count, mess, idstring, sev)
    enddefine;

    define dlocal readline();
        dlocal proglist_state = proglist_new_state(charin);
        old_readline()
    enddefine;

    unless isundef(pop_default_prmishap ->> p) or p == identfn then
        ;;; backward compatibility for prmishap
        p -> prmishap;
        sys_pr_exception -> pop_pr_exception
    endunless;
    unless isundef(pop_default_pr_exception ->> p) then
        ;;; Allow user's default procedure to take precedence
        p -> pop_pr_exception
    endunless;

    subsystem == "pop11" and fast_lmember(vedfileprops, [help teach ref doc])
            -> lmr_ignore_colons;

    if outfile then
        ;;; output will go to another already existing file
        fast_subscrv(VF_COLUMN, outfile) fi_- 1
    else
        ;;; output will go to the current file, or a new file
        0
    endif ->> pop_charout_col -> pop_charerr_col;
    conspair(vedpathname, vvedmarklo) -> charrep!PD_PROPS;
    subsystem -> pop_compiler_subsystem;
    subscr_subsystem(SS_COMPILER, subsystem)(charrep);
    sysflush(pop_charout_device);
    sysflush(pop_charerr_device);
enddefine;


    /*  Character repeater for ved_lmr. Ignore leading colons
    */
define lconstant Lmr_stream(statepair, linenum, getline) -> char;
    lvars statepair, char, linenum, procedure getline;
    if fast_front(statepair) == undef then
        ;;; need to set up next line first
        lmr_startfile -> ved_current_file;
        getline(statepair, linenum)
    endif;
    if fast_front(statepair) then
        Stringin(statepair) -> char;
        if char == `\St` then
            ;;; turn trailing space into space
            `\s` -> char
        elseif char == termin then
            ;;; got to end of line
            `\n` -> char;
            undef -> fast_front(statepair);
        endif
    else
        ;;; end of range
        termin -> char;
    endif;
    char -> poplastchar
enddefine;

define is_ved_lmr_stream() with_nargs 1;
    pdpart() == Lmr_stream
enddefine;

    /*  Load Marked Range
    */
define Lmr(restore) -> out_current;
    lvars   procedure prexc = pop_pr_exception, in_dev, out_dev, err_dev,
            statepair, linenum, restore, out_current, file,
            savesubsys = subsystem;
    dlocal  vedstatic = false, vedautowrite = false,
            lmr_startfile = ved_current_file, lmr_interrupted = false;

    define lconstant Set_line_state(statepair, line);
        lvars line, string, col, len, col2, statepair;
        if line fi_> vvedbuffersize or line fi_> vvedmarkhi then
            ;;; end of range
            false -> string, 1 -> col
        else
            Buffer_line(line) -> string;
            ;;; Trim string
            vedusedsize(string) -> len;
            if datalength(string) fi_> len then
                subvedstring(1, len, string) ->> string -> Buffer_line(line)
            endif;
            if locchar_back(`\Sp`, datalength(string), string) ->> col then
                col fi_+ 1
            else
                1
            endif -> col;
            if lmr_ignore_colons then
                ;;; ignore leading colons in demos, etc.
                ;;; But not if part of a word.
                Skipwhite(col, string) -> col2;
                if col2 and fast_subscrs(col2, string) == `:`
                and vedissubitem(":", col2, string) == col2 then
                    col2 fi_+ 1 -> col
                endif
            endif
        endif;
        col -> fast_back(statepair);
        string ->> fast_front(statepair)        ;;; return string or false
    enddefine;

    define lconstant Linerep1(statepair, linenum);
        lvars linenum, n, statepair;
        fast_cont(linenum) ->> n -> poplinenum;
        n fi_+ 1 -> fast_cont(linenum);
        Set_line_state(statepair, n) ->
    enddefine;

    define lconstant Linerep2(statepair, linenum);
        lvars linenum, statepair;
        vedline ->> fast_cont(linenum) -> poplinenum;
        if Set_line_state(statepair, vedline) then
            ;;; not end of range
            vedline fi_+ 1 -> vedline;
            1 -> vedcolumn;
            vedsetlinesize()
        endif
    enddefine;

    define lconstant Lmr_fexc(count, mess, idstring, sev,
                                        prexc, statepair, linenum);
        lvars count, mess, idstring, sev, statepair, linenum, procedure prexc;
        ;;; put error message into file
        vededit(lmr_startfile);
        fast_cont(linenum) -> vedline; ;;; vedsetlinesize(); not needed
        vedlinebelow();
        vedcheck();
        fast_back(statepair) -> vedcolumn;
        vedcharinsert(`^`);
        if sev == `I` or sev == `W` then
            prexc(count, mess, idstring, sev);
            vedline -> fast_cont(linenum)
        else
            Clear_temp_mark();
            vedmarklo();    ;;; mark the cursor column
            prexc(count, mess, idstring, sev);
            vedmarkhi()
        endif
    enddefine;

    ;;; check subsystem compiler loaded
    subscr_subsystem(SS_COMPILER, subsystem, vederror) -> ;

    if prexc == ved_pr_exception then sys_pr_exception -> prexc endif;
    ;;; if there's no marked range, mark the current line
    unless vvedmarkhi fi_> 0 then vedmarklo() endunless;
    conspair(undef, 1) -> statepair;
    consref(vvedmarklo) -> linenum;

    vedlmr_print_in_file -> file;
    ;;; nextbit is complicated in case of vedsearchlist /= []
    if file == true or isinteger(file) then
        vedpathname -> file
    endif;
    if isstring(file) then
        Searchlist_name(file) -> file;
        if ispair(file) then front(file) -> file endif;
    endif;
    if isstring(file) and file /= vedpathname and file /= vedcurrent then
        ;;; Put output into a different file.
        consveddevice(file, 2, true) -> out_dev;
        Vedcompile(out_dev, out_dev, out_dev, prexc,
                    Lmr_stream(%statepair, linenum, Linerep1%),
                    vedpresent(file));
        fast_cont(linenum) fi_- 1 -> fast_cont(linenum);
        false -> out_current
    else
        ;;; put output on screen or into current file
        vedmarkfind();
        pop_charerr_device -> err_dev;
        if file and vedlmr_print_in_file then
            consveddevice(file, 2, false) ->> in_dev -> out_dev;
            if vedlmr_errs_in_file then
                out_dev -> err_dev;
                Lmr_fexc(%prexc, statepair, linenum%) -> prexc
            endif
        else
            pop_charin_device -> in_dev, pop_charout_device -> out_dev
        endif;
        Vedcompile(in_dev, out_dev, err_dev, prexc,
                        Lmr_stream(%statepair, linenum, Linerep2%), false);
        true -> out_current
    endif;

    savesubsys -> lmr_startfile(VF_SUBSYSTEM);
    if ved_current_file == lmr_startfile then
        savesubsys -> subsystem
    endif;

    if lmr_interrupted then
        ;;; allow mishap procedure to change vedinputfocus to keep mishap
        ;;; window as current
        vededit(lmr_startfile, vedinputfocus == lmr_startfile);
        max(1,fast_cont(linenum)) -> vedline;
        vedsetlinesize();
        fast_back(statepair) -> vedcolumn;
        vederror(lmr_interrupted);
    elseif restore then
        ;;; Make sure original file is on screen
        vededit(lmr_startfile);
        if isstring(restore) then vedputmessage(restore) endif
    endif
enddefine;      /* Lmr */

define vars ved_lmr();
    ;;; load marked range
    if ved_on_status then chain(Do_status_range) endif;
    if vedprocswaiting() then chain(true, Im$-Input_text) endif;
    vedputmessage('\{b}doing ...');
    if Lmr('\{b}done') and caller(1) /== ved_lcp then
        Clear_temp_mark();
        vedmarklo()
    endif
enddefine;

define vars ved_lcp();
    vedmarkpush();
    false -> vvedmarkprops;
    ved_mcp();
    ved_lmr();
    vedmarkpop()
enddefine;

define vedsetpop();
    lvars   p;
    dlvars  restart = true;
    dlocal  vedinteractive, popprompt, popnewline,
            pop_charin_device, pop_charout_device, pop_charerr_device,
            pop_charout_col, pop_charerr_col,
            pop_pr_exception, prmishap, interrupt, popexit,
            pop_charin_escapes = [],
            Im$-localise_outdev_files = false,
        ;

    if iscaller(vedsetpop, 1) then
        setpop()
    elseunless vedinvedprocess then
        mishap(0, 'vedsetpop: NOT INSIDE VED')
    endif;
    vedprocswaiting() -> ;
    consveddevice(vedpathname, 2, true)
        ->> pop_charin_device ->> pop_charout_device -> pop_charerr_device;
    true -> vedinteractive;
    false -> vedbreak;
    setpop -> interrupt;

    unless isundef(pop_default_prmishap ->> p) or p == identfn then
        ;;; backward compatibility for prmishap
        p -> prmishap
    endunless;
    unless isundef(pop_default_pr_exception ->> p) then
        p
    else
        sys_pr_exception
    endunless -> pop_pr_exception;

    define dlocal popexit();
        false -> restart;
        exitfrom(vedsetpop)
    enddefine;

    define dlocal pop_setpop_compiler();
        dlocal pop_compiler_subsystem, poplineprefix = false;

        define lconstant ss_err(errms);
            lvars errms;
            vedputmessage(errms);
            vedscreenbell();
            sysexit()
        enddefine;

        Subsystem_setpop_compiler(ss_err)
    enddefine;

    define lconstant Reset();
        dlocal 0 %, if dlocal_context == 2 and restart then
                        chain(Reset)
                    endif %;
        vedclearinput();
        setpop_reset(false, true)
    enddefine;

    Reset()
enddefine;

    /* VED setup for Pop-11 subsystem */
define Pop11_vedsetup();
    returnif(vedsetupdone);
    'temp.p' -> vedvedname;
    nullstring ->> veddocname ->> vedhelpname ->> vedrefname -> vedteachname;
    'output.p' -> vedlmr_print_in_file;
    false -> vedlmr_errs_in_file;
enddefine;



endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug  4 2009
        Added vederrs_on_status to control error printing in ved
        default false
--- John Gibson, Apr  9 1996
        Changed local pop_pr_exception in Vedcompile to use mishap idstrings
        for testing syntax errors etc.
--- John Gibson, Feb 16 1996
        Vedcompile and vedsetpop now locally set Im$-localise_outdev_files
        to false
--- John Gibson, Feb 10 1996
        Replaced definitions for pr*mishap with pop_pr_exception etc
--- John Gibson, Mar  7 1994
        Uses vededit instead of ved*select
--- John Gibson, Jun 12 1993
        Moved is_ved_output_file to vdfiles.p
--- John Williams, May 10 1993
        Added is_ved_output_file
--- John Gibson, Jan 12 1993
        popcom*piler -> subsystem etc
--- John Gibson, Jan 11 1993
        Added Pop11_vedsetup
--- John Gibson, Jul 13 1992
        Made Lmr_stream turn a trailing space into a space
--- Robert John Duncan, Jul 26 1991
        Changed default output file to 'output.p'
--- John Gibson, Jun 27 1991
        Stopped lmr_ignore_colons getting set except for documentation
        files
--- John Gibson, Jun 24 1991
        Various changes
--- John Gibson, Nov  2 1990
        Fixed problem in -Vecompile- dlocal exit code.
--- John Gibson, Oct 27 1990
        Replaced -popdevX- with -pop_charX_device- in -Lmr-.
        Changed calls to -setpop_reset- for new version.
--- Aaron Sloman, Oct 13 1990
        Made pop_default_prmishap vars procedure
--- Aaron Sloman, Oct 12 1990
        Replaced xved with wved. Used pop_default_prmishap
--- John Gibson, Sep  6 1990
        Added procedure entry checks in local -interurpt- in -Vedcompile-
        (see bugreport mike.8)
--- John Gibson, Jun  6 1990
        Replaced use of internal device vars with public active ones
--- Aaron Sloman, Jan  3 1990
        Doesn't call vedsetonscreen if not necessary
--- Aaron Sloman, Dec 28 1989
        Introduced pop_charin_escapes, local to vedsetpop, to prevent
        unintended spawning of sub-processes when compiling from VED
--- John Gibson, Dec 21 1989
        Replaced (abusive) use of refs to wrap devices assigned to
        -c*harin_dev- etc (in -Lmr-, -vedsetpop-) with (correct) use of
        identifiers for that purpose (using -ident- to construct run-time
        identifiers). (Necessitated by new pop pointers, which mean that
        -fast_idval- no longer works with refs.)
--- John Gibson, Oct 18 1989
        Put into section Sys$-Ved; im procedures into Sys$-Ved$-Im.
            Replaced use of s*etpop_handlers with dlocal expressions
        trapping attempts to exit thru -vedsetpop- and -Vedcompile-.
--- John Gibson, Sep 11 1989
        Made -vedsetpop- and -Vedcompile- use non-local gotos in their
        setpop handlers, and then call -setpop_reset- after unwinding.
--- John Gibson, Sep  8 1989
        Made -setpop- assign false to ch*ain_trace before calling handler so
        handlers don't need to.
--- John Williams, Oct  6 1988
        Added -is_ved_lmr_stream-
--- John Williams, Sep  8 1988
        -popfilename- & -poplinenum- now catered for
--- Roger Evans, May  6 1988
        Fixed bad args to isstartstring in Vedcompile
--- John Gibson, Mar 14 1988
        -Stringin- into section Sys
 */
