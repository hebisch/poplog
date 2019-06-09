/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/startup.p
 > Purpose:         PML: Startup procedures
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */

weak constant procedure $-subsystem_button_selectable;

section $-ml =>
    ml_initcomp
    ml_setup
    ml_startup
    ml_subsystem_procedures
;

include subsystem.ph;

vars
    ml_version_messages = [^ml_title_string],
        ;;; messages printed on startup
;

define lconstant quiet_compile(file);
    lvars   file;
    dlocal  ml_quiet = true, ml_timer = false;
    subsystem_compile(file, "ml");
enddefine;

define ml_trycompile(file);
    lvars file;
    if readable(file) ->> file then
        quiet_compile(file), true;
    else
        false;
    endif;
enddefine;

define ml_expcompile(e);
    lvars e;
    ;;; argument may be a "stringin" character producer (yuk!)
    if isclosure(e) then front(frozval(1, e)) -> e endif;
    quiet_compile(stringin('(' <> e <> ')'));
enddefine;

define ml_initcomp();
    dlocal  vedvedname;
    ml_trycompile('$popsys/init.ml') -> ;
    unless ml_trycompile('$poplib/init.ml') then
        ml_trycompile('init.ml') -> ;
    endunless;
enddefine;

define ml_setup();
    setup_std_io();
    false -> ml_quiet;
enddefine;

define ml_vedsetup();
    unless vedsetupdone then
        'temp' <> ml_filetype -> vedvedname;
        'output' <> ml_filetype
            ->> vedlmr_print_in_file -> vedlmr_errs_in_file;
        'pml' -> vedhelpname;
    endunless;
enddefine;

define ml_xsetup();
    if testdef subsystem_button_selectable then
        true -> weakref subsystem_button_selectable("ml");
    endif;
enddefine;

define ml_prbanner();
    lvars msg;
    for msg in ml_version_messages do
        appdata(msg, cucharout);
        cucharout(`\n`);
    endfor;
enddefine;

define ml_reset();
    if systrmdev(pop_charin_device) == true then
        charout('\nSetml\n');
    endif;
    ;;; suppress awful start of line shell escapes
    0 -> poplastchar;
enddefine;

/*
 *  Startup procedure for standard PML image
 */

lvars
    option_banner       = true,
        ;;; print the banner
    option_initialise   = true,
        ;;; compile the "init.ml" files
    option_stdin        = true,
        ;;; compile from the standard input
    option_loadfiles    = [],
        ;;; list of files/expressions to compile
    option_vedfile      = false,
        ;;; start up by editing this file
    option_imfile       = false,
        ;;; start up in immediate mode in this file
;

define lconstant read_pml_options();
    lvars option, arg, options = poparglist;
    not(pop_nobanner) -> option_banner;
    not(pop_noinit) -> option_initialise;
    true -> option_stdin;
    false ->> option_vedfile -> option_imfile;
    [] -> option_loadfiles;
    while options /== [] do
        Front(options) -> option;
        quitunless(isstring(option) and isstartstring('-', option));
        Back(options) -> options;
        if option = '-' then
            ;;; ignore
        elseif option = '--' then
            ;;; stop
            quitloop;
        elseif option = '-nobanner' or option = '-nb' then
            false -> option_banner;
        elseif option = '-noinit' or option = '-ni' then
            false -> option_initialise;
        elseif option = '-nostdin' or option = '-ns' then
            false -> option_stdin;
        else
            false -> arg;
            if options /== [] and isstring(Front(options))
            and not(isstartstring('-', Front(options)))
            then
                Destpair(options) -> options -> arg;
            endif;
            if (option = '-load' or option = '-l') and arg then
                [^^option_loadfiles ^arg] -> option_loadfiles;
            elseif (option = '-eval' or option = '-e') and arg then
                ;;; wrap it in a reference to show it's an expression
                consref(arg) -> arg;
                [^^option_loadfiles ^arg] -> option_loadfiles;
            elseif option = '-ved' or option = '-v' then
                arg or true -> option_vedfile;
            elseif option = '-im' or option = '-i' then
                arg or true -> option_imfile;
            endif;
        endif;
    endwhile;
    ;;; did we read anything?
    if options == poparglist then
        false;
    else
        options ->> poparglist;
    endif;
enddefine;

;;; ml_startup:
;;;     ML subsystem startup procedure
;;;     NB: call -ml_setup- before running this.

define ml_startup();
    lvars file;
    ;;; Default to immediate exit on interrupt
    sysexit -> interrupt;
    ;;; Set subsystem to ML
    "ml" -> subsystem;
    ;;; Read PML-specific command line options;
    ;;; if there are none, but the arglist is non-empty,
    ;;; hand over to the default startup
    unless poparglist == [] or read_pml_options() then
        sys_process_poparg1(ml_expcompile, ml_trycompile, ml_filetype);
        return;
    endunless;
    ;;; Check for compilation of standard input
    if option_stdin then
        ;;; force interactive
        true -> pop_first_setpop;
        setpop -> interrupt;
        if option_banner and systrmdev(pop_charin_device) == true then
            ;;; print the banner
            sys_subsystems_init(SS_BANNER);
        endif;
    endif;
    ;;; Initialise
    if option_initialise then
        sys_subsystems_init(SS_INITCOMP);
    endif;
    ;;; Compile files and expressions from the command line
    for file in option_loadfiles do
        if isref(file) then
            ml_expcompile(cont(file));
        else
            quiet_compile(sourcefile(file) or file);
        endif;
    endfor;
    [] -> option_loadfiles;
    ;;; Quit now if not interactive
    unless option_stdin then sysexit() endunless;
    ;;; X setup
    if popunderx then
        sys_subsystems_init(SS_XSETUP);
    endif;
    ;;; Start VED if wanted
    if option_vedfile == true then
        'temp' <> ml_filetype -> option_vedfile;
    endif;
    if option_imfile == true then
        'output' <> ml_filetype -> option_imfile;
    endif;
    if option_vedfile then
        option_vedfile -> vedvedname;
    elseif option_imfile then
        option_imfile ->> option_vedfile -> vedvedname;
    endif;
    if option_vedfile
    and systrmdev(pop_charin_device) and systrmdev(pop_charout_device)
    then
        vedsetup();
        if option_imfile then
            procedure;
                option_imfile -> vedargument;
                chain(ved_im);
            endprocedure ::
            ved_char_in_stream -> ved_char_in_stream;
        endif;
        chain(vedveddefaults, option_vedfile, vededitor);
    endif;
enddefine;


/*
 *  ML subsystem procedures
 */

constant ml_subsystem_procedures =
    {   ^ml_compile         ;;; SS_COMPILER
        ^ml_reset           ;;; SS_RESET
        ^ml_setup           ;;; SS_SETUP
        ^ml_prbanner        ;;; SS_BANNER
        ^ml_initcomp        ;;; SS_INITCOMP
        ^ml_startup         ;;; SS_POPARG1
        ^ml_vedsetup        ;;; SS_VEDSETUP
        ^ml_xsetup          ;;; SS_XSETUP
    };


endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 27 1995
        Made ml_xsetup register PML with the UI if it's there
--- Robert John Duncan, Nov 24 1994
        Sectionised. Macro definitions moved out to "compile.p" and Ved
        initialisation to "mlved.p"
--- John Gibson, Jan 19 1993
        o Changed quiet_compile to use subsystem_compile
        o Replaced use of s*witch_subsystem_to with sys_compiler_subsystem(`c`)
--- John Gibson, Jan 14 1993
        o   Moved subsystem outer list entry to new lib ml_subsystem, with
            subsystem procedures now in vector ml_subsystem_procedures
        o   Replaced subsystem_ setup procedures with
            sys_subsystems_init(field)
--- Robert John Duncan, Apr  6 1992
        Renamed search list identifiers; added search list for -vedsrcname-
--- Robert John Duncan, Jul 25 1991
        Added subsystem title.
--- Robert John Duncan, Jun 18 1991
        Changed subsystem search-lists not to use procedures.
--- Robert John Duncan, Mar  1 1991
        Abolished -ml*_arglist-: option processing may now change
        -poparglist-.
--- Robert John Duncan, Nov  6 1990
        Changed -popdevX- to -pop_charX_device-
--- Robert John Duncan, Oct 17 1990
        Banner now consists of a (user-assignable) list of version messages.
--- Robert John Duncan, Oct 12 1990
        Put ML subsystem entry at the back of the subsystem table to match
        the changed order of processing by lib subsystem.
        Changed startup to do full subsytem banner, initcomp and xsetup.
--- Robert John Duncan, Oct  5 1990
        Removed leading newline from the banner.
--- Robert John Duncan, Oct  2 1990
        Revised processing of command-line arguments to reflect standard
        Poplog treatment.
--- Simon Nichols, Sep 10 1990
        Added -ml_xsetup-.
--- Robert John Duncan, Aug  8 1990
        Changes for extended subsystems.
--- Rob Duncan, May 29 1990
        Added macro -pml- as synonym for -ml-
--- Rob Duncan, Apr 10 1990
        Moved ml initialisation to end of -vedfiletypes-
--- Rob Duncan, Feb 23 1990
        Revised to make use of command line options
--- Rob Duncan, Jan 31 1990
        Added -getmllib- into -popuseslist- in -ml_setup- to enable "showlib"
--- Rob Duncan, Jan  9 1990
        Removed reference to -v*ed_try_??-
--- Rob Duncan, Oct 26 1989
        Added change to -vedteachlist- in -ml_setup-
 */
