/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/lib/xved_standalone_setup.p
 > Purpose:         Standalone setup for XVed
 > Author:          John Gibson, Jun  7 1993 (see revisions)
 > Documentation:
 */
compile_mode :pop11 +strict;

section;

include xved_constants.ph;
include subsystem.ph;

/*
This string is printed out by xved_standalone_setup if there are any
errors during the processing of arguments to the XVed saved image.
*/

lconstant usage_string =

'\
Usage: %p [-options ...] [ -do command ...] [ =line_number ] file ...\
\
   =place                Edit the following file at specified place\
   -do command           Execute a Ved ENTER command (See HELP *VEDCOMMS)\
\
options are:\
\
   -help                 Prints this\
   -im                   Start Ved with immediate mode (See HELP *VED_IM)\
   -chario file          Redirect Poplog standard devices to a Ved file\
   -output file          Redirect Poplog compiler output to a Ved file\
\
plus all the standard X Toolkit options, which include:\
\
   -display host:dpy     X server to use\
   -geometry geom        Size of window\
   -fn font              Font used for text\
   -fg colour            Foreground colour\
   -bg colour            Background colour\
   -xrm resourcestring   Specify a set of X resources as a string\
   -iconic               Create window initially as an icon\
   -name name            The X application name\
   -rv                   Start up window in reverse video\
\
(Run "xved -do \'teach xved\'" for more details)\
';


;;; this is a setup used for running xved as a "standalone" client
;;; it processes command line arguments, calls ved, and exits.

define xved_standalone_setup();
    lvars   arg, place, first_char, do_im = false;

    define lconstant Usage;
        ;;; print usage message and exit
        dlocal cucharout = cucharerr;
        printf(poparg0, usage_string);
        false -> pop_exit_ok;
        sysexit();
    enddefine;

    define lconstant Next_arg;
        ;;; exits if there aren't enough arguments
        if poparglist.islist and poparglist /== [] then
            dest(poparglist) -> poparglist
        else
            warning(poparglist,1, 'XVed: TOO FEW COMMAND LINE ARGUMENTS');
            Usage();
        endif;
    enddefine;


    ;;; process special options first. Leave remaining options in arglist
    [%  while poparglist /== [] and isstartstring('-', hd(poparglist)) do
            Next_arg() -> arg;
            if arg = '-help' then
                Usage();
            elseif arg = '-im' then
                true -> do_im;
            elseif arg = '-chario' then
                ;;; specify a standard io file (disassociate popdevin/out)
                Next_arg() -> ved_chario_file;
            elseif arg = '-output' then
                ;;; Specify Poplog's output file
                Next_arg() -> vedlmr_print_in_file;
            elseif arg = '-geometry' then
                '-xrm', XV_CLASS_NAME sys_>< '*XpwScrollText.geometry:'
                                        sys_>< Next_arg()
            else
                arg;
            endif;
        endwhile;
        ;;; add in remaining args and and popunderx args
        dl(poparglist); if popunderx.islist then popunderx.dl endif
    %] ->> poparglist -> popunderx;

    ;;; Assume syssetup hasn't been called
    sys_subsystems_init(SS_SETUP);

    ;;; startup X and XVed - obtain any unused arguments
    unless pop_noinit then
        sys_subsystems_init(SS_INITCOMP)
    endunless;
    xvedsetup();
    [%explode(XptUnprocessedArgs)%] -> poparglist; ;;; must copy
    vedsetup();

    unless ved_chario_file then
        if vedlmr_print_in_file.isstring then
            vedlmr_print_in_file
        elseif vedlmr_errs_in_file.isstring then
            vedlmr_errs_in_file
        else
            'output.p'
        endif -> ved_chario_file;
    endunless;

    if do_im then vedinput(ved_im) endif;

    ;;; process file arguments

    vedinput([%
    until poparglist == [] do
        dest(poparglist) -> poparglist -> arg;
        nextif(arg = nullstring);
        arg(1) -> first_char;
        if arg = '-do' then                                 ;;; A COMMAND
            Next_arg() -> arg;
            veddo(%arg%)
        elseif first_char == `=` then                       ;;; A PLACE
            if datalength(arg) > 1 then allbutfirst(1, arg) else Next_arg()
            endif -> place;
            unless strnumber(place) ->> place then Usage() endunless;
            procedure; place -> vvedgotoplace; endprocedure;
            Next_arg() :: poparglist -> poparglist; ;;; needs more args
        elseif fast_lmember(first_char,[ `-` `+` `%`]) then ;;; INVALID
            warning(arg,1,'XVED: INVALID COMMAND LINE OPTION'); Usage();
        else                                                ;;; A FILE
            procedure(vedargument);
                dlocal vedargument;
                ved_ved();
            endprocedure(% arg %);
        endif;
    enduntil%]);

    ;;; if we weren't given anything to do, default to ved_ved
    if ved_char_in_stream == [] then vedinput(ved_ved) endif;


    ;;; Procedure to repeatedly process XVed events until there are no
    ;;; buffers/inputs left:

    define lconstant main_loop();
        lconstant macro CONTINUE = [(xved$-xvedappcontext
                                        and (vedbufferlist /== []
                                            or ved_char_in_stream /== []))];

        dlocal 0 %, if dlocal_context /== 1 and CONTINUE then
                        true -> pop_first_setpop    ;;; make setpop reset
                    else
                        sysexit()
                    endif
                 %;

        false -> subsystem;     ;;; no top-level compiler running

        while CONTINUE do syshibernate() endwhile
    enddefine;

compile_mode :vm -prmprt;

    ;;; Redefine pop_setpop_compiler to call main_loop, through sysCOMPILE
    ;;; to ensure VM is set up properly. (This doesn't affect the normal
    ;;; vedsetpop definition of pop_setpop_compiler.)
    sysCOMPILE(%main_loop%) -> pop_setpop_compiler;
    true -> pop_first_setpop;
    setpop -> interrupt;
    ;;; go into a loop
    setpop();
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Replaced call to sysinitcomp with code that runs all subsystem
        SS_INITCOMP procedures, provided pop_noinit is false.
--- John Gibson, Feb 24 1995
        Changed main_loop to quit if xvedappcontext becomes false (i.e. if
        X connection broken)
 */
