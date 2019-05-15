/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_subsystem_procedures.p
 > Purpose:         Prolog: subsystem procedures
 > Author:          Robert John Duncan, Jun 23 1993 (see revisions)
 > Documentation:
 > Related Files:   C.all/lib/lib/prolog_subsystem.p
 */

uses prolog_subsystem;

weak constant procedure $-subsystem_button_selectable;

section prolog;

constant
    procedure ( prolog_file_type, prolog_query, prolog_directive, );

weak constant
    procedure ( prolog_init_ved, ),
    prolog_banner,
;

vars
    default_compile_mode,
    readenv;

weak vars
    prolog_messages,
    prolog_modules,
;

;;; =======================================================================

vars
    prolog_version_list = [],
        ;;; list of version messages
;

;;; prolog_version:
;;;     prints or updates prolog_version_list

define prolog_version(message);
    lvars message;
    if message == nil then
        For message on prolog_version_list do
            format('~w~n', message);
        endfor;
    else
        prolog_generalise(message) -> message;
        [^^prolog_version_list ^message] -> prolog_version_list;
    endif;
enddefine;

;;; prolog_trycompile:
;;;     compiles the given file if it exists and returns <true>

define prolog_trycompile(file);
    lvars file;
    ;;; file is a file specification
    if readable(file) ->> file then
        subsystem_compile(file, "prolog"), true;
    else
        false;
    endif;
enddefine;

/*
 *  Subsystem procedures
 */

define prolog_setprolog;
    if systrmdev(pop_charin_device) == true then
        appdata('\nSetprolog\n', charout);
    endif;
    prolog_reset();
    if testdef prolog_modules then
        ;;; clear the module stack
        [] -> weakref prolog_modules;
    endif;
enddefine;

define prolog_setup();
    if prolog_version_list == [] then
        if testdef prolog_init_ved then
            weakref prolog_init_ved();
        endif;
        if testdef prolog_banner then
            prolog_version(weakref prolog_banner);
        endif;
        if testdef prolog_messages then
            applist(weakref prolog_messages, prolog_version);
        endif;
    endif;
enddefine;

define prolog_prbanner =
    prolog_version(%[]%);
enddefine;

define prolog_initcomp();
    lvars   dev;
    dlocal  VEDWEAK vedvedname;
    ;;; systemwide init.pl
    if readable('$popsys/init.pl') ->> dev then
        subsystem_compile(dev, "prolog");
    endif;
    ;;; user init.pl
    if readable('$poplib/init.pl') or readable('init.pl') ->> dev then
        subsystem_compile(dev, "prolog")
    endif;
enddefine;

define prolog_poparg1();
    sys_process_poparg1(prolog_toplevel_compile, prolog_trycompile,
        prolog_file_type());
enddefine;

define prolog_xsetup();
    if testdef subsystem_button_selectable then
        true -> weakref subsystem_button_selectable("prolog");
    endif;
enddefine;

define prolog_vedsetup();
    if VEDLOADED and not(VEDWEAK vedsetupdone) then
        lvars file_type = prolog_file_type();
        'prolog' ->> VEDWEAK vedhelpname -> VEDWEAK vedteachname;
        'temp' <> file_type -> VEDWEAK vedvedname;
        'output' <> file_type ->> VEDWEAK vedlmr_print_in_file
            -> VEDWEAK vedlmr_errs_in_file;
    endif;
enddefine;

;;; Prolog subsystems procedure vectors:

constant
    prolog_subsystem_procedures =
        {
            ^prolog_compile             ;;; SS_COMPILER
            ^prolog_setprolog           ;;; SS_RESET
            ^prolog_setup               ;;; SS_SETUP
            ^prolog_prbanner            ;;; SS_BANNER
            ^prolog_initcomp            ;;; SS_INITCOMP
            ^prolog_poparg1             ;;; SS_POPARG1
            ^prolog_vedsetup            ;;; SS_VEDSETUP
            ^prolog_xsetup              ;;; SS_XSETUP
        },
    top_subsystem_procedures =
        {
            ^prolog_toplevel_compile    ;;; SS_COMPILER
            ^prolog_setprolog           ;;; SS_RESET
            ^identfn                    ;;; SS_SETUP
            ^identfn                    ;;; SS_BANNER
            ^identfn                    ;;; SS_INITCOMP
            ^prolog_poparg1             ;;; SS_POPARG1
            ^prolog_vedsetup            ;;; SS_VEDSETUP
            ^prolog_xsetup              ;;; SS_XSETUP
        },
;

/*
 *  Pop-11 syntax for calling Prolog
 */

vars prolog_trace_do = false;
    ;;; used by library(tracer)

;;; :- , ?-
;;;     evaluate a single Prolog goal

define lconstant do_one_goal with_props false;
    sysCOMPILE() -> ;
enddefine;

define syntax ?-;
    lvars term = prolog_readterm();
    if prolog_trace_do then
        prolog_maketerm(term, "prolog_trace_do", 1) -> term;
    endif;
    sysPUSHQ(term), sysPUSHQ(readenv), sysPUSH("false"),
        sysPUSH("ident prolog_query"), sysCALLQ(do_one_goal);
    ";" :: proglist -> proglist;
enddefine;

define syntax :-;
    lvars term = prolog_readterm();
    sysPUSHQ(term), sysPUSH("false"), sysPUSH("ident prolog_directive"),
        sysCALLQ(do_one_goal);
    ";" :: proglist -> proglist;
enddefine;

;;; prolog:
;;;     switch to subsystem "top"

define syntax prolog;
    unless iscaller(prolog_setup) then prolog_setup() endunless;
    prolog_setprolog();
    "top" -> sys_compiler_subsystem(`t`);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 26 1995
        Made prolog_xsetup register Prolog with the UI if it's there
--- Robert John Duncan, Aug  2 1993
        Changed to use prolog_file_type to get the default file extension
 */
