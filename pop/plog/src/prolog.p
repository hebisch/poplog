/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.all/plog/src/prolog.p
 > Purpose:         Compiles the Prolog system
 > Author:          Robert Duncan & Simon Nichols, Oct. 1987 (see revisions)
 */

compile_mode:pop11 +strict;

uses prolog_subsystem;

/*
 *  System ID
 */

lconstant
    TITLE   = 'Prolog',
    VERSION = '3.2',
;

/*
 *  Version banner
 */

constant prolog_banner =
    sprintf('%S (Version %S)', [^TITLE ^VERSION]);


;;; ========================================================================
;;;
;;; THE REST OF THIS FILE IS FOR INTERACTIVE LOADING ONLY
;;;
;;; ========================================================================

#_TERMIN_IF DEF POPC_COMPILING

;;; Messages printed on startup
vars prolog_messages = [];

;;; Debug mode
#_IF not(DEF prolog_debugging)
vars prolog_debugging = false;
#_ENDIF

/*
 *  Command-line option processing
 */

#_IF DEF mkimage_flags_table
;;; Image built using LIB * MKIMAGE

lvars srcdirs = [], helpdirs = [], teachdirs = [], libdirs = [];
lvars rootdir = false;
lvars arg, args = mkimage_flags_table("prolog");

define lconstant optarg(option, args);
    lvars option, args;
    if null(args) then
        mishap(option, 1, 'MISSING ARGUMENT TO OPTION');
    else
        dest(args);
    endif;
enddefine;

while not(null(args))
and isstring(front(args)) and isstartstring('-', front(args))
do
    dest(args) -> (arg, args);
    if arg = '-r' then
        optarg(arg, args) -> (rootdir, args);
    elseif arg = '-s' then
        optarg(arg, args) -> (arg, args);
        [^^srcdirs ^arg] -> srcdirs;
    elseif arg = '-h' then
        optarg(arg, args) -> (arg, args);
        [^^helpdirs ^arg] -> helpdirs;
    elseif arg = '-t' then
        optarg(arg, args) -> (arg, args);
        [^^teachdirs ^arg] -> teachdirs;
    elseif arg = '-l' then
        optarg(arg, args) -> (arg, args);
        [^^libdirs ^arg] -> libdirs;
    elseif arg = '-v' then
        optarg(arg, args) -> (arg, args);
        [^^prolog_messages ^arg] -> prolog_messages;
    elseif arg = '-d' then
        true -> prolog_debugging;
    else
        mishap(arg, 1, 'UNRECOGNISED OPTION');
    endif;
endwhile;

if rootdir then
    [^^srcdirs % rootdir dir_>< 'src/' %] -> srcdirs;
    [^^helpdirs % rootdir dir_>< 'help/' %] -> helpdirs;
    [^^teachdirs % rootdir dir_>< 'teach/' %] -> teachdirs;
    [^^libdirs % rootdir dir_>< 'lib/' %] -> libdirs;
endif;

[^^srcdirs ^^prolog_srcdirs] -> prolog_srcdirs;
[^^helpdirs ^^prolog_helpdirs] -> prolog_helpdirs;
[^^teachdirs ^^prolog_teachdirs] -> prolog_teachdirs;
[^^libdirs ^^prolog_libdirs] -> prolog_libdirs;

#_ENDIF

if prolog_debugging then
    '[DEBUGGING]' :: prolog_messages -> prolog_messages;
endif;

printf(prolog_banner, '\n;;; Compiling %S\n');
printf('\n;;; Source directories: ');
applist(prolog_srcdirs, spr);
nl(2);

;;; Start time of compilation
lvars start_time = systime();

;;; Ensure a sufficiently high memory limit
sysgarbage();
max(popmemused + 250000, popmemlim) -> popmemlim;

;;; List of source files:
lconstant sources = [
    'prolog.ph'
    'builtins.ph'
    'syntax.ph'
    'util.p'
    'macros.p'
    'itemise.p'
    'precedence.p'
    'parse.p'
    'write.p'
    'format.p'
    'io.p'
    'alias.p'
    'procedures.p'
    'dynamic.p'
    'predicate_valof.p'
    'predicate_has_clauses.p'
    'prolog_valof.p'
    'prolog_invoke.p'
    'prolog_exceptions.p'
    'prolog_eval.p'
    'predicate_record.p'
    'predicate_declare.p'
    'predicate_define.p'
    'predicate_abolish.p'
    'transform.p'
    'gencode.p'
    'prolog_compile.p'
    'prolog_subsystem_procedures.p'
    'inline.p'
    'controlpreds.p'
    'optcontrol.p'
    'modules.p'
    'errors.p'
    'operators.p'
    'commands.p'
    'declpreds.p'
    'basicpreds.p'
    'iopreds.p'
    'sortpreds.p'
    'findall.p'
    'dbpreds.p'
    'compile.p'
    'systempreds.p'
    'predicate_info.p'
    'statistics.p'
    'toplevel.p'
    'grammar.p'
    'expand_term.p'
    'spy.p'
    'prolog_error.p'
    'prolog_ui.p'
    'plogved.p'
    'obsolete.p'
];

;;; This allows switching from Pop-11 to Prolog without using subsystems
global constant macro
    PROLOG = [chainfrom(cucharin, pop11_compile, prolog_compile);],
;

;;; These have to be declared here to control the compilation of
;;; predicates in the system
global vars
    $-prolog_no_clauses = false,
    $-prolog_syspredicate = false,
    $-prolog_tags = true,
;

;;; Record warnings of undeclared variables
lvars warnings = false;

;;; Load the source files

define lconstant loadfile(file);
    lvars
        dir, file, dev = false;
    dlocal
        prolog_syspredicate = not(prolog_debugging),
        prolog_no_clauses   = not(prolog_debugging),
        prolog_tags         = prolog_debugging,
        pop_debugging       = prolog_debugging,
        popgctrace          = true,

        ;;; Make backslash an alphabeticiser
        % item_chartype(`\\`) % = 12
    ;

    define dlocal prwarning(word);
        lvars word;
        sysprwarning(word);
        printf(';;; LINE NUMBER: %p\n', [^poplinenum]);
        true -> warnings;
    enddefine;

    for dir in prolog_srcdirs do
        quitif(readable(dir dir_>< file) ->> dev);
    endfor;
    unless dev then mishap(file, 1, 'SOURCE FILE NOT FOUND') endunless;
    loadwarning(device_open_name(dev)); sysflush(pop_charout_device);
    subsystem_compile(dev, false);
    sysflush(pop_charerr_device);
enddefine;

applist(sources, loadfile);

;;; Clean up
sysflush(pop_charout_device);
procedure; dlocal popgctrace = true; sysgarbage(); endprocedure();
sysflush(pop_charerr_device);

;;; Check for undeclared variables
if warnings and not(prolog_debugging) then
    mishap(0, 'UNDECLARED VARIABLE(S) IN PROLOG SOURCES');
endif;

;;; Total time taken
printf((systime()-start_time)/100.0,
    '\n;;; PROLOG COMPILED, TIME = %p seconds\n');


unless prolog_debugging then
    applist([PROLOG prolog_messages prolog_debugging], syscancel);
endunless;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jul 17 1996
        New source file "prolog_ui.p"
--- Robert John Duncan, Aug  4 1993
        Disabled prolog_tags while building the system
--- Robert John Duncan, Jul 15 1993
        Changes to the list of source files. Increased the version number
        to 3.2 to reflect considerable reorganisation (mainly to support
        standalone compilation).
--- Robert John Duncan, Jul 14 1993
        Moved command-line option processing back into here from lib
        prolog_subsystem (it's only really relevant to this file).
--- John Williams, Jul 13 1993
        Now uses %S option to printf when printing "Compiling ..." message.
--- John Gibson, Jan 14 1993
        o Moved the whole section defining prolog_rootdir, _helpdirs, etc
          to lib prolog_subsystem.
        o Changed loadfile to use subsystem_compile.
--- Simon Nichols, May 22 1992
        Changed version from 3.0 to 3.1 (call/N, spy_action, format/[1,2]).
--- Robert John Duncan, Apr  9 1992
        New source file "format.p"
--- Robert John Duncan, Jun 18 1991
        Simplified search lists
--- Robert John Duncan, May 24 1991
        Interface to LIB MKIMAGE
--- Robert John Duncan, Apr 25 1991
        Changed assignment to -popmemlim- to take account of -popmemused-
        for layering on top of other subsystems.
--- John Gibson, Dec  7 1990
        Replaced r*ecordclass with defclass
--- Robert John Duncan, Nov  6 1990
        Added teach directories to the end of the help list and vice versa.
--- Simon Nichols, Nov  6 1990
        Changed -popdevX- to -pop_charX_device-.
--- Robert John Duncan, Oct  5 1990
        Changed the banner.
--- John Williams, Aug  6 1990
        Added -prolog_refdirs-
--- Robert John Duncan, Jul 26 1990
        Fixed argument processing to allow nullstring argument.
--- Simon Nichols, Jul 17 1990
        - Changes for use with new subsystem facilities. In particular, it
            longer creates a saved image.
        - Added "commands.p" to list of source files.
--- Rob Duncan, Apr  9 1990
        Removed references to -popdefineprocedure- etc. from -loadfile-.
        Changed initialisation of -prolog_section-.
--- Rob Duncan, Mar 16 1990
        Added calls to -sys_install_image- for VMS.
--- Rob Duncan, Oct 11 1989
        Added call to -prolog_setup- for when loaded interactively.
--- Rob Duncan, Aug  8 1989
        Major changes:
        moved in argument processing and saved-image construction from
        the "mkplog" command file (with many improvements);
        moved out global declarations to "prolog.ph";
        moved out start-up code to "startup.p";
        removed initialisations of -prolog_defaults-, most of which
        were no longer used: the useful ones have been distributed to
        their point of use;
        removed explicit change to section prolog: this is now done in
        each individual source file;
        changed the order of compilation of source files;
        added definition of macro "_PROLOG_" to indicate when this file
        is being loaded.
        Many other minor mods.
--- Andrew Casson, Aug  8 1989
        Exported ->/2 so that (Cond -> G) can work
        (see definition in controlpreds.p)
--- John Gibson, Jun  9 1989
        In -loadfile-, replaced dlocal assignments to -popconstants- and
        -pop_optimise- with pop_debugging = prolog_debugging.
--- Rob Duncan, Mar 10 1989
        Exported phrase/3 to fix bug in grammar rule expansion:
        see "grammar.p"
--- Rob Duncan, Sep  8 1988
        Exported -prolog_abolish/2- and -prolog_evaltrue/1-.
        Removed explicit calls to the garbage collector.
--- Rob Duncan, Jun 22 1988
        Moved default initialisation of variables defined in section prolog
        to after the global declarations.
--- Rob Duncan, Apr  7 1988
        Removed an erroneous "$-" prefix from section prolog (as prolog should
        be a subsection of the current section)
 */
