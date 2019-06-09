/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/ml.p
 > Purpose:         Compiles Poplog Standard ML
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */

compile_mode:pop11 +strict;

section;

uses ml_subsystem;


/*
 *  Identification
 */

lconstant
    BANNER              = 'Standard ML (Version %P.%P)',
    VERSION             = 2,
    REVISION            = 12,
;

global constant
    ml_version_number   = VERSION,
    ml_revision_number  = REVISION,
    ml_title_string = sprintf(BANNER, [% VERSION, REVISION div 10 %]),
    VED_LOADED =  isdefined("vedprocess");
;

/*
 *  Source files
 */

lconstant sourcefiles = [
    'util.p'
    'errors.p'
    'io.p'
    'lexitems.p'
    'lex.p'
    'syntax.p'
    'env.p'
    'parse.p'
    'types.p'
    'print_types.p'
    'data.p'
    'print_data.p'
    'type_errors.p'
    'modules.p'
    'typecheck.p'
    'trace.p'
    'exceptions.p'
    'vmml.p'
    'pregen.p'
    'gencode.p'
    'commands.p'
    'print_bindings.p'
    'compile.p'
    'external.p'
        ^(if VED_LOADED then
        'mlved.p'
        endif)
    'showdef.p'
    'startup.p'
    'StdTypes.ml'
    'StdValues.ml'
    'StdIO.ml'
    'Int.ml'
    'Real.ml'
    'String.ml'
    'List.ml'
    'Vector.ml'
    'Array.ml'
    'Combinators.ml'
    'Memory.ml'
    'Printer.ml'
    'Compile.ml'
    'System.ml'
    'PML.ml'
];

/*
 *  Compilation of source files
 */

;;; Record warnings of undeclared variables
lvars warnings = false;

;;; loadfile:
;;;     compiles a PML source file

define lconstant loadfile(file);
    lvars   dir, file, dev = false;
    dlocal  pop_debugging = ml_debugging,
            popgctrace = true;

    ;;; Make backslash an alphabeticiser
    dlocal  % item_chartype(`\\`) % = 12;

    define dlocal prwarning(word);
        lvars word;
        sysprwarning(word);
        printf(';;; LINE NUMBER: %p\n', [^poplinenum]);
        true -> warnings;
    enddefine;

    for dir in ml_srcdirs do
        quitif(readable(dir dir_>< file) ->> dev);
    endfor;
    unless dev then mishap(file, 1, 'SOURCE FILE NOT FOUND') endunless;
    loadwarning(device_open_name(dev)); sysflush(pop_charout_device);
    subsystem_compile(dev, false);
    sysflush(pop_charerr_device);
enddefine;

;;; Ensure a sufficiently high memory limit
sysgarbage();
max(popmemused + 250000, popmemlim) -> popmemlim;

;;; Load the sourcefiles
printf('\n;;; Compiling ' <> ml_title_string <> '\n');
printf('\n;;; Source directories: ');
applist(ml_srcdirs, spr);
printf('\n\n');
lconstant start_time = systime();
applist(sourcefiles, loadfile);

;;; Clean up
sysflush(pop_charout_device);
procedure; dlocal popgctrace = true; sysgarbage(); endprocedure();
sysflush(pop_charerr_device);

;;; Check for undeclared variables
;;; if warnings and not(ml_debugging) then
;;; mishap(0, 'UNDECLARED VARIABLE(S) IN PML SOURCES');
;;; endif;

;;; Done
printf('\n;;; PML COMPILED, TIME = %P seconds\n',
        [% (systime()-start_time)/100.0 %]);

endsection;     /*  $-  */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec 20 1994
        Revision 12 = function call tracing; new file "trace.p"
        Revision 11 = new treatment of wrapped/unwrapped values
--- Robert John Duncan, Nov 24 1994
        All source files now individually sectionised and exported names
        declared in the files in which they're defined
--- Robert John Duncan, Nov 17 1994
        Revision 10 = arrays and vectors now built-in; arrays made an
        equality type
--- Robert John Duncan, Oct 28 1994
        Removed declaration of ved_p*ml
--- Robert John Duncan, Oct 24 1994
        Revision 9 = changes to error reporting; new "type_errors.p" file
        for verbose explanation of common type errors
--- John Gibson, Jan 14 1993
        o Moved setup of ml_helpdirs etc to new lib ml_subsystem
        o Changed loadfile to use subsystem_compile
--- Robert John Duncan, Apr  6 1992
        Renamed directory lists for help, teach etc.
--- Robert John Duncan, Nov  5 1991
        Bug in -fast_chain- now fixed
--- Robert John Duncan, Sep 27 1991
        Temporarily disabled use of -fast_chain- because of possible VM bug.
--- Simon Nichols, Jun 26 1991
        Revision 8 = usage counts for values.
--- Robert John Duncan, May 24 1991
        Interface to LIB MKIMAGE
--- Robert John Duncan, Apr 25 1991
        Changed assignment to -popmemlim- to take account of -popmemused-
        for layering on top of other subsystems.
--- Robert John Duncan, Mar 22 1991
        Revision 7 = revisions to type unification and typechecking of
        function bindings; minor name changes.
--- Robert John Duncan, Feb 18 1991
        Revision 6 = fixed identifier status; new error messages; new
        lexical items
--- Robert John Duncan, Feb  4 1991
        Deleted "operators.p" source file: now handled in "env.p"
--- Robert John Duncan, Jan 21 1991
        Revision 5 = additional "pregen" pass prior to code generation
--- Robert John Duncan, Nov  6 1990
        Changed -popdevX- to -pop_charX_device-
--- Robert John Duncan, Oct 31 1990
        Added "PML.ml" source file.
--- Robert John Duncan, Oct 17 1990
        Trivial name changes.
--- Robert John Duncan, Oct  5 1990
        Simplified the banner for revised subsystem startup
--- Robert John Duncan, Oct  2 1990
        Revision 4 = local structure declarations.
--- Robert John Duncan, Sep 13 1990
        Revision 3 = Overflow exception.
        Fixed argument processing to check isstring.
--- Robert John Duncan, Aug  8 1990
        Changes for extended subsystems.
        Deleted code for making the saved image: now done by "mkssimage".
        Changed construction of the system banner and added a new revision
        number: revision 1 = tuple optimisation; revision 2 = subsystems.
        ML section now always lives in top section.
--- Rob Duncan, May 29 1990
        Added macro -pml- and command -ved_p*ml- (as synonyms for -ml- and
        -ved_ml-)
--- Rob Duncan, Apr 19 1990
        Made "System.ml" the last source file: now includes references to
        "Compile", "Memory" etc.
        Exported -ml_consref- as part of the external interface.
--- Rob Duncan, Mar 14 1990
        Removed -ml_immediate- flag: now available as a command-line
        option from -ml_startup-
--- Rob Duncan, Jan 10 1990
        Changed all declarations in top section to start "ml_";
        removed some redundant dynamic locals from -loadfile-;
        tidied up.
--- Rob Duncan, Oct 31 1989
        Added source files "Memory.ml", "Printer.ml", "Compile.ml"
--- Rob Duncan, Oct 26 1989
        Added -ml_teachdirs-, -ml_teachpath-
--- Rob Duncan, Sep  4 1989
        Renamed "Lists" and "Strings" to "List" and "String"; broke up
        "Numbers" into "Int" and "Real"; changed the order of loading.
--- Rob Duncan, Aug 30 1989
        Renamed "Useful.ml" to "Combinators.ml".
 */
