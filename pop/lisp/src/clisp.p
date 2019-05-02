/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lisp/src/clisp.p
 > Purpose:         Building the Poplog Common Lisp system
 > Author:          John Williams, May 25 1987 (see revisions)
 > Documentation:   HELP * CLISP
 > Related Files:   LIB * CLISP, C.unix/com/mkclisp, C.vms/com/mkclisp.com
 */

section;

nprintf('Loading Poplog Common Lisp');
sysflush(popdevout, true);

sysgarbage();
max(popmemused + 8e5, popmemlim) -> popmemlim;


/********************** Initialise a few things *****************************/

global constant macro lisp_compile_mode
    =   [compile_mode:pop11 +defcon +defpdr +varsch -lprops;];

lisp_compile_mode;

uses lisp_subsystem;

global constant VED_LOADED
    =   isdefined("vedprocess");

global constant lisp_section
    =   section_subsect("lisp", pop_section, true);

global vars lisp_system_building
    =   false;


/********************* Special source file loading procedure ****************/

define global lisp_src_compile(file);
    lvars dirs;
    dlocal
        lisp_system_building    =   true,
        pop_debugging           =   false,
        popgctrace              =   true,
        popsyscall              =   1,
        subsystem_compile_warn  =   sysloadwarning,
        pop_buffer_charout      =   false,
        ;
    if islist(file) then
        file -> dirs;
        -> file
    else
        lispsrcdirlist -> dirs
    endif;
    if subsystem_libcompile(file, dirs) then
        sysflush(popdeverr, true);
        sysflush(popdevout, true);
    else
        mishap(file, 1, 'CLISP SOURCE FILE NOT FOUND')
    endif;
enddefine;


/********************** Load some library packages **************************/

pop11_compile('$usepop/pop/lib/include/vm_flags.ph');

uses_lib_idents("int_parameters", false, [], 0);

applist([
         'bitvectors.p'
         'random_states.p'
         'bits_in.p'
         'bits_out.p'
         'float_parameters.p'
         'format_print.p'
         'format_compile.p'
         ^(if VED_LOADED then 'ved_wiggle.p' endif)
        ], lisp_src_compile(% popuseslist %));


/********************** Load source files ***********************************/

applist(
    [
     'packages.p'
     'util.p'
     'new_2d_property.p'
     'symbols.p'
     'types.p'
     'clos.p'
     'subtypep.p'
     'specs.p'
     'functions.p'
     'apply.p'

     'numbers.p'
     'bitwise.p'
     'chars.p'
     'arrays.p'
     'seq.p'
     'hash.p'
     'lists.p'
     'strings.p'
     'equals.p'
     'clos-types.p'
     'typep.p'

     'streams.p'
     'pathnames.p'
     'files.p'
     'readutils.p'
     'itemise.p'
     'tmacs.p'
     'readtable.p'
     'umacs.p'
     'dmacs.p'
     'read.p'

     'pp-dispatch.p'
     'printcore.p'
     'pp.p'
     'print.p'
     'write.p'

     'tokens.p'
     'plant.p'
     'eval.p'
     'declare.p'

     'set.p'
     'setf.p'
     'blocks.p'
     'tags.p'
     'let.p'
     'multi.p'
     'progv.p'
     'the.p'
     'catch.p'
     'inlines.p'

     'lamlists.p'
     'destr.p'
     'defun.p'
     'defstruct.p'
     'methods.p'

     'magic-words.p'
     'time.p'
     'break.p'
     'errors.p'
     'inspect.p'
     'trace.p'
     'compile.p'
     'modules.p'
     'propsheet.p'
     'subsystem_procedures.p'
     'saved_images.p'

    ^(if VED_LOADED then
        'lispved.p',
        'ved_f.p',
        'ved_mcp.p'
      endif)

     'exportv.p'
     'export.p'
     'exporti.p'

     'defs.lsp'
     'cmacros.lsp'
     'packages.lsp'
     'types.lsp'
     'control.lsp'
     'mapping.lsp'
     'setf.lsp'
     'defpackage.lsp'
     'defstruct.lsp'
     'arrays.lsp'
     'lists.lsp'
     'ansi-loop.lsp'
     'numbers.lsp'
     'seq.lsp'
     'strings.lsp'
     'io.lsp'
     'pp.lsp'
     'clos.lsp'
     'conditions.lsp'
     'errors.lsp'
     'misc.lsp'
     'tidy.lsp'
    ], lisp_src_compile);


/* Finishing touches */

[] -> $-lisp$-constant_functions;

$-lisp$-user_package -> $-lisp$-package;

$-lisp$-setup_sort_preds();

nprintf('Loaded Common Lisp');
sysflush(popdevout, true);


endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 27 1999
        Prevented problem due to recompiling int_parameters
        Slightly increased popmemlim increment.
--- John Williams, Nov 29 1995
        Revised for new pretty printer.
--- John Williams, Jun  1 1995
        Added constant VED_LOADED (= testdef vedprocess).
--- John Williams, May  5 1995
        Added "ansi-loop.lsp".
--- John Williams, Feb 27 1995
        Changes for the Condition system (Steele 1990 ch 29).
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun 10 1994
        New file 'defstruct.lsp'.
--- John Williams, Jun  7 1994
        'defpackage.lsp' now compiled sooner.
--- John Williams, Apr 26 1994
        Changes for new CLOS source files, also renaming of 'lispinspect.p'.
--- John Williams, Nov 30 1993
        Added file 'defpackage.lsp'.
--- John Williams, Oct 14 1993
        Now uses sync argument to sysflush.
--- John Williams, Aug 27 1993
        Calls $-lisp$-setup_sort_preds at the end.
--- John Williams, Jul 12 1993
        Assigns [] to constant_functions at the end.
        Added lisp_compile_mode macro.
--- John Williams, Mar 18 1993
        Now uses sys_file_exists.
--- John Williams, Jan 18 1993
        Moved lisp, clisp, and setlisp to C.all/lisp/src/subsystem_procedures.p
--- John Gibson, Jan 18 1993
        Moved in lisp/clisp macros and setlisp from LIB LISP_SUBSYSTEM
        (clisp mustn't be in lisp_subsystem otherwise 'uses clisp' won't
        work).
        Replaced s*witch_subsystem_to with sys_compiler_subsystem(`c`)
--- John Williams, Jan 15 1993
        Revised for new subsystem implementation
--- John Williams, Dec  4 1992
        No longer needs to unprotect popfilename & poplinenum
--- John Williams, Apr 21 1992
        Removed duplicate call to load 'subtypep.p'
--- John Williams, Apr 13 1992
        Added -POP_HAS_PWM-, a constant used for defending PWM-related code
        with #_IF
--- John Williams, Feb 21 1992
        Added -lispsrcdirlist- (to fix buggy interaction between -lispsrclist-
        and new -subsystem_libcompile-)
--- Robert John Duncan, Apr 25 1991
        Changed assignment to -popmemlim-
--- John Williams, Dec 18 1990
        Added -lispteachlist-
--- John Williams, Oct  4 1990
        Changed -lisptitle- and -lispversion- for new -syssetup-
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
