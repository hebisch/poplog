/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/syscomp/prologc.p
 > Purpose:         Compiles the standalone Prolog compiler
 > Author:          Robert John Duncan, Jul 13 1993 (see revisions)
 > Documentation:
 > Related Files:   C.all/plog/src/prolog.p
 */

compile_mode:pop11 +strict;

/*
 *  System ID
 */

lconstant
    TITLE   = 'Prolog Compiler',
    VERSION = '1.0',
;


/*
 *  Version banner
 */

global constant prolog_banner =
    sprintf('%S (Version %S)', [^TITLE ^VERSION]);


;;; ========================================================================

printf(prolog_banner, '\n;;; Compiling %S\n\n');

;;; Start time of compilation
lvars start_time = systime();

;;; Ensure a sufficiently high memory limit
sysgarbage();
max(popmemused + 400000, popmemlim) -> popmemlim;

;;; List of Prolog source files:
;;; (cut down from prolog.p)
lconstant prolog_sources = [
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
    'prolog_invoke.p'
    'prolog_prmishap.p'
    'prolog_eval.p'
    'predicate_record.p'
    'predicate_declare.p'
    'predicate_define.p'
    'predicate_abolish.p'
    'transform.p'
    'gencode.p'
    'prolog_compile.p'
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
    'systempreds.p'
    'grammar.p'
    'expand_term.p'
];

;;; List of POPC source files:
;;; (adapted from syscomp/make_popc.p)
lconstant popc_sources = [
    'lib.p'
    'sysdefs.p'
    'files.p'
    'wordflags.p'
    'asmout.p'
    'os_comms.p'
    'sysint.p'
    'syspop.p'
    'ident_labs.p'
    'genfloat.p'
    'genstruct.p'
    'genproc.p'
    'pas_optimise.p'
    'm_optimise.p'
    'm_trans.p'
    'vm_genp.p'
    'pas_interpret.p'
    'w_util.p'
    'syspop_mode.p'
    'do_asm.p'
    'popc_main.p'
];

;;; This allows switching from Pop-11 to Prolog without using subsystems
;;; (NB: POPC doesn't use pop11_compile, but goes direct to pop11_comp_stream)
global constant macro
    PROLOG = [chainfrom(cucharin, pop11_comp_stream, prolog_compile);],
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

;;; Load sources

define lconstant load_file(file, current_directory);
    lvars
        dir, file, dev = false;
    dlocal
        current_directory,
        prolog_syspredicate = not(DEF prolog_debugging),
        prolog_no_clauses   = not(DEF prolog_debugging),
        prolog_tags         = DEF prolog_debugging,
        pop_debugging       = DEF prolog_debugging,
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

    unless readable(file) ->> dev then
        mishap(file, 1, 'SOURCE FILE NOT FOUND');
    endunless;
    loadwarning(device_open_name(dev)); sysflush(pop_charout_device);
    subsystem_compile(dev, false);
    sysflush(pop_charerr_device);
enddefine;

applist(prolog_sources, load_file(% './' %));
applist(popc_sources, load_file(% '$popsrc/syscomp/' %));
;;; Customise Prolog for POPC
load_file('prologc_defs.p', 'syscomp/');

;;; Clean up
sysflush(pop_charout_device);
procedure; dlocal popgctrace = true; sysgarbage(); endprocedure();
sysflush(pop_charerr_device);

;;; Check for undeclared variables
if warnings and not(DEF prolog_debugging) then
    mishap(0, 'UNDECLARED VARIABLE(S) IN PROLOG SOURCES');
endif;

;;; Total time taken
printf((systime()-start_time)/100.0,
    '\n;;; PROLOG COMPILER COMPILED, TIME = %p seconds\n');

unless DEF prolog_debugging then
    applist([prolog_debugging], syscancel);
endunless;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  4 1993
        Disabled prolog_tags while building the compiler.
--- Robert John Duncan, Jul 16 1993
        Changed the directory paths assumed for source files.
 */
