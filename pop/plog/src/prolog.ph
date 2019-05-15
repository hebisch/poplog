/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/prolog.ph
 > Purpose:         Prolog: principal declarations
 > Author:          Rob Duncan, Jun 29 1989 (see revisions)
 */

/*
 *  Pop-11 global identifiers defined by the Prolog system
 */

section;

global constant
    procedure (
        prolog_body,
        prolog_clause,
        prolog_command,
        prolog_compile,
        prolog_generalise,
        prolog_getitem,
        prolog_head,
        prolog_initcomp,
        prolog_instance,
        prolog_invoke,
        prolog_macro,
        prolog_raw_valof,
        prolog_readline,
        prolog_readterm,
        prolog_readterm_to,
        prolog_setup,
        prolog_startup,
        prolog_toplevel_compile,
        prolog_valof,
        prolog_version,
        prolog_write,
    ),
    syntax (
        prolog
        :-
        ?-
    ),
    prolog_subsystem_procedures,
    top_subsystem_procedures,
;

global vars
    procedure (
        prolog_interrupt,
        prolog_toplevel_trap,
        prolog_invisible,
    ),
    prolog_no_clauses,
    prolog_read_prompt,
    prolog_syspredicate,
    prolog_trace_do,
    prolog_version_list,
    prologliblist,
;

;;; Ved commands
global vars
    procedure (
        ved_pophelp,
        ved_spy,
        ved_nospy,
    ),
;

;;; Bitwise operators for is/2
global constant
    procedure (
        \\\/,
        \/\\,
        \\,
    ),
;

;;; Test for whether VED is loaded
weak global constant procedure $-vedprocess;

endsection;     /* $- */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Moved out predicate declarations to "builtins.ph" and deleted some
        obsolete identifiers (now exported from "obsolete.p"). Removed
        most weak declarations.
--- Robert John Duncan, Jul 14 1993
        Exported prolog_readline which should replace the old safereadline.
--- Robert John Duncan, Apr 13 1992
        Added ^/2
--- Robert John Duncan, Apr 10 1992
        Added findall/3
--- Robert John Duncan, Apr  9 1992
        Added format/[1,2]
--- Simon Nichols, Apr  9 1992
        Added call/2 and call/3.
--- Robert John Duncan, Feb 11 1992
        Added prolog_error_handling/1
--- Robert Duncan, Jan 28 1992
        Added spy_action and related predicates and VED spy/nospy commands
--- Simon Nichols, Oct  8 1991
        Added close/1.
--- Simon Nichols, Aug 19 1991
        Renamed restart/0 prolog_restart/0 because of conflict with Flex
        KSL predicate of the same name.
--- Robert John Duncan, Jun 28 1991
        Added restart/0.
--- Simon Nichols, May 28 1991
        Added printq/1.
--- Simon Nichols, Oct 17 1990
        Added read/2 and write/2.
--- Simon Nichols, Sep 12 1990
        Added prolog_abolish_command/1.
--- Simon Nichols, Aug 31 1990
        Removed declarations for prolog_macros/1, ved/0 and ved/1.
--- Simon Nichols, Aug 30 1990
        Changed n*ote to weak.
--- Simon Nichols, Jul 19 1990
        Changed prolog_expand_clause/2 to term_expansion/2, for
        compatibility with other Prolog systems.
--- Simon Nichols, Jul 17 1990
        Added declaration for -prolog_command-.
--- Rob Duncan, Jun  6 1990
        Exported -ved_pophelp-
--- Simon Nichols, Apr 25 1990
        Added declaration for prolog_expand_clause/2
--- Rob Duncan, Apr  9 1990
        Added declaration for module/0
--- Simon Nichols, Feb  6 1990
        Added declarations for prolog_macros/1, ved/0 and ved/1 (defined in
        "systempreds.p").
--- Rob Duncan, Aug  8 1989
        Moved from "prolog.p" and reorganised. Made all declarations "weak".
 */
