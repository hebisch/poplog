/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/languages.pl
 > Purpose:         Prolog commands and POP-11 macros for changing subsystem
 > Author:          Simon Nichols, Jul 11 1990 (based on Laventhol, 1983) (see revisions)
 > Documentation:   HELP * LANGUAGES
 */

;;; Note: for compatibility reasons, these commands and macros are uppercase.

:- prolog_language(pop11).

section $-ploglib => POP11 TOP PROLOG;

global constant
    macro POP11,
    macro TOP,
    macro PROLOG,
;


;;; switch_subsystem_cmd:
;;;     switch subsystem

define lconstant switch_subsystem_cmd(ss);
    lvars ss;
    ss -> sys_compiler_subsystem(`c`);
enddefine;


;;; Prolog commands

switch_subsystem_cmd(%"pop11"%)  -> prolog_command("POP11");
switch_subsystem_cmd(%"top"%)    -> prolog_command("TOP");
switch_subsystem_cmd(%"prolog"%) -> prolog_command("PROLOG");


;;; POP-11 macros

prolog_command("POP11")  -> nonmac POP11;
prolog_command("TOP")    -> nonmac TOP;
prolog_command("PROLOG") -> nonmac PROLOG;

endsection; /* $-ploglib */

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 18 1993
        Replaced use of s*witch_subsystem_to with sys_compiler_subsystem(`c`)
        and got rid of prolog_lan*guage_stay (which no longer serves
        any purpose).
 */
