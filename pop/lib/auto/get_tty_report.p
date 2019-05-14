/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:           C.all/lib/auto/get_tty_report.p
 > Purpose:        get a report from a terminal
 > Author:         Ben Rubinstein, Mar  6 1986 (see revisions)
 > Documentation:   HELP * POPPROCS /get_tty_report
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

;;; get a report from the terminal.  The args are:
;;;
;;;     trigger:    a string, sent to the tty to trigger the report
;;;     opener:     a string, the constant opening characters of the report
;;;     terminator: a character, the last character in the report
;;;
;;; The procedure will return false (if the report did not start according
;;; the specification of OPENER) or the part of the report between OPENER
;;; and TERMINATOR.
;;;
;;;
define get_tty_report(trigger, opener, terminator) -> string;
    lvars c, i, trigger, opener, terminator, string = false;
    sys_clear_input(poprawdevin);
    if      trigger.isprocedure
    then    trigger()
    else    appdata(trigger, rawcharout)
    endif;
    sysflush(poprawdevout);
    appdata(opener, procedure(c);
                        lvars c;
                        unless  rawcharin() == c
                        do      sys_clear_input(poprawdevin);
                                false;
                                exitfrom(get_tty_report)
                        endunless;
                    endprocedure);
    0 -> i;
    rawcharin() -> c;
    until   c == terminator
    do      c;
            i + 1 -> i;
            rawcharin() -> c
    enduntil;
    consstring(i) -> string
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Jun 12 1991
        Declared parameters as lvars (see bugreport davidy.43).
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevin- and -poprawdevout-,
        and -sys_purge_terminal- with -sys_clear_input-
 */
