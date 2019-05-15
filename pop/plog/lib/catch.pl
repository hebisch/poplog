/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/catch.pl
 > Purpose:         Raising and handling exceptions
 > Author:          Simon Nichols, Jun 19 1991 (see revisions)
 > Documentation:   HELP * CATCH
 */

:- system_predicate catch/3, throw/1.

:- module prolog.

:- prolog_language(pop11).

compile_mode:pop11 +strict;

12 -> item_chartype(`\\`, readitem);    ;;; alphabeticiser

lvars
    status = undef,
;

define catch\/3(Goal, Tag, Recovery);
    lvars   Goal, Tag, Recovery;
    dlocal  status = false;
    SAVE;
    call\/1(Goal);
    ;;; Goal has failed or throw/1 has been called
    RESTORE; SAVE;
    if status and prolog_unify(status, Tag) then
        chain(Recovery, call\/1);
    else
        RESTORE;
        if status then chain(status, throw\/1) endif;
    endif;
enddefine;

define throw\/1(Tag);
    lvars Tag;
    if status == undef then
        mishap(Tag, 1, 'throw: NO MATCHING catch');
    endif;
    prolog_instance(prolog_generalise(Tag)) -> status;
    exitto(catch\/3);
enddefine;

:- prolog_language(prolog).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 19 1993
        Simplified to use call/1.
--- Simon Nichols, May 21 1992
        -prolog_predof- now takes predicate name and arity as arguments.
 */
