/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/log.pl
 *  Purpose:        Simple terminal interaction logging in PROLOG and POP-11
 *  Author:         Unknown, ???
 *  Documentation:  HELP * LOG
 *  Related Files:
 */

;;; Terminal interaction is saved in PROLOG.LOG
;;; This works even when switching between languages

;;; The Prolog predicates 'log' and 'nolog',
;;; as well as the POP-11 operators 'LOG' and 'NOLOG'
;;; turn logging on and off.

:- prolog_language("pop11").

define global 9 LOG;
    if poplogfile then pr('\nAlready logging\n')
    else
        discout('prolog.log') -> poplogfile;
        pr('\nInteraction is being recorded in PROLOG.LOG\n')
    endif
enddefine;

define global 9 NOLOG;
    if isprocedure(poplogfile) then
        pr('End of logging -- back to non recorded interaction\n');
        poplogfile(termin);
        false -> poplogfile
    endif
enddefine;

:- prolog_language("prolog").

log :- prolog_eval(apply(valof('LOG'))).

nolog :- prolog_eval(apply(valof('NOLOG'))).
