/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.unix/plog/auto/date.pl
 *  Purpose:        get the time and date into a list
 *  Author:         Jonathan Laventhol, Jun 1983 (see revisions)
 *  Documentation:  HELP * DATE
 *  Related Files:  VMS version.
 */

;;;    ?- time(X).
;;;    X = [24, jun, 1983, 19, 22, 27]

:- prolog_language("pop11").

define subexplode(i, l, s);
vars n;
    for n from i to i + l - 1
    do  if s(n).isuppercode then s(n) + 32 else s(n) endif;
    endfor;
    ` `
enddefine;

define prolog_date();
vars r x;
    procedure(s);
    cons_with consstring
    {%  subexplode(9, 2, s);
        subexplode(5, 3, s);
        subexplode(datalength(s)-3, 4, s);
        subexplode(12, 2, s);
        subexplode(15, 2, s);
        subexplode(18, 2, s) %}
    endprocedure(sysdaytime()).stringin.incharitem -> r;
    [% until (r() ->> x) == termin do x enduntil%]
enddefine;

:- prolog_language("prolog").

date(List) :-
    prolog_eval(apply(valof(prolog_date)), List).

/*  --- Revision History ---------------------------------------------------
--- Jonathan Laventhol, May 3 1985 - bug fixed for any length timezone string.
--- Jonathan Laventhol, Sep 7 1983 - updated for unix style date string.
 */
