/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/popstring.pl
 *  Purpose:        for getting POP-11 strings into commands
 *  Author:         Aaron Sloman, August 1982 (see revisions)
 *  Documentation:  HELP * POPSTRING
 *  Related Files:
 */

;;; example of use in prolog
;;;     prolog_eval(type(popstring '[pop.lib.auto]string.p'))
;;; in this case the string will not be a constant

:- prolog_language("pop11").

define popstring(_x);
    "consstring", "(",
    appdata(_x, procedure _x; _x, "," end);
    datalength(_x);
    ")"
enddefine;
;;;
popstring -> prolog_macro("popstring");

/*  --- Revision History ---------------------------------------------------
--- Jonathan Laventhol, July 1983 - altered for new system, and name changed
    so as not to clash.
 */
