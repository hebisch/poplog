/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/lib/debug.p
 >  Purpose:        Interactive debugging aid.
 >  Author:         Aaron Sloman, Juy 1982 (see revisions)
 >  Documentation:
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

;;; Needs to be improved to make patterns work properly as keys

;;; Make this available with:    LIB DEBUG;      or USES DEBUG;

;;; An interactive debugging aid. Conditional break points with messages
;;; can be set with the macro DEBUG, and controlled by BUGON and BUGOFF.

;;; If NODEBUG is TRUE then calls of DEBUG are ignored by the compiler:
;;; I.e. the procedures compile as if they were not there (though compilation
;;; takes a bit longer.

;;;     DEBUG fred <expression>;        ;;; sets a break point
;;; fred could be a list.
;;; If switched on, this causes fred and the value of the expression to be
;;; printed out and  POPREADY called.

;;; The individual points are dynamically switched on or off with the macros
;;; BUGON fred joe silly;         BUGOFF fred tom;
;;; BUGON and BUGOFF can take patterns instead of just words.
;;; All debugging is switched on and off by making DEBUGGING true or false.
;;; I.e. this enables or disables already compiled calls of DEBUG

;;; BUGON always makes DEBUGGING true.
;;; BUGOFF; without any parameters makes it false.

;;; During the break the name and value of the expression are available
;;; in the variables  BUGNAME and BUGMESSAGE

;;; NB LISTREAD is used below, on the assumption that it returns a list,
;;; a vector, or the next item on proglist if it is not "[" or "{"

vars debugging debuglist;
true  -> debugging;
[] -> debuglist;

vars nodebug;
false -> nodebug;


define macro bugon;
    vars bug_key;
    true -> debugging;
    listread() -> bug_key;
    until bug_key == ";" do
        unless member(bug_key,debuglist) then
            bug_key::debuglist -> debuglist
        endunless;
        listread() -> bug_key;
    enduntil
enddefine;

define macro bugoff;
    vars bug_key;
    listread() -> bug_key;
    if bug_key == ";" then false -> debugging endif;
    until bug_key == ";" do
        delete(bug_key, debuglist) -> debuglist;
        listread() -> bug_key;
    enduntil
enddefine;


define constant 1 sysdebug(bug_key, bugmessage);
    ;;; This is the procedure which is actually used to do the break
    vars bug_pat;
    if debugging then
        for bug_pat in debuglist do
            if bug_key matches bug_pat then
                'BREAKING IN: ' >< bug_key =>
                bugmessage ==>
                popready(); return
            endif
        endfor
    endif
enddefine;

define macro debug ;
    vars bug_key;
    if nodebug then erase([%readtill(";")%]) else
        listread() -> bug_key;
        "bugon", bug_key, ";",              ;;; Make sure it is on the list
        ;;; Now plant a call of sysdebug
        if islist(bug_key) then
            "[", dl(bug_key), "]"
        else
            """, bug_key,  """
        endif,
        "sysdebug"      ;;; The second argument follows
    endif;
enddefine;

/* --- Revision History ---------------------------------------------------
--- Poplog System, Nov 29 1988 - Added "enddefine" at EOF (cf ALPHA 8)
 */
