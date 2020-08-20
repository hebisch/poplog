/* --- Copyright University of Birmingham 2005. All rights reserved. ------
 > File:            $usepop/pop/lib/database/forevery.p
 > Purpose:         Redefine forevery not to use "instance"
                    Avoid clash with objectclass instance
 > Author:          Aaron Sloman, Jan 19 1997 (see revisions)
 > Documentation:
 > Related Files:
 */

/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/database/forevery.p
 >  Purpose:        takes a list of patterns and tries to satisfy all.
 >  Author:         A. Sloman circa 1982 (see revisions)
 >  Documentation:  HELP * FOREVERY
 >  Related Files:  LIB * FOREACH, HELP * FOREACH, HELP * DATABASE
 */

;;; FOREVERY is to ALLPRESENT as FOREACH is to PRESENT

;;;  TRYALL takes a whole list of patterns and a database and tries to find
;;;  way of binding variables so that all items are present in the
;;;  DATABASE after finding one, it suspends the current process, which
;;;  can be resumed later.

uses pattern_instance;

section $-database => them nonmac forevery nonmac endforevery starttryall
    ;

uses database;

section forevery => nonmac forevery nonmac endforevery them starttryall;

global vars them;
vars procedure systryall ;

vars FOREVERY_PLIST;

define tryall(FOREVERY_PLIST,database);
    ;;; must be dynamic variables
    dlocal popmatchvars, FOREVERY_PLIST, database;
    [] -> popmatchvars;
    systryall(FOREVERY_PLIST);
    ksuspend(false,1)
enddefine;

;;;  SYSTRYALL does all the work. It finds a match for the
;;;  first element of the list and then calls itself recursively to
;;;  find a match for the remainder. The use of POPMATCHVARS is
;;;  important; When match encounters a variable (indicated by the
;;;  prefix "?" or "??") it either users the existing value (if the
;;;  variable is a member of POPMATCHVARS) or finds a value. If the
;;;  match found for the first item is no good, then MATCHVARS must
;;;  be reset to allow a second match for the first item.
;;;  If the recursion ever terminates with FOREVERY_PL empty, then a complete match
;;;  has been found. An instance of the list of patterns is built and assigned
;;;  to THEM, and the current process is suspended. If it is RUNPROC again, it
;;;  go back up the recursive stack and try again.

define systryall(FOREVERY_PL);
    lvars FOREVERY_PL, FOREVERY_DB, FOREVERY_X, FOREVERY_SS;
    if FOREVERY_PL == [] then
        pattern_instance(FOREVERY_PLIST) -> them;
        suspend(true,1);return(false)
    else
        popmatchvars -> FOREVERY_SS;
        database -> FOREVERY_DB;
        dest(FOREVERY_PL) -> FOREVERY_PL -> FOREVERY_X;
        until null(FOREVERY_DB) do
            FOREVERY_SS -> popmatchvars;
            if sysmatch(FOREVERY_X, fast_front(FOREVERY_DB)) then
                systryall(FOREVERY_PL);
            endif;
            fast_back(FOREVERY_DB) -> FOREVERY_DB;
        enduntil;
    endif
enddefine;


;;; STARTTRYALL creates a process, with a pattern list, a database and TRYALL
;;; as the procedure

define global starttryall(FOREVERY_PL,database);
    lvars FOREVERY_PL;
    consproc(FOREVERY_PL,database,2,tryall)
enddefine;

;;;  FOREVERY [........] DO <actions> ENDFOREVERY
;;; becomes, roughly:
;;;
;;;  VARS %V;
;;;  STARTTRYALL([.........],database) -> %V;
;;;  WHILE RUNPROC(V, 0) DO <actions> ENDFOREVERY
;;;
;;; and
;;;  FOREVERY [........] IN <list> DO <actions> ENDFOREVERY
;;; becomes, roughly:
;;;  VARS %V;
;;;  STARTTRYALL([.........],<list>) -> %V;
;;;  WHILE RUNPROC(V, 0) DO <actions> ENDFOREVERY
;;;
global vars syntax endforevery;

lvars oldpopconstruct=popconstruct;
true -> popconstruct;      ;;; make lists compile as constants

define global syntax forevery;
    lvars FOREVERY_ENDLAB, FOREVERY_LAB, FOREVERY_VAR, _x;
    pop11_loop_start(sysNEW_LABEL() ->> FOREVERY_LAB);
    pop11_loop_end(sysNEW_LABEL() ->> FOREVERY_ENDLAB);
    sysNEW_LVAR() -> FOREVERY_VAR;
    pop11_comp_expr_to([do then in]) -> _x;
    if _x == "in" then
        pop11_comp_expr_to([do then]) ->
    else
        sysPUSH("database")
    endif;
    sysCALL("starttryall");
    sysPOP(FOREVERY_VAR);
    sysLABEL(FOREVERY_LAB);
    sysPUSHQ(0);
    sysPUSH(FOREVERY_VAR);
    sysCALL("runproc");
    sysIFNOT(FOREVERY_ENDLAB);
    pop11_comp_stmnt_seq_to([endforevery {close}]) -> ;
    sysGOTO(FOREVERY_LAB);
    sysLABEL(FOREVERY_ENDLAB)
enddefine;

oldpopconstruct -> popconstruct;
section_cancel(current_section);
endsection;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 11 2005
        inserted braces round close, and moved into system.
--- Aaron Sloman, Jan 20 1997
    made to use pattern_instance, not instance, to prevent clashes.
--- Aaron Sloman, Jun 22 1986
    comments corrected, lvars used where appropriate.
*/
