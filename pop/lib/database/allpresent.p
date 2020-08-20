/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/database/allpresent.p
 >  Purpose:        tries to bind variables to satisfy a list of patterns
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * ALLPRESENT
 >  Related Files:
 */

;;;  ALLPRESENT takes a whole list of patterns and tries to find
;;;  way of binding variables so that all items are present in the
;;;  DATABASE

compile_mode :pop11 +oldvar;


section $-database => allpresent them ;

global vars them ;
vars procedure sysallpresent;

define global allpresent(XS);
    vars popmatchvars;
    [] -> popmatchvars;
    sysallpresent(XS);
enddefine;

;;;  SYSALLPRESENT does all the work. It finds a match for the
;;;  first element of the list and then calls itself recursively to
;;;  find a match for the reminder. The use of POPMATCHVARS is
;;;  important; When match encounters a variable (indicated by the
;;;  prefix "?" or "??") it either users the existing value (if the
;;;  variable is a member of POPMATCHVARS) or finds a value. If the
;;;  match found for the first item is no good, then MATCHVARS must
;;;  be reset to allow a second match for the first item

define sysallpresent(XS);
    vars DB, XX, SS;
    if XS == [] then [] -> them; return(true); endif;
    popmatchvars -> SS;
    database -> DB;
    dest(XS) -> XS -> XX;
    until DB == [] then
        SS -> popmatchvars;
        if sysmatch(XX, hd(DB)) then
            if sysallpresent(XS) then
                hd(DB) :: them -> them;
                return(true);
            endif;
        endif;
        tl(DB) -> DB;
    enduntil;
    return(false)
enddefine;

syscancel("sysallpresent");
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  1 1995
        Added +oldvar at top
 */
