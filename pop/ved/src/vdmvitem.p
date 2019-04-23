/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vdmvitem.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;-------------------- MOVING PAST AN ITEM -------------------------------

#_INCLUDE 'vddeclare.ph'


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vedvedrepeater, veddrepeater, vedrepeater,
                        vedmoveitem, vednextitem;

define vedvedrepeater() -> vchar;
    lvars vchar, char;
    if vedline fi_> vvedbuffersize then
        termin ->> vchar -> char
    elseif vedcolumn fi_> vvedlinesize then
        `\n` ->> vchar -> char;
        vedline fi_+ 1 -> vedline;
        vedsetlinesize();
        1 -> vedcolumn
    else
        fast_subscrvedstring(vedcolumn, vedthisline()) ->> vchar -> char;
        if iscompound(char) then fast_front(char) -> char endif;
        char fi_&& 16:FFFF -> char;
        if char == `\t` then
            vedtabright()
        else
            vedcolumn fi_+ 1 -> vedcolumn
        endif
    endif;
    char -> poplastchar
enddefine;

define veddrepeater();
    if ispair(dup(vedvedrepeater())) then fast_front() endif
enddefine;

define vedrepeater();
    if iscompound(dup(vedvedrepeater())) then
        if dup() == termin then return() else fast_front() endif
    endif;
    () fi_&& 16:FFFF
enddefine;


lvars item_rep = false;

    /*  Return next item, and move beyond it
    */
define vedmoveitem();
    lvars item, item_ref;

    define dlocal pop_exception_final(count, mess, idstring, sev);
        lvars count, mess, idstring, sev;
        if isstartstring('incharitem-', idstring)
        and isendstring(':syntax', idstring)
        and (Errmess_sprintf(count, mess, idstring, 1) ->> mess) then
            vederror(mess)
        else
            false
        endif
    enddefine;

    unless item_rep then
        incharitem(vedrepeater) -> item_rep
    endunless;
    fast_frozval(1, item_rep) -> item_ref;
    vedrepeater -> fast_cont(item_ref);
    item_rep();

    ;;; backspace as necessary
    fast_cont(item_ref) -> item;
    while ispair(item) do
        vedcharleft();
        back(item) -> item
    endwhile
enddefine;

    /*  Return next text item.
    */
define vednextitem();
    dlocal vedline, vedcolumn, vvedlinesize;
    vedmoveitem()
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 27 1997
        Made vedmoveitem create item_rep when first called.
--- John Gibson, Feb 15 1997
        String16 changes
--- John Gibson, Apr  3 1996
        Made pop_exception_final test for appropriate id-string
--- John Gibson, Feb  6 1996
        vedmoveitem uses local pop_exception_final instead of
        pr*mishap
--- John Gibson, Sep  8 1995
        Added vedvedrepeater
--- John Gibson, Jan 18 1992
        Added veddrepeater
--- John Gibson, Sep 10 1991
        Made vedrepeater deal with tabs correctly
--- John Gibson, Aug 16 1987
        Tidied up
 */
