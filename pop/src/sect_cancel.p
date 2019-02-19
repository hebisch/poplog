/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/sect_cancel.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; -------------------- CANCEL A SECTION -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'

global constant
        procedure Sys$-Undef_cancel
    ;

;;; ----------------------------------------------------------------------

section $-Sys$-Sect => section_cancel;

define section_cancel(sect);
    lvars list, list2, id, word, wid, psect, sect, zap_pdprops = false;
    if isboolean(sect) or sect == "undef" then
        ;;; zap pdprops argument
        sect -> zap_pdprops -> sect
    endif;
    Checkr(sect) -> ;
    unless sect!SEC_SUPERSECT ->> psect then
        mishap(sect, 1, 'CAN\'T CANCEL TOP-LEVEL SECTION')
    endunless;

    returnif(vm_pas_mode == "popc");

    ;;; cancel all subsections
    applist(sect!SEC_SUBSECTS, section_cancel(%zap_pdprops%));

    ;;; remove sect from its supersect
    if (psect!SEC_SUBSECTS ->> list) == [] then
        return
    elseif fast_front(list) == sect then
        fast_back(list) -> psect!SEC_SUBSECTS
    else
        until (fast_back(list) ->> list2) == [] do
            if fast_front(list2) == sect then
                fast_back(list2) -> fast_back(list);
                quitloop
            else
                list2 -> list
            endif
        enduntil
    endif;

    ;;; remove any associated identifier words from ident_words property
    ;;; also zap pdprops of internal procedures if requested
    sect!SEC_IDENTS -> list;
    until list == [] do
        dest_assoc(list) -> list -> id -> word;
        if testdef word_idents
        and (weakref word_idents(id) ->> wid) then
            true -> wid!W_DICT_NEXT;    ;;; 'cancel' word identifier
            false -> weakref word_idents(id)
        endif;
        unless zap_pdprops == "undef" then
            Undef_cancel(word, id);
            if zap_pdprops then
                fast_idval(id) -> id;
                if isprocedure(id) and isinheap(id)
                and pdprops(id) == word then
                    false -> pdprops(id)
                endif
            endif
        ;;; else don't interfere with undef records
        endunless
    enduntil;

    sect!SEC_WORD_IDENTS -> list;
    until list == [] do
        dest_assoc(list) -> list -> wid -> ;
        true -> wid!W_DICT_NEXT     ;;; 'cancel' word identifier
    enduntil
enddefine;

endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- John Gibson, May 30 1989
        Allowed -section_cancel- to take optional 2nd arg of "undef", meaning
        don't interfere with undef records in cancelled identifiers
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
