/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/sect_ident.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---------------- IDENTIFIERS INSIDE SECTIONS -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'

;;; ----------------------------------------------------------------------

section $-Sys$-Sect;

    ;;; Deal with the section placement of of a new value for the identifier
    ;;; field of a word -- called by -Assign_perm_id-
define New_ident(word, new_id);
    lvars sect, old_id = word!W_IDENTIFIER, word, new_id, wid, dnext, idents,
        currsect = current_section;

    if new_id == old_id then
        ;;; not changing anything
        return
    elseif ispair(word!W_DICT_NEXT ->> dnext) then
        ;;; word identifier
        fast_destpair(dnext) -> sect -> word;
        weakref[word_idents] Set_1word(word, sect, currsect);
        word!W_IDENTIFIER -> old_id
    elseunless isword(dnext) then
        ;;; not in dictionary
        new_id -> word!W_IDENTIFIER;
        return
    else
        ;;; word in dictionary
        false -> dnext;
        currsect -> sect;
        unless sect!SEC_LEVEL /== 0
        and (list_assoc_val(word, sect!SEC_EXPORTS) ->> sect) then
            currsect -> sect
        endunless
    endif;
    new_id -> word!W_IDENTIFIER;

    if sect!SEC_LEVEL /== 0 then
        ;;; insert section local
        list_assoc(word, sect!SEC_IDENTS) -> idents;
        if old_id /== sect!SEC_LEVEL then
            unless idents then
                ;;; not saved on entry to sect and not already redefined
                ;;; in sect -- add old old_id to the restore list
                cons_assoc(word, old_id, sect!SEC_RESTORE) -> sect!SEC_RESTORE
            elseif issimple(new_id) then
                ;;; cancelling a section local - insert sect level instead
                sect!SEC_LEVEL -> word!W_IDENTIFIER;
                ;;; remove the assoc entry
                del_assoc_val(word, sect!SEC_IDENTS) -> sect!SEC_IDENTS ->
            endunless
        endif;
        ;;; put new id in the sect property
        unless issimple(new_id) then
            if idents then
                new_id -> fast_front(idents)
            else
                cons_assoc(word, new_id, sect!SEC_IDENTS) -> sect!SEC_IDENTS
            endif
        endunless
    endif;

    if dnext then
        weakref[word_idents] Set_1word(word, currsect, sect)
    endif;

    ;;; deal with any word identifier
    if ( (list_assoc_val(word, sect!SEC_WORD_IDENTS) ->> wid)
         or iscompound(old_id) and testdef word_idents
            and (weakref word_idents(old_id) ->> wid)
       )
    and ispair(wid!W_DICT_NEXT ->> dnext)
    and fast_front(dnext) == word and fast_back(dnext) == sect then
        if iscompound(wid!W_IDENTIFIER) then
            false -> weakref word_idents(wid!W_IDENTIFIER)
        endif;
        if iscompound(new_id) then
            ;;; attach new id to word identifier
            new_id -> wid!W_IDENTIFIER;
            wid -> weakref word_idents(new_id);
            del_assoc_val(word, sect!SEC_WORD_IDENTS)
                                                -> sect!SEC_WORD_IDENTS ->
        endif
    endif
enddefine;

endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
