/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/sect_1word.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ------------- CHANGE TO A NEW SECTION FOR A SINGLE WORD ---------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'sections.ph'

;;; ----------------------------------------------------------------------

section $-Sys$-Sect;

define Set_1word(word, sect, currsect);
    lvars word, sect, target = sect, saved;
    dlvars currsect;

    ;;; restore a section from an ancestor down for the word
    define lconstant Enter_sect_path(word, sect);
        lvars id, word, sect, local_id;
        if sect == currsect then return endif;
        Enter_sect_path(word, sect!SEC_SUPERSECT);
        ;;; enter the new section
        word!W_IDENTIFIER -> id;
        if (list_assoc_val(word, sect!SEC_IDENTS) ->> local_id)
        or not( issimple(id) or id!ID_PERM_FLAGS _bitst _:M_PERM_GLOBAL
                or fast_lmember(word, sect!SEC_IMPORTS) )
        then
            cons_assoc(word, id, sect!SEC_RESTORE) -> sect!SEC_RESTORE;
            if local_id then local_id else sect!SEC_LEVEL endif
                                                    -> word!W_IDENTIFIER
        endif
    enddefine;      /* Enter_sect_path */

    until target == currsect do
        if target!SEC_LEVEL fi_> currsect!SEC_LEVEL then
            target!SEC_SUPERSECT -> target
        else
            if del_assoc_val(word, currsect!SEC_RESTORE)
                                -> currsect!SEC_RESTORE ->> saved then
                saved -> word!W_IDENTIFIER
            endif;
            currsect!SEC_SUPERSECT -> currsect
        endif
    enduntil;
    Enter_sect_path(word, sect)
enddefine;


endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
