/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/sect_wid.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; -------------- CONSTRUCTING WORD IDENTIFIERS -------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'sections.ph'

section $-Sys$-Sect;

constant
        procedure (Curr_word_id, Cons_word_id)
    ;

endsection;

;;; ----------------------------------------------------------------------

section $-Sys$-Sect => word_identifier;

define word_identifier(word, sect, context);
    lvars word, sect, id, wid, context, want_undef = false, psect;

    define lconstant Find_top_ident(word);
        lvars word, saved_id;

        define lconstant Find_top_ident_sect(word, sect);
            lvars sect, word;
            sect!SEC_LEVEL /== 0
            and (Find_top_ident_sect(word, sect!SEC_SUPERSECT)
                 or list_assoc_val(word, sect!SEC_RESTORE))
        enddefine;

        if Find_top_ident_sect(word, current_section) ->> saved_id then
            ;;; on a restore list above
            if iscompound(saved_id) then
                ;;; and restoring top-level identifier
                saved_id
            else
                false
            endif
        elseif iscompound(word!W_IDENTIFIER) then
            ;;; top-level identifier imported all the way
            word!W_IDENTIFIER
        else
            false
        endif
    enddefine;

    define lconstant Find_ident_above(word, sect) -> id -> owner_sect;
        lvars sect, id, word, owner_sect = sect!SEC_SUPERSECT;
        if owner_sect!SEC_LEVEL == 0 then
            Find_top_ident(word) -> id
        elseunless list_assoc_val(word, owner_sect!SEC_IDENTS) ->> id then
            Find_ident_above(word, owner_sect) -> id -> owner_sect
        endif;
        unless id and (id!ID_PERM_FLAGS _bitst _:M_PERM_GLOBAL
                        or fast_lmember(word, sect!SEC_IMPORTS)) then
            false -> id
        endunless
    enddefine;

    Check_word(word);
    Checkr(sect) -> ;
    if context == "undef" then true -> want_undef, false -> context endif;
    unless isword(word!W_DICT_NEXT) then
        ;;; word not in dictionary
        return(word)
    endunless;
    if list_assoc_val(word, sect!SEC_EXPORTS) ->> psect then
        ;;; use the section it's exported to
        psect -> sect
    endif;
    if sect!SEC_LEVEL == 0 then
        ;;; top-level
        Find_top_ident(word) -> id
    else
        ;;; not top level
        if not(list_assoc_val(word, sect!SEC_IDENTS) ->> id) and context then
            if current_section == sect then
                return( if iscompound(word!W_IDENTIFIER) then
                            Curr_word_id(word)
                        else
                            false
                        endif)
            else
                ;;; not local to this section - try to find it in context
                Find_ident_above(word, sect) -> id -> sect
            endif
        endif
    endif;
    if id then
        if word_idents(id) ->> wid then
            wid
        else
            Cons_word_id(id, word, sect)
        endif
    elseif want_undef then
        ;;; return undef identifier word
        lvars wids = sect!SEC_WORD_IDENTS;
        unless list_assoc_val(word, wids) ->> wid then
            Cons_word_id(0, word, sect) -> wid;
            cons_assoc(word, wid, wids) -> sect!SEC_WORD_IDENTS
        endunless;
        wid
    else
        false
    endif
enddefine;

endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
