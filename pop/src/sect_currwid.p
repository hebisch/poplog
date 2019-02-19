/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/sect_currwid.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; -------- CONSTRUCT WORD IDENTIFIER FOR CURRENT SECTION ---------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'


;;; ----------------------------------------------------------------------

section $-Sys$-Sect;

define Cons_word_id(id, word, sect) -> wid;
    lvars wid, id, word, sect, string = word!W_STRING;
    if sect!SEC_LEVEL /== 0 then
        sect!SEC_PATH_NAME <> '$-' <> string -> string
    endif;
    conspair(word, sect) -> sect;
    Get_record(word_key) -> wid;
    id -> wid!W_IDENTIFIER;
    string -> wid!W_STRING;
    sect -> wid!W_DICT_NEXT;    ;;; mark as identifier word
    if iscompound(id) then wid -> word_idents(id) endif
enddefine;

    ;;; return the symbolic identifier for a word in the current section, i.e.
    ;;; a word representing the identifier currently associated with the word
define Curr_word_id(word);
    lvars sect, wid, word;
    unless isword(word!W_DICT_NEXT) then
        ;;; not dictionary word
        word
    elseif word_idents(word!W_IDENTIFIER) ->> wid then
        wid
    else
        ;;; find the section in which the id is defined
        current_section -> sect;
        while sect!SEC_SUPERSECT and not(list_assoc(word, sect!SEC_IDENTS)) do
            sect!SEC_SUPERSECT -> sect
        endwhile;
        chain(word!W_IDENTIFIER, word, sect, Cons_word_id)
    endunless
enddefine;

    ;;; For tokens to label identifiers in the VM
define Curr_pas_token(word);
    lvars sect, wid, word;
    unless vm_pas_mode == "popc" then chain(word, Curr_word_id) endunless;
    word -> wid;
    if ispair(word!W_DICT_NEXT)
    or (word_idents(word!W_IDENTIFIER) ->> wid) then
        ;;; existing word identifier
        fast_destpair(wid!W_DICT_NEXT) -> (word, sect)
    elseunless isword(word!W_DICT_NEXT) then
        ;;; not dictionary word
        return(word)
    else
        ;;; find the section in which the id is defined
        current_section -> sect;
        while sect!SEC_SUPERSECT
        and not(list_assoc(word, sect!SEC_IDENTS)) do
            sect!SEC_SUPERSECT -> sect
        endwhile
    endif;
    if sect!SEC_LEVEL == 0 then
        word
    elseif wid then
        wid
    else
        chain(word!W_IDENTIFIER, word, sect, Cons_word_id)
    endif
enddefine;

endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- John Gibson, Oct 26 1992
        Split Curr_pas_token off as a separate procedure
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
