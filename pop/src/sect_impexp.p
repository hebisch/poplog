/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sect_impexp.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; --------------- IMPORTING AND EXPORTING ----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'

section $-Sys;

global constant
        procedure (Get_perm_ident, Sect$-New_ident)
    ;

endsection;

;;; ----------------------------------------------------------------------

section $-Sys$-Sect => section_import, section_export;

    ;;; remove a word from a section restore list (return saved value)
define lconstant Remove_restore(word, sect);
    lvars word, sect;
    del_assoc_val(word, sect!SEC_RESTORE) -> sect!SEC_RESTORE
enddefine;

    ;;; make a word an import to the current section
define section_import(word);
    lvars sect, word;
    Check_word(word);
    if (current_section ->> sect)!SEC_LEVEL == 0 then
        mishap(word, 1, 'CAN\'T IMPORT WORD TO TOP LEVEL')
    endif;

    if VM_LOADED and vm_pas_mode then
        VM_WEAK valof("pas_section_import")(word)
    endif;

    if fast_lmember(word, sect!SEC_IMPORTS) then
        return
    elseif list_assoc(word, sect!SEC_IDENTS) then
        ;;; already local to section - cancel it
        sys_raise_exception(word, sect!SEC_PATH_NAME, 2,
                            '%IMPORT %P TO SECTION %P CANCELS LOCAL IDENTIFIER',
                            'vm-ident-perm-import:name-decl-ambig', `W`);
        New_ident(word, 0)
    endif;
    word :: sect!SEC_IMPORTS -> sect!SEC_IMPORTS;
    ;;; if it's on the restore list, remove it
    ;;; and restore whatever was saved
    if word!W_IDENTIFIER == sect!SEC_LEVEL then
        Remove_restore(word, sect) -> word!W_IDENTIFIER
    elseif issimple(word!W_IDENTIFIER) then
        ;;; switch to parent section temporarily to define it
        ;;; admittedly a bit slow
        sect!SEC_SUPERSECT -> current_section;
        Get_perm_ident(word, true) -> ;
        sect -> current_section
    endif
enddefine;

    ;;; make a word an export from the current section
define section_export(word);
    lvars word, sect, id, wid, nonex_sect, local_id;

    ;;; add an entry to the current export list
    ;;; containing the section wherein it should be defined
    ;;; i.e. the highest section that doesn't export it
    define lconstant Add_export(word, sect);
        lvars word, sect, nonex_sect;
        unless list_assoc_val(word, sect!SEC_SUPERSECT!SEC_EXPORTS)
                                                        ->> nonex_sect then
            ;;; exported only to next level up
            sect!SEC_SUPERSECT -> nonex_sect
        endunless;
        cons_assoc(word, nonex_sect, sect!SEC_EXPORTS) -> sect!SEC_EXPORTS;
        ;;; also add it onto the imports of the section
        unless fast_lmember(word, sect!SEC_IMPORTS) then
            word :: sect!SEC_IMPORTS -> sect!SEC_IMPORTS
        endunless
    enddefine;

    Check_word(word);
    if (current_section ->> sect)!SEC_LEVEL == 0 then
        mishap(word, 1, 'CAN\'T EXPORT WORD FROM TOP LEVEL')
    endif;

    if VM_LOADED and vm_pas_mode then
        VM_WEAK valof("pas_section_export")(word)
    endif;

    if list_assoc(word, sect!SEC_EXPORTS) then return endif;
    list_assoc_val(word, sect!SEC_IDENTS) -> local_id;
    if list_assoc_val(word, sect!SEC_RESTORE) ->> id then
        if iscompound(id) then
            ;;; defined in section above
            if local_id then
                ;;; local one will replace one above
                sys_raise_exception(word, sect!SEC_PATH_NAME, 2,
                        '%EXPORT %P FROM SECTION %P CANCELS OUTER IDENTIFIER',
                        'vm-ident-perm-export:name-decl-ambig', `W`);
                ;;; cancel the identifier above
                Set_1word(word, sect!SEC_SUPERSECT, sect);
                New_ident(word, 0);
                Set_1word(word, sect, sect!SEC_SUPERSECT);
                ;;; remove from the restore list
                Remove_restore(word, sect) ->
            else
                ;;; import the one from above
                section_import(word);
                Add_export(word, sect);
                return
            endif
        else
            ;;; remove from the restore list
            Remove_restore(word, sect) ->
        endif
    endif;

    if local_id then
        ;;; already locally defined
        if testdef word_idents and weakref word_idents(local_id) ->> wid then
            false -> weakref word_idents(local_id)
        endif;
        New_ident(word, 0);
        Add_export(word, sect);
        ;;; restore what was local identifier, now exported
        New_ident(word, local_id);
        if wid then wid -> weakref word_idents(local_id) endif
    else
        del_assoc_val(word, sect!SEC_WORD_IDENTS) -> sect!SEC_WORD_IDENTS -> ;
        Add_export(word, sect)
    endif
enddefine;


endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1996
        Uses sys_raise_exception for warnings
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
