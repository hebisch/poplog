/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/src/sect_current.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; ------------ GETTING/SETTING THE CURRENT SECTION ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'sections.ph'

global constant
        pop_section, Sys$-dict_chain_end
    ;

global vars
        Sys$- _inhibit_gc
    ;

;;; ----------------------------------------------------------------------

section $-Sys$-Sect => pop_default_section current_section;


lvars
    sect_current    = pop_section,
    default_sect    = pop_section,
    ;

define active pop_default_section;
    default_sect
enddefine;
;;;
define updaterof active pop_default_section with_nargs 1;
    Checkr() -> default_sect
enddefine;

    ;;; return the current section
define active current_section;
    sect_current
enddefine;

    ;;; change to a new section
define updaterof active current_section sect;
    lvars sect;

        ;;; leave the current section
    define lconstant Leave_section();
        lvars list, _word;
        ;;; restore all identifiers saved in the restore list
        fast_for list on sect_current!SEC_RESTORE do
            fast_front(list) -> _word;
            fast_back(list) -> list;
            fast_front(list) -> _word!W_IDENTIFIER
        endfast_for;
        ;;; return all the pairs on the restore list to the pair free list
        sys_grbg_list(sect_current!SEC_RESTORE);
        [] -> sect_current!SEC_RESTORE;

        ;;; return into supersect
        sect_current!SEC_SUPERSECT -> sect_current
    enddefine;

        ;;; enter a daughter section of the current section
    define lconstant Enter_section(sect);
        lvars list, id, sect, sysimps;
        dlvars restore_list, _level;

        define lconstant Save_dict(sysimps) -> restore_list;
            lvars word, restore_list = [], sysimps, _id, _cell, _lim;
            dlocal _inhibit_gc = true;  ;;; make sure nothing can interrupt this
            dictionary@RAW_WORDS -> _cell;
            dictionary@POPBASE{dictionary!RAW_SIZE} -> _lim;
            while _cell <@(w) _lim do
                _cell!(w)++ -> _cell -> word;
                until word == dict_chain_end do
                    unless issimple(word!W_IDENTIFIER ->> _id)
                    or _id!ID_PERM_FLAGS _bitst
                                        (_:M_PERM_GLOBAL _biset _:M_PERM_IMPORT)
                    or (_id <@(w) _system_end and fast_lmember(_id, sysimps))
                    then
                        cons_assoc(word, _id, restore_list) -> restore_list;
                        ;;; set the identifier field to the current sect
                        ;;; level
                        _level -> word!W_IDENTIFIER
                    endunless;
                    word!W_DICT_NEXT -> word
                enduntil
            endwhile
        enddefine;      /* Save_dict */

        define lconstant Restore_local_idents(idlist);
            lvars word, id, idlist;
            until idlist == [] do
                dest_assoc(idlist) -> idlist -> id -> word;
                unless word!W_IDENTIFIER == _level then
                    ;;; not saved on entry to sect
                    ;;; add old id to the restore list
                    cons_assoc(word, word!W_IDENTIFIER, restore_list)
                                                            -> restore_list
                endunless;
                id -> word!W_IDENTIFIER
            enduntil
        enddefine;      /* Restore_local_idents */

        ;;; enter the new section
        sect -> sect_current;
        sect!SEC_LEVEL -> _level;

        ;;; mark all identifiers in the cumulative import list
        [] -> sysimps;
        for sect!SEC_IMPORTS->list step fast_back(list)->list
        till list==[] then
            if iscompound(fast_front(list)!W_IDENTIFIER ->> id) then
                if id >=@(w) _system_end then
                    id!ID_PERM_FLAGS _biset _:M_PERM_IMPORT -> id!ID_PERM_FLAGS
                elseunless id!ID_PERM_FLAGS _bitst _:M_PERM_GLOBAL then
                    ;;; sys identifier may not be writeable, so can't set
                    ;;; PERM_IMPORT -- pass a separate list of these
                    id :: sysimps -> sysimps
                endif
            endif
        endfor;

        ;;; save all non-imported identifiers in the dictionary to create
        ;;; the initial restore list for the sect. All words whose
        ;;; identifiers have the PERM_GLOBAL flag set or are in -sysimps-
        ;;; are imported
        Save_dict(sysimps) -> restore_list;

        ;;; then unmark all identifiers in the cumulative import list
        for sect!SEC_IMPORTS->list step fast_back(list)->list
        till list==[] then
            fast_front(list)!W_IDENTIFIER -> id;
            if iscompound(id) and id >=@(w) _system_end then
                id!ID_PERM_FLAGS _biclear _:M_PERM_IMPORT -> id!ID_PERM_FLAGS
            endif
        endfor;
        sys_grbg_list(sysimps);

        ;;; finally, reset internal identifiers from the sect id
        ;;; property, saving current identifiers when necessary
        Restore_local_idents(sect!SEC_IDENTS);
        restore_list -> sect!SEC_RESTORE
    enddefine;      /* Enter_section */


    ;;; restore a section from an ancestor down
    define lconstant Enter_sect_path(sect);
        lvars sect;
        if sect /== sect_current then
            Enter_sect_path(sect!SEC_SUPERSECT);
            Enter_section(sect)
        endif
    enddefine;      /* Enter_sect_path */

    lvars target = Checkr(sect);
    until target == sect_current do
        if target!SEC_LEVEL fi_> sect_current!SEC_LEVEL then
            target!SEC_SUPERSECT -> target
        else
            Leave_section()
        endif
    enduntil;
    Enter_sect_path(sect)
enddefine;      /* updaterof current_section */


endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 14 1991
        Dictionary now a rawstruct rather than a string
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
