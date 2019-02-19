/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syscomp/syspop_mode.p
 > Purpose:
 > Author:          John Gibson, May 20 1989 (see revisions)
 */

/* -------------------------------------------------------------------------

                SWITCHING BETWEEN NORMAL AND SYSPOP MODE

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

vars
    syspop_mode = false,
    syspop_mode_established = false,
    ;

lconstant
    empty_prop      = newproperty([], 1, false, true),
    sect_saved_prop = newproperty([], 4, empty_prop, true),
    ;

lvars
    syspop_pop_ids  = false;

define init_syspop_idents();
    lvars id, word, n, procedure init_saved;
    dlocal current_section = pop_section;

    returnif(syspop_pop_ids);

    ;;; initial setup
    newproperty([], 16, false, true) ->> init_saved
                                    -> sect_saved_prop(pop_section);
    [%  fast_for n from 1 by 2 to datalength(syspop_ident_vec) do
            f_subv(n, syspop_ident_vec) -> word;
            f_subv(n fi_+ 1, syspop_ident_vec) -> id;
            if fast_subscrw(1,word) == `_` then
                id -> init_saved(word);
                copy(id) -> identof("\^_" <> allbutfirst(1,word))
            else
                conspair(word, id)
            endif
        endfast_for
    %] -> syspop_pop_ids
enddefine;

define set_syspop_mode(mode);
    lvars id, new_id, word, pair, mode;
    dlocal pop_pas_mode = false;

    define lconstant swap_idents(sect);
        lvars word, id, sect;
        dlvars procedure (old_saved, new_saved);
        dlocal current_section = sect;

        define lconstant do_swap(word, id);
            lvars word, id, new_id;
            returnif(datalength(word) fi_< 2 or fast_subscrw(1,word) /== `_`);
            if new_saved == empty_prop then
                newproperty([], 32, false, true) -> new_saved
            endif;
            id -> new_saved(word);
            if old_saved(word) ->> new_id then
                new_id -> identof(word);
                false -> old_saved(word)
            else
                ;;; so undef records are retained
                fast_idval(id), syscancel(word), -> fast_idval(id)
            endif
        enddefine;

#_IF pop_debugging
        returnif(sect == #_< current_section >_#);
#_ENDIF
        sect_saved_prop(sect) -> old_saved;
        empty_prop -> new_saved;
        fast_app_sect_idents(sect, do_swap);
        new_saved -> sect_saved_prop(sect);

        fast_for word, id in_property old_saved do
            id -> identof(word)
        endfast_for;

        fast_for sect in section_subsect(sect) do
            swap_idents(sect)
        endfast_for
    enddefine;

    returnif(syspop_mode == mode);

    dlocal current_section = pop_section;

    fast_for pair in syspop_pop_ids do
        fast_destpair(pair) -> new_id -> word;
        isdeclared(word) ->> id -> fast_back(pair);
        if new_id then
            new_id -> identof(word)
        else
            ;;; so undef records are retained
            fast_idval(id), syscancel(word), -> fast_idval(id)
        endif
    endfast_for;

    swap_idents(pop_section);
    mode -> syspop_mode
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1996
        Excluded "_" as a nonpop ident
 */
