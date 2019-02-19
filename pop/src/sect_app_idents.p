/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/sect_app_idents.p
 > Purpose:
 > Author:          John Gibson, Jan 11 1989
 > Documentation:   REF * SECTIONS
 */

;;; --------- APPLY A PROCEDURE TO ALL IDENTIFIERS IN A SECTION -------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'

global constant
        procedure fast_appdic
    ;


;;; ------------------------------------------------------------------------

section $-Sys$-Sect => fast_app_sect_idents;

define fast_app_sect_idents(sect, app_p);
    lvars id_assoc, sect;
    dlvars procedure app_p;
    Checkr(sect) -> ;
    Check_procedure(app_p);
    if sect == pop_section then
        dlocal current_section = pop_section;
        fast_appdic(procedure(word);
                        lvars word, id;
                        if iscompound(word!W_IDENTIFIER ->> id) then
                            chain(word, id, app_p)
                        endif
                    endprocedure)
    else
        sect!SEC_IDENTS -> id_assoc;
        until id_assoc == [] do
            _CHECKUSER;
            app_p(dest_assoc(id_assoc) -> id_assoc)
        enduntil
    endif
enddefine;

endsection;
