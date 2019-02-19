/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/sect_subsect.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; -------------- MANIPULATING AND CONSTRUCTING SECTIONS --------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'

global constant
        section_key
    ;

;;; ----------------------------------------------------------------------

section $-Sys => section_supersect section_subsect;

define section_supersect();
    Sect$-Checkr()!SEC_SUPERSECT
enddefine;

define section_subsect(arg) with_nargs 3;
    lvars list, newsect, sect, name, create, pathname, arg;
    if datakey(arg) == section_key then
        ;;; return list of sub-sections
        return(copylist(arg!SEC_SUBSECTS))
    endif;
    arg -> create -> sect -> name;
    Sect$-Checkr(sect) -> ;
    Check_word(name);
    for sect!SEC_SUBSECTS->list step fast_back(list)->list till list==[] then
        if fast_front(list)!SEC_NAME == name then
            return(fast_front(list))
        endif
    endfor;

    ;;; not found - create new sub section record if create true
    sect!SEC_PATH_NAME <> ('$-' <> name!W_STRING) -> pathname;
    unless create then
        mishap(pathname, 1, 'NONEXISTENT SECTION')
    endunless;
    Get_record(section_key) -> newsect;
    name    ->  newsect!SEC_NAME;
    pathname ->  newsect!SEC_PATH_NAME;
    sect    ->  newsect!SEC_SUPERSECT;
    sect!SEC_LEVEL fi_+ 1
            ->  newsect!SEC_LEVEL;
    []      ->> newsect!SEC_RESTORE
            ->> newsect!SEC_IMPORTS
            ->> newsect!SEC_EXPORTS
            ->> newsect!SEC_SUBSECTS
            ->> newsect!SEC_IDENTS
            ->  newsect!SEC_WORD_IDENTS;
    newsect :: sect!SEC_SUBSECTS -> sect!SEC_SUBSECTS;
    newsect
enddefine;

endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
