/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/vm_global.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; ------------- DECLARING PERM IDENTIFIERS GLOBAL ----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'sections.ph'

global constant
        procedure Sys$-Get_perm_ident
    ;

global vars
        pop_syntax_only
    ;


;;; ----------------------------------------------------------------------

section $-Sys$-Sect => sysGLOBAL;

protected
define vars sysGLOBAL(word);
    lvars id, word, sect, _glob = _:M_PERM_GLOBAL, _prmflags;
    if isboolean(word) then
        ;;; optional boolean arg -- true = global, false = nonglobal
        unless word then _0 -> _glob endunless;
        () -> word
    endif;
    Check_word(word);
    returnif(VM_LOADED and VM_WEAK pop_syntax_only);
    Get_perm_ident(word, true) -> id;
    id!ID_PERM_FLAGS -> _prmflags;
    if id >=@(w) _system_end and (_prmflags _bimask _:M_PERM_GLOBAL) /== _glob
    then
        if _zero(_glob) then
            _prmflags _biclear _:M_PERM_GLOBAL -> id!ID_PERM_FLAGS
        else
            _prmflags _biset _:M_PERM_GLOBAL -> id!ID_PERM_FLAGS;
            if ispair(word!W_DICT_NEXT) and testdef current_section then
                ;;; word identifier -- ensure imported to current section
                ;;; if necessary
                fast_destpair(word!W_DICT_NEXT) -> (word, sect);
                weakref[current_section, word_idents]
                                Set_1word(word, sect, weakref current_section);
                weakref[current_section, word_idents]
                                Set_1word(word, weakref current_section, sect)
            endif
        endif
    endif;

    if VM_LOADED and vm_pas_mode then
        if _zero(_glob) then `N` else `G` endif -> _glob;
        VM_WEAK valof("pas_declare_perm")(id, _glob)
    endif
enddefine;

endsection;     /* $-Sys$-Sect */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 27 1992
        Gave it an optional boolean arg, true = global, false = nonglobal
--- John Gibson, Oct  6 1992
        Changed to call pas_declare_perm and to use Get_perm_ident
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
