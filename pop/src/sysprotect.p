/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/sysprotect.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ----------- PROTECTING/UNPROTECTING IDENTIFIERS -----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'vmdefs.ph'

global constant
        procedure (ident_declare, isactive, isdefined, Sys$-Get_perm_ident)
    ;

;;; ----------------------------------------------------------------------

section $-Sys => sysprotect, sysunprotect;

define sysprotect(/* token */) with_nargs 1;
    ;;; allow weak declaration for POPC
    lvars id = Get_perm_ident((), vm_pas_mode == "popc");
    if VM_LOADED and vm_pas_mode then
        VM_WEAK valof("pas_declare_perm")(id, `P`)
    endif;
    unless id!ID_IDENTPROPS _bitst _:M_ID_PROTECT then
        ;;; avoids problems with non-writeable system constants
        id!ID_IDENTPROPS _biset _:M_ID_PROTECT -> id!ID_IDENTPROPS
    endunless
enddefine;

define sysunprotect(token);
    lvars idprops, token;
    dlocal pop_vm_flags = pop_vm_flags fi_|| VM_NOPROT_PVARS;
    if (identprops(token) ->> idprops) /== "undef" then
        if isactive(token) then
            conspair(idprops, isactive(token)) -> idprops
        endif;
        ;;; don't declare strongly if currently weak
        ident_declare(token, idprops,
                        if isdefined(token) then 0 else 2:10 endif)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
        Test for Popc now vm_pas_mode == "popc"
--- John Gibson, Oct  6 1992
        Changes for POPC
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May  2 1989
        sysunprotect now uses -pop_vm_flags- to unprotect perm identifiers.
--- John Gibson, Jan 29 1989
        Uses new procedure -Get_perm_ident-
--- John Williams, Mar 21 1988
        -sysunprotect- fixed for active variables
--- John Gibson, Mar 13 1988
        Moved out of ident.p
 */
