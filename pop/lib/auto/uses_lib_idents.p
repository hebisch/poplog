/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/auto/uses_lib_idents.p
 > Purpose:         Load a library (only if its name is undefined)
 > Author:          John Gibson, Nov  9 1992 (see revisions)
 > Documentation:   REF * LIBRARY/uses_lib_idents
 > Related Files:   LIB * USES
 */
compile_mode :pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

section;

    /*  Flags -- replicated in uses */
lconstant macro (
    NOW         = 2:1e0,    ;;; do it now (i.e. normal compile if in POPC)
    BY_NAME     = 2:1e1,    ;;; uses ids by name (i.e. via quoted words)
    EXCL_LIB_ID = 2:1e2,    ;;; don't add library name id to idnames
);

define uses_lib_idents(libname, liblist, idnames, flags);
    lvars   libname, liblist, idnames, flags, idname, len, n,
            by_name = flags &&/=_0 BY_NAME;

    if libname then
        libname -> idname;

        ;;; If a given a pathname, extract name part (only allows for
        ;;; Unix-style pathnames). Note that -sys_fname_nam- can't be used
        ;;; here since it may truncate the name and/or (in VMS) translate
        ;;; chars to lowercase
        datalength(libname) -> len;
        if locchar_back(`/`, len, idname) ->> n then
            substring(n+1, len-n, idname) -> idname
        endif;
        if locchar(`.`, 1, idname) ->> n then
            substring(1, n-1, idname) -> idname
        endif;
        unless isword(idname) then consword(idname) -> idname endunless;

        unless flags &&/=_0 EXCL_LIB_ID then
            idname :: idnames -> idnames
        endunless
    endif;

    if pop_pas_mode == "popc" then
        if flags &&/=_0 NOW then
            ;;; POPC escape
            dlocal pop_pas_mode = false;
        else
            valof("popc_uses")(libname and idname, idnames, by_name);
            return
        endif
    endif;

    returnunless(libname);
    if pop_vm_flags &&/=_0 VM_NO_FAST then
        ;;; for a 'fast' version of a library, will load the non-fast
        ;;; version if there is one.
        valof("pop_nofast_trans")(idname, liblist or true) ->
    endif;

    returnif(isdefined(idname) and not(isundef(nonactive_valof(idname))));

    ;;; do the load, or make the ids autoloadable

    define lconstant do_load(word);
        lvars word;
        dlocal pop_default_type = '.p';
        loadlib(libname, if liblist then liblist endif);
        true
    enddefine;

    if by_name then
        for idname in idnames do
            do_load -> sys_autoload_action(idname)
        endfor
    else
        do_load(idname) ->
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 15 1997
        Now uses nonactive_valof when testing if ident is already defined.
        (Fixes problem with active variables reported by Aaron).
--- John Gibson, Sep 16 1996
        Added test for VM_NO_FAST.
--- John Gibson, Jul  3 1993
        Changed call to popc_uses to include library name
 */
