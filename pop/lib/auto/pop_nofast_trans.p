/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/pop_nofast_trans.p
 > Purpose:         Translate 'fast' identifiers to non-fast
 > Author:          John Gibson, Sep 16 1996
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

;;; Words that start with these strings are assumed to be fast, and the
;;; corresponding word without the string is assumed to be the slow version
lconstant fast_prefixes = ['fi_' 'fast_'];

;;; These have to be left alone because of semantic differences between the
;;; fast and slow versions
lconstant exclusions = [% "'fast_bagof/3'", "'fast_setof/3'" %];

lvars tried_loading = [];

    /*  This is called for names defined as perm identifiers by
        sys_use_current_ident and sys_current_ident when
        given 2nd argument true and the flag VM_NO_FAST is set in
        pop_vm_flags (islib false).

        It is also called by uses_lib_idents when VM_NO_FAST is set
        (islib true or a library list).
    */
define vars pop_nofast_trans(name, islib);
    lvars prefix, slowname = false, id;

    returnif(lmember(name, exclusions)) (false);

    for prefix in fast_prefixes do
        if isstartstring(prefix, name) then
            allbutfirst(datalength(prefix), name) -> slowname;
            quitloop
        endif
    endfor;
    returnunless(slowname) (false);

    if islib then
        ;;; library
        returnif(isdefined(slowname) and not(isundef(valof(slowname))))
                                                (slowname);
        returnif(lmember(slowname, tried_loading)) (false);
        dlocal pop_default_type = '.p', subsystem_compile_warn = erase;
        returnif(subsystem_libcompile(slowname,
                if islist(islib) then islib else popuseslist endif))
                        (slowname)
    else
        ;;; identifier
        returnif(isdeclared(slowname)) (slowname);
        returnif(lmember(slowname, tried_loading)) (false);
        returnif(sys_autoload(slowname) and isdeclared(slowname)) (slowname)
    endif;
    slowname :: tried_loading -> tried_loading;
    false
enddefine;

endsection;
