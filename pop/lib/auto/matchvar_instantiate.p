/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/matchvar_instantiate.p
 > Purpose:         Instantiate matchvars in structure, copying if necessary
 > Author:          John Gibson, Dec 28 1995
 > Documentation:   REF * DATA
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

    /* Optional flags argument */
lconstant macro (
    PMVB_ONLY       = 2:1e0,    ;;; instantiate only if in pop_matchvars_bound
    UNINST_MISHAP   = 2:1e1,    ;;; mishap for uninstantiated
    UNINST_UNDEF    = 2:1e2,    ;;; replace uninstantiated with pop_undef or []
);

define matchvar_instantiate(x);
    dlvars ispairfront = false, needs_copy = false, flags = 0, org_x;

    define lconstant Instance(x);
        lvars key = datakey(x), n, save_nc;
        dlocal ispairfront;

        if class_field_spec(key) then
            needs_copy, false -> (save_nc, needs_copy);
            if key == pair_key then
                true -> ispairfront;
                #|  Instance(fast_front(x));    ;;; may return 0 or more
                    false -> ispairfront;
                    Instance(fast_back(x))
                |# -> n;
                if needs_copy then
                    return(until n == 1 do conspair(), n fi_- 1 -> n enduntil)
                endif
            else
                false -> ispairfront;
                #| appdata(x, Instance) |# -> n;
                if needs_copy then
                    if isvectorclass(x) then n endif;
                    return(fast_apply(class_cons(key)))
                endif
            endif;
            erasenum(n), x;
            save_nc -> needs_copy

        elseif key == matchvar_key then
            lvars (, id, , mvflags) = destmatchvar(x);
            if id and (flags &&=_0 PMVB_ONLY
                        or fast_lmember(id, pop_matchvars_bound))
            then
                if mvflags &&/=_0 1 then
                    ;;; list segment
                    if ispairfront then
                        true -> needs_copy;
                        return(dl(valof(id)))
                    endif
                else
                    true -> needs_copy;
                    return(valof(id))
                endif
            endif;
            ;;; anonymous matchvar or uninstantiated
            if flags &&/=_0 UNINST_MISHAP then
                mishap(org_x, 1, 'STRUCTURE CONTAINS UNINSTANTIATED MATCHVAR')
            elseif flags &&/=_0 UNINST_UNDEF then
                true -> needs_copy;
                return( unless mvflags &&/=_0 1 and ispairfront then
                            pop_undef
                        endunless)
            else
                x
            endif

        else
            x
        endif
    enddefine;

    if isinteger(x) then ((), x) -> (x, flags) endif;
    x -> org_x;

    Instance(x)
enddefine;

endsection;
