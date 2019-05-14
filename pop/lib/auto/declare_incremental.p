/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/declare_incremental.p
 > Purpose:         Tell POPC an identifier's value is incrementally linked
 > Author:          John Gibson, Jul  9 1992
 > Documentation:   REF *POPC (when available)
 */
compile_mode :pop11 +strict;

    /*
        declare_incremental
                property (perm_identifier = propexpr, ... );
                list[attributes] (perm_identifier, ... ),
                procedure[attributes] (perm_identifier, ... ),
    */

section;

lconstant macro (
    INCR_TYPE       = 2:11,     ;;; type
    INCR_WRITEABLE  = 2:1e2,    ;;; for a list, make writeable
    INCR_SUBLISTS   = 2:1e3,    ;;; for a list, generate per-file sublists
    INCR_PREC_SHIFT = 8,        ;;; precedence starts at bit 8
    ;;; types in INCR_TYPE
    INCRT_PROPERTY  = 0,
    INCRT_LIST      = 1,
    INCRT_PROCEDURE = 2,
);

sysunprotect("declare_incremental");

define global syntax declare_incremental;
    lvars flags, item;
    dlocal pop_autoload = false;

    repeat
        pop11_need_nextitem([property list procedure]) -> item;
        if item == "property" then
            INCRT_PROPERTY
        elseif item == "list" then
            INCRT_LIST
        else
            INCRT_PROCEDURE
        endif -> flags;

        if (nextitem() ->> item) == "[" or islist(item) then
            ;;; attributes specified -- currently, all pertain to lists/pdrs
            if flags == INCRT_PROPERTY then
                mishap(0, 'declare_incremental: UNEXPECTED ATTRIBUTES AFTER property')
            endif;
            procedure(proglist_state, flags) -> flags;
                lvars flags, item; dlocal proglist_state;
                until (itemread() ->> item) == termin do
                    if item == "writeable" then
                        flags || INCR_WRITEABLE -> flags    ;;; writeable list
                    elseif item == "sublists" then
                        flags || INCR_SUBLISTS -> flags     ;;; gen sublists
                    elseif item == "prec" then
                        pop11_need_nextitem("=") -> ;
                        if isinteger(itemread() ->> item) then
                            (item << INCR_PREC_SHIFT) || flags
                                    -> flags    ;;; precedence for this incr
                        else
                            mishap(item, 1, 'declare_incremental: EXPECTING INTEGER AFTER prec=')
                        endif
                    elseunless item == "," then
                        mishap(item, 1, 'declare_incremental: INVALID ATTRIBUTE ITEM')
                    endif
                enduntil
            endprocedure(proglist_new_state(listread()), flags) -> flags;
        endif;

        define lconstant read_ident(flags);
            lvars   flags, propexpr,
                    idname = sys_read_path(itemread(), false, false);
            dlocal pop_autoload = true;
            if flags && INCR_TYPE == INCRT_PROPERTY then
                pop11_need_nextitem("=") -> ;
                ;;; read property expression, then ensure macros are expanded
                ;;; so the expression is not dependent on them thereafter
                procedure(expr);
                    lvars expr;
                    dlocal  proglist_state = proglist_new_state(expr),
                            pop_autoload = false;
                    [% until null(proglist) do itemread() enduntil %];
                    sys_grbg_list(expr)
                endprocedure(exprread())
            else
                false
            endif -> propexpr;
            sysPUSHQ(idname);
            sysPUSHQ(flags);
            sysPUSHQ(propexpr);
            ;;; redefined by POPC
            sysCALL("pop_declare_incremental")
        enddefine;

        if pop11_try_nextitem("(") then
            repeat
                read_ident(flags);
                if (pop11_need_nextitem([, )]) ->> item) == "," then
                    pop11_try_nextitem(")") -> item
                endif;
                quitif(item)
            endrepeat
        else
            read_ident(flags)
        endif;
        if (pop11_need_nextitem([, ;]) ->> item) == "," then
            pop11_try_nextitem(";") -> item
        endif;
        quitif(item)
    endrepeat;

    ";" :: proglist -> proglist
enddefine;

sysprotect("declare_incremental");

endsection;
