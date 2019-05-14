/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/uses.p
 > Purpose:         Syntax for loading libraries/identifiers
 > Author:          John Gibson, Nov  9 1992 (see revisions)
 > Documentation:   HELP * USES, REF * LIBRARY/uses
 > Related Files:   LIB * USES_LIB_IDENTS
 */
compile_mode :pop11 +strict;

section;

    /*  Flags passed to uses_lib_idents */
lconstant macro (
    NOW         = 2:1e0,    ;;; do it now (i.e. normal compile if in POPC)
    BY_NAME     = 2:1e1,    ;;; uses ids by name (i.e. via quoted words)
    EXCL_LIB_ID = 2:1e2,    ;;; don't add library name id to idnames
);

sysunprotect("uses");

define syntax uses;
    lvars   item, libname, idnames, initflags = 0, flags, liblist = false,
            first = true;
    lconstant
        terminators     = [, ; ^termin],
        lib_terminators = [( ^^terminators];

    dlocal popnewline;

    if pop11_try_nextreaditem("-") then
        if (readitem() ->> item) == "now" then
            NOW
        elseif item == "by_name" then
            BY_NAME
        else
            mishap(item, 1, 'INVALID ITEM FOLLOWING uses-')
        endif -> initflags;
    endif;

    repeat
        initflags -> flags;

        if pop11_try_nextreaditem("[") then
            ;;; liblist expr
            pop11_need_nextreaditem("liblist") -> ;
            proglist_read_by("]", pop11_comp_expr_to) -> (, liblist)
        endif;

        if (pop11_try_nextreaditem("from") ->> item) or nextreaditem() /== "("
        then
            ;;; read lib name
            if item then flags || EXCL_LIB_ID -> flags endif;
            false -> libname;
            until fast_lmember(nextreaditem(), lib_terminators) do
                readitem() -> item;
                if libname then
                    libname sys_>< item
                elseif isword(item) then
                    if item == "$-" then
                        sys_read_path(item, false, false)
                    else
                        item        ;;; preserve the first word
                    endif
                else
                    nullstring sys_>< item
                endif -> libname;
                if (first ->> popnewline) and nextreaditem() == newline then
                    ";" -> hd(proglist)
                endif
            enduntil;
            false -> popnewline;
            unless libname then
                mishap(readitem(), 1, 'EXPECTING LIBRARY NAME')
            endunless;
            sysPUSHQ(libname);
            if liblist then
                liblist <> proglist -> proglist;
                pop11_comp_expr_to("]") ->
            else
                sysPUSHQ(false)
            endif
        else
            sysPUSHQ(false), sysPUSHQ(false)
        endif;
        false -> first;

        if pop11_try_nextreaditem("(") then
            ;;; identifier name list
            [%  repeat
                    sys_read_path(readitem(), false, false) -> item;
                    if not(isword(item)) or fast_lmember(item, terminators)
                    or item == ")" then
                        mishap(item, 1, 'EXPECTING IDENTIFIER NAME')
                    else
                        item
                    endif;
                    if (pop11_need_nextreaditem([, )]) ->> item) == "," then
                        pop11_try_nextreaditem(")") -> item
                    endif;
                    quitif(item)
                endrepeat;
            %]
        else
            []
        endif -> idnames;

        sysPUSHQ(idnames);
        sysPUSHQ(flags);
        sysCALL("uses_lib_idents");

        if (pop11_need_nextreaditem(terminators) ->> item) == "," then
            pop11_try_nextreaditem(tl(terminators)) -> item
        endif;
        quitif(item)
    endrepeat;

    ";" :: proglist -> proglist
enddefine;

sysprotect("uses");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 16 1994
        Protected uses
--- John Gibson, Jun  2 1993
        Made it allow a sectioned pathname as the libname/identifier
--- John Gibson, Nov  9 1992
        Completely rewritten
--- James Goodlet, Jan 16 1990 - reads whole line, not just next word.  This
        allows partial paths and filename extensions to be specified.
--- John Williams, Mar  8 1989 - Now a syntax word
--- John Williams, Aug  8 1988 - Now plants call to -useslib- (cf SFR 4183)
 */
