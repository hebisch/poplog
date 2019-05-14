/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/shadowclass.p
 > Purpose:         Syntax interface to shadowkeys
 > Author:          Roger Evans, Nov 15 1990 (see revisions)
 > Documentation:   REF * SHADOWCLASS
 > Related Files:   LIB * CONSSHADOWKEY, * SHADOWCLASS_DATA, * TYPESPEC_UTILS
 */
compile_mode:pop11 +strict;

section $-typespec_utils => shadowclass;

lconstant names = [shadowclass];
applist(names, sysunprotect);

uses
    typespec_utils,
    consshadowkey,
    shadowclass_data,
;


define syntax shadowclass;
    lvars   item, datword, key, spec, fldmode,
            global_p, decl_p, idprops, key_idname, sub, name,
            defname, f, declare_only = false, fast_pre, rclass,
            typename, attributes, attrlist, prefix;

    dlocal  pop_autoload = false;

    define lconstant do_syspassign(item, idname);
        lvars item, idname;
        if declare_only and decl_p == sysCONSTANT then
            ;;; do this so we don't leave constants marked as assigned
            item -> fast_idval(sys_current_ident(idname))
        else
            sysPASSIGN(item, idname)
        endif
    enddefine;

    define lconstant do_declare(idname, idprops);
        lvars idname, idprops;
        pop11_define_declare(idname, global_p, decl_p, idprops);
        if declare_only then
            ;;; redeclare for POPC
            declare_only -> pop_pas_mode;
            pop11_define_declare(idname, global_p, decl_p, idprops);
            false -> pop_pas_mode;
        endif
    enddefine;

    define lconstant p_assign(p, idname);
        lvars idname, p;
        if isref(idname) then fast_cont(idname) -> idname endif;
        returnunless(p and idname);
        do_declare(idname, idprops);
        do_syspassign(p, idname);
        if isinheap(p) then
            pop11_define_props(idname, idname, false) -> pdprops(p)
        endif;
        if (updater(p) ->> p) and isinheap(p) then
            pop11_define_props(idname, idname, true) -> pdprops(p)
        endif
    enddefine;

    dlocal pop_pas_mode;

    if (readitem() ->> item) == "-" and nextreaditem() == "declare" then
        readitem() -> , readitem() -> item;
        if (pop_pas_mode ->> declare_only) then false -> pop_pas_mode endif
    endif;

    get_declarator(item) -> global_p -> decl_p;
    if decl_p then readitem() -> item endif;

    false -> idprops;
    if item == "procedure" or item == 0 then
        item -> idprops, readitem() -> item
    endif;

    split_chars();
    checkr_name(item, false) -> datword;
    [%  repeat;
            if (nextitem()->>item) == "[" then
                listread();
            elseif islist(item) then
                readitem();   ;;; literal list
            else
                quitloop();
            endif;
        endrepeat;
    %] -> attrlist;
    if attrlist == [] then [[]] -> attrlist endif;  ;;; ensure default spec

    datword <> "_shadowkey" -> key_idname;
    if nextitem() == ";" then
        ;;; --- no spec - just adding access procs ---

        ;;; use pre-declared shadowkey to look for spec data
        sys_current_ident(key_idname) -> key;
        unless key and $-shadowkey$-isshadowkey(idval(key)->>key) then
            mishap(datword,1,'PRE-DEFINED SHADOWCLASS NOT FOUND');
        endunless;
        $-shadowkey$-shk_spec(key) -> spec;
        $-shadowkey$-shk_fldmode(key) -> fldmode;
        islist(spec) -> rclass;
    else
        ;;; --- spec present: must make shadowkey and all procs ---

        ;;; read typespec data
        read_typespec(false, false) -> (spec, fldmode, );

        islist(spec) -> rclass;

        ;;; declare/initialise key identifier
        do_declare(key_idname, 0);
        idval(sys_current_ident(key_idname)) -> key;
        unless popexecute and $-shadowkey$-isshadowkey(key)
        and $-shadowkey$-shk_spec(key) = spec and pop_pas_mode /== "popc"
        then
            consshadowkey(datword, spec, front(attrlist)) -> key;
            fldmode -> $-shadowkey$-shk_fldmode(key);
        endunless;
        do_syspassign(key, key_idname);

        p_assign(shadowclass_recognise(key),  "is" <> datword);
        p_assign(shadowclass_init(key),       "init" <> datword);
        p_assign(shadowclass_refresh(key),    "refresh" <> datword);
        p_assign(shadowclass_import(key),     "import" <> datword);
    endif;

    ;;; look for typespec request
    if (lmember("typespec", front(attrlist)) ->> typename)
    and back(typename) /== [] then
        ;;; declare typename in accordance with modifiers given
        ;;; and assign typespec value
        type_idname(front(back(typename))) -> typename;
        (decl_p or sysCONSTANT)(typename,0);
        if global_p then global_p(typename); endif;
        do_syspassign(conspair(fldmode, spec), typename);
    endif;


    for attributes in attrlist do
        if (lmember("prefix", attributes) ->> prefix)
        and back(prefix) /== [] then
            front(back(prefix));
        else
            "''"
        endif -> prefix;

        p_assign(shadowclass_cons(key,attributes), prefix <> "cons" <> datword);
        p_assign(shadowclass_dest(key,attributes), prefix <> "dest" <> datword);
        p_assign(shadowclass_fill(key,attributes), prefix <> "fill" <> datword);

        unless rclass then
            p_assign(shadowclass_subscr(key,attributes), prefix <> "subscr" <> datword);
        endunless;

        ;;; assign access procedures for recordclass/struct
        if rclass and fldmode then
            1 -> sub;
            repeat;
                quitif(fldmode == []);
                dest(fldmode) -> fldmode -> name;
                if isref(name) then fast_cont(name) -> name; endif;
                if isword(name) then
                    p_assign(shadowclass_field(sub,key,attributes),
                             prefix <> name);
                endif;
                sub fi_+ 1 -> sub;
            endrepeat;
        endif;
    endfor;

    if popclosebracket == popclosebracket_exec then sysEXECUTE() endif;
    need_nextitem([;]) :: proglist -> proglist
enddefine;

applist(names, sysprotect);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
        Test for Popc now pop_pas_mode == "popc"
--- John Gibson, May  5 1993
        shadowkey internal procedures now in section $-shadowkey
--- John Gibson, Mar 22 1993
        Added do_syspassign
--- Adrian Howard, Nov  2 1991 Installed Roger Evans fix which
    added "typespec" attribute and fldmode slot in key
    Made repeat declarations look for an existing shadowclass, not
    a typespec
--- Roger Evans, Jul  2 1991 added import procedures
--- Roger Evans, Feb 15 1991 fixed assignment to rclass so that just
        a new field accessor type can be built without requiring whole
        shadowclass definition
--- Roger Evans, Feb  6 1991 now passes entire first attr list to consshadowkey
--- Roger Evans, Jan 30 1991 fixed so that <empty> specs get the variable
        scoping right for pre-exisiting shadowkeys
--- Roger Evans, Jan 26 1991 now allows multiple attribute specs
--- Roger Evans, Jan  9 1991 revised to accomodate shadowkey changes
--- Roger Evans, Nov 27 1990 added init and refresh functions
--- Roger Evans, Nov 15 1990 popval was no good - now itemread's it!
--- Roger Evans, Nov 15 1990 now popval's optional props field
 */
