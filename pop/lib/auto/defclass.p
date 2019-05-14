/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/auto/defclass.p
 >  Purpose:        Declare typespecs for pop or external structures,
 >                  and define keys and access procedures for them.
 >  Author:         John Gibson, Apr 22 1990 (see revisions)
 >  Documentation:  REF *DEFSTRUCT, REF *KEYS
 */
compile_mode:pop11 +strict;

uses typespec_utils;


section $-typespec_utils => defexacc, defclass;

    /*  Defines these autoloadable syntax words (the file for the other
        just has 'uses defclass')
    */
lconstant names = [defexacc defclass];
applist(names, sysunprotect);


define lconstant def_access(extn);
    lvars   item, datword, key, spec, fldmode, attributes = [],
            extn, global_p, decl_p, idprops, key_idname,
            defname, f, declare_only = false, fast_pre, rclass;

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
    if extn and (item == "[" or fast_lmember(item, spec_starters)) then
        false -> datword;
        item :: proglist -> proglist
    else
        checkr_name(item, false) -> datword
    endif;

    if extn then
        lvars (checking, mode) = exacc_attributes();
        read_typespec(false, false) -> (spec, fldmode, defname);
        if checking then "''" else "fast_" endif -> fast_pre;
        deref_struct1(spec, fldmode) -> (spec, fldmode);
        if islist(spec) then
            ;;; defining struct field procedures
            true -> rclass;
            cons_access(fldmode, spec, checking, mode) -> key;
            if datword then datword <> "_" else fast_pre endif -> datword;
            if datword /== "''" then
                ;;; prefix each field name
                [% for f in fldmode do prefix_field(datword, f) endfor %]
                                                -> fldmode
            endif
        else
            false -> rclass;
            unless datword then
                if defname then
                    "_" <> defname
                elseif isvector(spec) then
                    "''"
                else
                    mishap(0, 'extern_access: CAN\'T DERIVE NAME FROM TYPE SPEC')
                endif -> defname;
                if ispair(spec) then
                    ;;; defining array subscriptor
                    "exsub"
                elseif isvector(spec) then
                    ;;; defining function applier
                    consword('exapp' sys_>< if spec(1) then spec(1)
                                            else 'N'
                                            endif)
                else
                    ;;; defining single access procedure
                    "exacc"
                endif <> defname -> datword;
                fast_pre <> datword -> datword
            endunless;
            p_assign(cons_access(fldmode, spec, checking, mode), datword)
        endif

    else
        if nextitem() == "[" then
            ;;; attribute(s)
            listread() -> attributes
        endif;
        read_typespec(true, false) -> (spec, fldmode, );

        ;;; record/vector class
        islist(spec) -> rclass;

        ;;; declare/initialise key identifier

        datword <> "_key" -> key_idname;
        do_declare(key_idname, 0);
        idval(sys_current_ident(key_idname)) -> key;
        unless popexecute and pop_pas_mode /== "popc"
        and iskey(key) and class_field_spec(key) = spec
        and sort(class_attribute(key)) = sort(attributes)
        then
            conskey(datword, spec, {%dl(attributes)%}) ->> key
                                                -> key_of_dataword(datword)
        endunless;
        do_syspassign(key, key_idname);

        p_assign(class_recognise(key),  "is" <> datword);
        p_assign(class_cons(key),       "cons" <> datword);
        p_assign(class_dest(key),       "dest" <> datword);

        unless rclass then
            p_assign(class_init(key),       "init" <> datword);
            p_assign(class_subscr(key),     "subscr" <> datword);
            p_assign(class_fast_subscr(key),"fast_subscr" <> datword);
        endunless

    endif;

    ;;; assign access procedures for recordclass/struct
    if rclass then
        appdata(key, procedure(p);
                        lvars p;
                        p_assign(p, dest(fldmode) -> fldmode)
                     endprocedure)
    endif;

    if popclosebracket == popclosebracket_exec then sysEXECUTE() endif;
    need_nextitem([;]) :: proglist -> proglist
enddefine;  /* def_access */

define global syntax defexacc;
    def_access(0)
enddefine;

define global syntax defexacca;
    def_access(2:10e8)      ;;; address mode flag
enddefine;

define global syntax defclass;
    def_access(false)
enddefine;

applist(names, sysprotect);

endsection;     /* $-typespec_utils */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
        Test for Popc now pop_pas_mode == "popc"
--- John Gibson, Mar 22 1993
        Added do_syspassign
--- John Gibson, May 28 1992
        Fixed test for whether key is the same as an existing one to include
        attributes.
--- John Gibson, Jan 18 1991
        Changed c*lass_spec to class_field_spec
--- John Gibson, Oct 19 1990
        Added extra attributes for -defexacc-.
--- John Gibson, Jul  4 1990
        Split off typespec procedures into lib typespec_utils, added
        support for external function apply
--- John Gibson, May  9 1990
        Changed order of first 2 args to -cons_access-
 */
