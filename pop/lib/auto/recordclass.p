/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/auto/recordclass.p
 >  Purpose:        Create new record/vector type objects
 >  Author:         John Gibson, May 29 1982 (see revisions)
 >  Documentation:  HELP *RECORDCLASS, HELP *VECTORCLASS, REF *KEYS
 */
compile_mode :pop11 +strict;


;;; recordclass <opt identspec> <dataword> <opt flags> <field1>, ... <fieldN> ;
;;;
;;; See HELP *RECORDCLASS for more details.

;;; vectorclass <opt identspec> <dataword> <opt flags> : <fieldspec> ;
;;;
;;; See HELP *VECTORCLASS for more details.


section;

define lconstant Do_rv_class(vclass, declare_only);
    lvars item, datword, key, spec = [], fields = [], flag_vec = {},
        vclass, global_p, decl_p, idprops, key_idname, key_id,
        declare_only
        ;

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
        do_declare(idname, idprops);
        sysPASSIGN(p, idname);
        if isinheap(p) then
            pop11_define_props(idname, idname, false) -> pdprops(p)
        endif;
        if (updater(p) ->> p) and isinheap(p) then
            pop11_define_props(idname, idname, true) -> pdprops(p)
        endif
    enddefine;

    dlocal pop_pas_mode;

    if declare_only then false -> pop_pas_mode endif;

    readitem() -> item;
    if item == "constant" then
        readitem() -> item, sysCONSTANT, sysGLOBAL
    elseif item == "vars" then
        readitem() -> item, sysVARS, sysGLOBAL
    elseif item == "lconstant" then
        readitem() -> item, sysLCONSTANT, false
    elseif item == "lvars" then
        readitem() -> item, sysLVARS, false
    elseif item == "dlvars" then
        readitem() -> item, sysDLVARS, false
    else
        false, sysGLOBAL
    endif -> global_p -> decl_p;

    false -> idprops;
    if item == "procedure" or item == 0 then
        item -> idprops, readitem() -> item
    endif;

    check_word(item ->> datword);       ;;; dataword

    ;;; look for flag spec
    if (nextreaditem() ->> item) == "{" or item == "[" then
        unless isvector(listread() ->> flag_vec) then
            {% dl(flag_vec) %} -> flag_vec
        endunless
    endif;

    ;;; read specs
    if vclass then
        ;;; vectorclass
        if nextreaditem() == ";" then
            "full" -> spec
        else
            if (readitem() ->> spec) == ":" then
                readitem() -> spec
            elseif spec == ":-" then
                -readitem() -> spec
            endif
        endif
    else
        ;;; recordclass
        until (readitem() ->> item) == ";" do
            nextif(item == ",");
            check_word(item);
            item :: fields -> fields;
            if (nextreaditem() ->> item) == ":" then    ;;; fieldspec
                readitem() ->, readitem()
            elseif item == ":-" then
                readitem() ->, -readitem()
            else
                "full"                          ;;; else default to full
            endif :: spec -> spec
        enduntil;
        rev(fields) -> fields;
        rev(spec) -> spec
    endif;
    ";" :: proglist -> proglist;

    ;;; declare/initialise key identifier
    datword <> "_key" -> key_idname;
    do_declare(key_idname, 0);
    sys_current_ident(key_idname) -> key_id;
    idval(key_id) -> key;
    unless popexecute and iskey(key) and class_field_spec(key) = spec
    and pop_pas_mode /== "popc" then
        conskey(datword, spec, flag_vec) ->> key -> key_of_dataword(datword)
    endunless;
    sysPASSIGN(key, key_idname);

    p_assign(class_recognise(key),  "is" <> datword);
    p_assign(class_cons(key),       "cons" <> datword);
    p_assign(class_dest(key),       "dest" <> datword);

    if vclass then
        p_assign(class_init(key),       "init" <> datword);
        p_assign(class_subscr(key),     "subscr" <> datword);
        p_assign(class_fast_subscr(key),"fast_subscr" <> datword);
    else
        appdata(key, procedure(p);
                        lvars p;
                        p_assign(p, dest(fields) -> fields)
                     endprocedure);
    endif;

    if popclosebracket == popclosebracket_exec then sysEXECUTE() endif
enddefine;

sysunprotect("recordclass");
sysunprotect("declare_recordclass");
sysunprotect("vectorclass");
sysunprotect("declare_vectorclass");

define global syntax recordclass;
    Do_rv_class(false, false)
enddefine;

define global syntax vectorclass;
    Do_rv_class(true, false)
enddefine;

define global syntax declare_recordclass;
    Do_rv_class(false, pop_pas_mode)
enddefine;

define global syntax declare_vectorclass;
    Do_rv_class(true, pop_pas_mode)
enddefine;

sysprotect("recordclass");
sysprotect("declare_recordclass");
sysprotect("vectorclass");
sysprotect("declare_vectorclass");

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
        Test for Popc now pop_pas_mode == "popc"
--- Roger Evans, Jan 28 1991
        fixed unprotection of declare_vectorclass
--- John Gibson, Jan 18 1991
        Changed c*lass_spec to class_field_spec
--- John Williams, Aug 30 1989
        Protected -recordclass- etc (cf FR 4290)
--- John Gibson, Jun 13 1989
        -conskey- now takes vector for key flags -- syntax in -recordclass-
        etc allows for flag names inside {...} or [...].
--- John Gibson, Apr 27 1989
        Rewritten to allow lconstant, etc, and to use -pop11_define_declare-
        and -pop11_define_props-. -recordclass- and -vectorclass- combined
        into one file with 'declare_' versions.
--- Roger Evans, Oct 10 1988
        Now allows [external] flag specification
 */
