/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptVal.p
 > Purpose:         Syntax for resource accessing/updating
 > Author:          John Gibson, Sep  6 1992 (see revisions)
 > Documentation:   REF *XPT_RESOURCE
 */
compile_mode:pop11 +strict;

uses typespec_utils;


section $-typespec_utils => XptVal;

sysunprotect("XptVal");

lconstant macro (
    TYPED_ARG_STRING    = 2:1e0,
    TYPED_ARG_INTEGER   = 2:1e1,

    DEFAULT_TYPESPEC    = ["int"],
    );

lconstant
    typed_args = [string ^TYPED_ARG_STRING integer ^TYPED_ARG_INTEGER];


define global syntax XptVal;
    lvars   v, n, item, spec, name, checking, mode, org_spec,
            w, var, optval, type_flags, l, len_count = false,
            old_autoload = pop_autoload, updating = pop_expr_update;
    dlvars  value_list, count = 0;
    dlocal  pop_new_lvar_list, pop_autoload;

    lconstant macro (NAME = 1, SPEC = 2, FLAGS = 3, OPT = 4, VAR = 5);

    exacc_attributes() -> (checking, mode);
    if mode &&/=_0 2:1e9 then
        mishap(0, 'ADDRESS MODE (@) NOT VALID FOR XptVal')
    endif;

    if pop11_try_nextitem("(") then
        ;;; expression
        pop11_comp_expr_to(")") -> ;
        if updating then sysPOP(sysNEW_LVAR() ->> w) endif
    else
        sys_read_path(itemread(), false, false) -> w;
        unless updating then sysPUSH(w) endunless
    endif;

    pop11_need_nextitem("(") -> ;

    ;;; read resource names, optional typespecs and typing flags
    false -> item;
    [%  until item == ")" do
            if (itemread() ->> name) == "%" then
                ;;; expression for resource name (n.b. includes the final "%")
                proglist_read_by("%", pop11_comp_expr_to) -> (, name)
            else
                check_string(name)
            endif;

            false -> pop_autoload;
            read_typespec(false, true) -> (spec, , );
            false -> optval;
            0 -> type_flags;
            while pop11_try_nextitem("<") do
                ;;; typing flags
                true -> len_count;
                if pop11_need_nextitem([OPT TYPE]) == "OPT" then
                    ;;; optional arg
                    if pop11_try_nextitem("=") then
                        ;;; identifier/value used for no value
                        if isword(itemread() ->> optval) then
                            checkr_name(sys_read_path(optval, false, false),
                                            false) -> optval
                        endif
                    else
                        "false"
                    endif -> optval;
                    pop11_need_nextitem(">") ->
                else
                    ;;; typed arg
                    pop11_need_nextitem("=") -> ;
                    repeat
                        if isword(itemread() ->> item)
                        and (fast_lmember(item,typed_args) ->> l) then
                            hd(tl(l)) || type_flags -> type_flags
                        else
                            mishap(item,1,'EXPECTING TYPED ARG NAME')
                        endif;
                        quitif(pop11_need_nextitem([, >]) == ">")
                    endrepeat
                endif
            endwhile;

            old_autoload -> pop_autoload;
            unless spec then DEFAULT_TYPESPEC -> spec endunless;
            count+1 -> count;
            {% name, spec, if updating then type_flags, optval, false endif %};

            if (pop11_need_nextitem([, )]) ->> item) == "," then
                pop11_try_nextitem(")") -> item
            endif
        enduntil

    %] -> value_list;
    returnif(count == 0);

    define lconstant push_resname(name);
        lvars name;
        if islist(name) then
            ;;; expression -- compile it
            procedure;
                dlocal proglist_state = proglist_new_state(/*name*/);
                pop11_comp_expr_to("%") ->
            endprocedure(name)
        else
            ;;; string
            sysPUSHQ(name)
        endif
    enddefine;

    if updating then

        /* setting values */

        for v in rev(value_list) do sysPOP(sysNEW_LVAR() ->> v(VAR)) endfor;
        sysPUSH(w);
        if checking then sysCALL("XptCheckWidget") endif;

        if len_count then
            sysCALL("stacklength"), sysPOP(sysNEW_LVAR() ->> len_count)
        endif;

        for v in value_list do
            explode(v) -> (name, spec, type_flags, optval, var);
            lvars label = false;
            if optval then
                ;;; first test for optional value
                sysPUSH(var);
                if optval /== "false" then
                    ;;; optval is value of identifier
                    if isword(optval) then sysPUSH(optval) else sysPUSHQ(optval) endif;
                    sysCALL("/==")
                endif;
                sysIFNOT(sysNEW_LABEL() ->> label)  ;;; skip rest if optval
            endif;

            push_resname(name);     ;;; resource name
            sysPUSH(var);           ;;; value given

            if type_flags /== 0 then
                sysPUSHQ(type_flags);
                sysCALL("XptValTestTypedArg");
                ;;; skip rest if returns true (XtVaTypedArg)
                sysIFSO(label or (sysNEW_LABEL() ->> label))
            endif;

            ;;; run updaters of conversion procedures backwards
            field_spec_info(spec) -> (,);   ;;; check valid
            spec -> org_spec;
            while isclosure(spec) do
                if frozval(2,spec) then
                    sysUCALLQ(pdpart(spec));    ;;; conv procedure
                    frozval(1,spec) -> spec     ;;; strip it off
                else
                    ;;; access procedure -- invalid, since we only allow
                    ;;; direct values
                    mishap(spec, 1, 'INVALID TYPESPEC FOR XptVal')
                endif
            endwhile;
            if isword(spec) and isendstring('float', spec) then
                ;;; real -> decimal
                sysPUSHQ(1.0s0);
                sysCALL("number_coerce")
            elseif checking and spec == org_spec then
                ;;; else with no conversions, just check field value
                sysPUSH(var);
                sysPUSHQ(#_< EXPTRINITSTR(:dfloat) >_#);
                sysUFIELD(false, spec, false, 1);
            endif;

            if label then sysLABEL(label) endif;
        endfor;

        if len_count then
            sysCALL(sysCALL("stacklength"), sysPUSH(len_count), "fi_-")
        else
            sysPUSHQ(count*2)
        endif;
        sysCALL("fast_XtVaSetValues");
        false -> pop_expr_update;

    else

        /* getting values */
        dlvars struct;

        define lconstant comp_field_offsets();
            lvars n;
            lconstant ptr_val = nonwriteable consref("int");
            for n from 2 to count do    ;;; offset of first is always 0
                ;;; offset of field
                sysPUSHQ(null_external_ptr);
                sysFIELD(n, struct, false, 16:301);     ;;; addr mode
                sysFIELD(false, ptr_val, false, 1)
            endfor
        enddefine;

        if checking then sysCALL("XptCheckWidget") endif;

        [%  POINTER; for v in value_list do v(SPEC) endfor %] -> struct;
        lvars (, size) = field_spec_info(struct);
        if (size // SIZEOFTYPE(:byte,:1) -> size) /== 0 then
            size+1 -> size
        endif;

        push_resname(hd(value_list)(NAME));     ;;; name1
        unless count = 1 and size <= SIZEOFTYPE(:exval) then
            ;;; args to sys_XptGetValues are
            ;;;   w, name1, offs2, name2, ... offsN, nameN, (size<<10)||N
            lvars offsets = {% sysEXEC_COMPILE(comp_field_offsets, false) %};
            1 -> n;
            for v in tl(value_list) do
                sysPUSHQ(subscrv(n,offsets));
                push_resname(v(NAME));
                n+1 -> n
            endfor;
            sysPUSHQ((size << 10) || count);
        ;;; else for one (<=) exval value, args are just w, name1
        endunless;

        sysCALL("sys_XptGetValues");
        sysPOP(sysNEW_LVAR() ->> var);

        for n to count do
            sysPUSH(var);
            sysFIELD(n, struct, false, mode);
        endfor;

        sysPUSH(var);
        sysCALL("sys_grbg_fixed")

    endif
enddefine;

sysprotect("XptVal");

endsection;     /* $-typespec_utils */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 31 1995
        Changed SIZEOFTYPE to :exval
--- John Gibson, Apr 10 1993
        Added checking of typespec in update case
 */
