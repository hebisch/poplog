/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/exacc.p
 > Purpose:         Syntax for external structure access/function calls
 > Author:          John Gibson, Jul  2 1990 (see revisions)
 > Documentation:   REF *DEFSTRUCT, REF *EXTERNAL, REF *VMCODE
 > Related Files:   LIB *P_TYPESPEC
 */
compile_mode:pop11 +strict;

uses typespec_utils;


section $-typespec_utils => exacc;

sysunprotect("exacc");

define exacc_result_spec(spec) -> (spec, fldmode);
    lvars spec, fldmode, p;
    if isclosure(spec) and not(frozval(2,spec))
    and pdpart(pdpart(spec) ->> p) == identfn
    and frozval(1,p) == ":" then
        ;;; implicit type access
        frozval(2,p) -> spec
    endif;
    if ispair(spec) then
        compound_fldmode(spec) -> fldmode
    elseif isvector(spec) then
        consref(true) -> fldmode
    else
        false ->> (spec, fldmode)
    endif
enddefine;

define lconstant do_exacc(inner) -> result_spec;
    lvars   exptr = false, count, item, spec, fldmode, subexpr = false, id,
            field_name, n, m, result_spec, fm, checking, mode, addr_mode,
            inner;
    dlocal  pop_new_lvar_list, pop_autoload = false;

    exacc_attributes() -> (checking, mode);
    mode &&/=_0 2:10e8 -> addr_mode;
    if inner then mode || 2:01e8 -> mode endif; ;;; nc if passing on result

    if pop11_try_nextitem("(") then
        if pop11_try_nextitem("exacc") then
            do_exacc(true) -> spec;
            pop11_need_nextitem(")") -> ;
            exacc_result_spec(spec) -> (spec, fldmode);
            true -> subexpr;
            pop_expr_inst(pop_expr_item)
        else
            "(" :: proglist -> proglist
        endif
    endif;

    unless subexpr then
        ;;; try for typespec cast before
        read_typespec(false, true) -> (spec, fldmode, );

        ;;; read identifier or expression
        true -> pop_autoload;
        if pop11_try_nextitem("(") then
            ;;; expression
            pop11_comp_expr_to(")") ->
        else
            ;;; should work with section paths but currently doesn't
            itemread() -> exptr;
            unless isword(exptr)
            and not((sys_current_ident(exptr) ->> id) and identprops(id) /== 0)
            then
                mishap(exptr, 1, 'INVALID EXTERNAL POINTER IDENTIFIER')
            endunless;
            unless spec then
                ;;; get type from identifier name
                if sys_current_ident(type_idname(exptr)) ->> id then
                    fast_destpair(fast_idval(id)) -> (fldmode, spec)
                endif
            endunless
        endif
    endunless;

    ;;; check we now have a spec
    unless spec then
        mishap(0, 'exacc: CANNOT DERIVE TYPESPEC FOR IDENTIFIER/EXPRESSION')
    endunless;

    deref_struct1(spec, fldmode) -> (spec, fldmode);

    true -> pop_autoload;
    if islist(spec) then
        ;;; structure field
        if subexpr then false -> checking endif;
        pop11_need_nextitem(".") -> ;
        false -> pop_autoload, itemread() -> field_name, true -> pop_autoload;
        1 -> n;
        repeat
            if fldmode == [] then
                mishap(field_name, 1, 'exacc: INVALID STRUCTURE FIELD NAME')
            endif;
            if isref(dest(fldmode) -> fldmode ->> fm ->> item) then
                fast_cont(item) -> item
            endif;
            quitif(item == field_name);
            n+1 -> n
        endrepeat;
        fm -> fldmode;
        1 -> m;
        fast_for item in spec do
            nextif(item == POINTER or item == "|");
            quitif(m == n);
            m+1 -> m
        endfor;
        item

    elseif ispair(spec) then
        ;;; array element
        pop11_need_nextitem("[") -> ;
        unless isinteger(itemread() ->> n) and pop11_try_nextitem("]") then
            n :: proglist -> proglist;
            unless exptr then sysPOP(sysNEW_LVAR() ->> exptr) endunless;
            pop11_comp_expr_to("]") -> ;
            false -> n
        endunless;
        front(spec)

    elseif isvector(spec) then
        ;;; function call
        if ispair(subscrv(1,spec) ->> m) then front(m) -> m endif;  ;;; nargs
        ;;; don't need a check unless it's variadic
        if subexpr and m then false -> checking endif;
        pop11_need_nextitem("(") -> ;
        unless exptr then sysPOP(sysNEW_LVAR() ->> exptr) endunless;
        pop11_comp_expr_seq_to(")") -> ;
        subscrv(2,spec)

    else
        ;;; single type access
        if subexpr then false -> checking endif;
        spec

    endif -> result_spec;

    if exptr then sysPUSH(exptr) endif;
    n -> pop_expr_item;
    sysFIELD(% spec, checking, mode %) -> pop_expr_inst;
    if isref(fldmode) or addr_mode then false -> updater(pop_expr_inst) endif;

    if addr_mode then
        [% POINTER, result_spec %] -> result_spec;
        [% false, if isref(fldmode) then consref() endif %]
                                        -> compound_fldmode(result_spec)
    endif;
enddefine;

define global syntax exacc;
    do_exacc(false) ->
enddefine;

sysprotect("exacc");

endsection;     /* $-typespec_utils */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 26 1992
        Changed function call case to allow for spec(1) being a pair
--- John Gibson, Nov 27 1990
        Copes properly with structure with overlaid fields, plus other
        improvements.
--- John Gibson, Nov 16 1990
        Made use of -checking- value in -sysFIELD- (i.e. so "fast" works!)
--- John Gibson, Oct 19 1990
        Added attribute list following -exacc-.
 */
