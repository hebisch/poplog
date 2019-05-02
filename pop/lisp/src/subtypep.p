/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/subtypep.p
 > Purpose:         SUBTYPEP
 > Author:          John Williams, Apr  5 1990 (see revisions)
 > Documentation:   CLtL, p72-3
 > Related Files:   C.all/lisp/src/types.p, C.all/lisp/src/specs.p
 */

lisp_compile_mode;

section $-lisp;

constant 6 spec_<= procedure (Subtypep typep);

vars Subtypep_flag = true;


define lconstant Array_subtypep(x, y);
    lvars s1, r1, d1, s2, r2, d2;
    dest_array_type(x) -> (s1, r1, d1);
    dest_array_type(y) -> (s2, r2, d2);
    (not(s2) or (s1 and s1 spec_<= s2))
        and
    (not(r2) or (r1 and r1 == r2))
        and
    (not(d2) or (d1 and compare_dims(d1, d2, nonop <=)))
enddefine;


define lconstant Cons_subtypep(x, y);
    lvars xcartype, xcdrtype, ycartype, ycdrtype;
    dest_cons_type(x) -> (xcartype, xcdrtype);
    dest_cons_type(y) -> (ycartype, ycdrtype);
    Subtypep(xcartype, ycartype, 2:11)
        and
    Subtypep(ycartype, ycartype, 2:11)
enddefine;


define lconstant Real_subtypep(x, y);
    lvars lo1, hi1, lo2, hi2;
    dest_num_type(x, false) -> (lo1, hi1);
    dest_num_type(y, false) -> (lo2, hi2);
    (not(lo2) or (lo1 and lo1 >= lo2))
        and
    (not(hi2) or (hi1 and hi1 <= hi2))
enddefine;


define lconstant Type_=(x, y);
    if ispair(x) and ispair(y) then
        Type_=(fast_front(x), fast_front(y))
            and
        Type_=(fast_back(x), fast_back(y))
    else
        x ==# y
    endif
enddefine;


define Subtypep(x, y, ex);
    lvars hx, tx, hy, ty, cx, cy;

    /* First try the class hierarchy, expanding types x and y each
        time round the loop.
    */

    repeat
        if Type_=(x, y) then
            return(true)
        endif;

        if isinstance(x)
        and is_instance_of(x, get_class_by_name(@CLASS)) then
            Accept_ioc(x, false) -> cx;
            (class_name(cx), []) -> (hx, tx)
        else
            if ispair(x) then
                fast_destpair(x)
            else
                x, []
            endif -> (hx, tx);
            get_class_by_name(hx) -> cx
        endif;

        if isinstance(y)
        and is_instance_of(y, get_class_by_name(@CLASS)) then
            Accept_ioc(y, false) -> cy;
            (class_name(cy), []) -> (hy, ty)
        else
            if ispair(y) then
                fast_destpair(y)
            else
                y, []
            endif -> (hy, ty);
            get_class_by_name(hy) -> cy
        endif;

        if cx and cy
        and tx == [] and ty == [] then
            return(is_subclass_of(cx, cy))
        else
            unless (ex _bitst 2:10) and (type_expand1(x) -> x)
            or (ex _bitst 2:01) and (type_expand1(y) -> y) then
                quitloop
            endunless
        endif
    endrepeat;

    /* Now try logical type specifiers */

    if hx == @AND then
        until endp(tx) do
            if Subtypep(fast_destpair(tx) -> tx, y, 2:10) then
                return(true)
            endif
        enduntil;
        false
    elseif hy == @AND then
        until endp(ty) do
            if Subtypep(x, fast_destpair(ty) -> ty, 2:01) then
                return(false)
            endif
        enduntil;
        true
    elseif hx == @OR then
        until endp(tx) do
            unless Subtypep(fast_destpair(tx) -> tx, y, 2:10) then
                return(false)
            endunless
        enduntil;
        true
    elseif hy == @OR then
        until endp(ty) do
            if Subtypep(x, fast_destpair(ty) -> ty, 2:01) then
                return(true)
            endif
        enduntil;
        false
    elseif hx == @NOT and hy == @NOT then
        Subtypep(checkr_type_arg(y), checkr_type_arg(x), 2:11)
    elseif hy == @NOT then
        not(Subtypep(x, checkr_type_arg(y), 2:01))
            and
        not(Subtypep(checkr_type_arg(y), x, 2:10))
    elseif hx == @NOT then
        hy == @T and ty == []
    elseif hx == @MEMBER and hy == @MEMBER then
        pop_true(@SUBSETP(tx, ty, 2))
    elseif hx == @MEMBER then
        until endp(tx) do
            unless typep(fast_destpair(tx) -> tx, y) do
                return(false)
            endunless;
        enduntil;
        true
    elseif hy == @MEMBER then
        false
    elseif hx == @SATISFIES or hy == @SATISFIES then
        false ->> Subtypep_flag
    else
        /* Finally try specialised array, cons, or numeric type specifiers */
        unless cx do
            lisp_error('Unrecognised type specifier', [^x])
        endunless;
        unless cy do
            lisp_error('Unrecognised type specifier', [^y])
        endunless;
        lvars ARRAY = get_class_by_name(@ARRAY);
        lvars REAL = get_class_by_name(@REAL);
        if is_subclass_of(cx, ARRAY) or is_subclass_of(cy, ARRAY) then
            is_subclass_of(cx, cy) and Array_subtypep(x, y)
        elseif is_subclass_of(cx, REAL) or is_subclass_of(cy, REAL) then
            is_subclass_of(cx, cy) and Real_subtypep(x, y)
        elseif hx == @COMPLEX or hy == @COMPLEX then
            hx == hy
        elseif hx == @CONS or hy == @CONS then
            hx == hy and Cons_subtypep(x, y)
        elseif hx == @FUNCTION then
            lisp_error('Illegal use of FUNCTION type specifier', [^x])
        elseif hy == @FUNCTION then
            lisp_error('Illegal use of FUNCTION type specifier', [^y])
        elseif tx /== [] then
            lisp_error('Unknown compound type specifier', [^x])
        elseif ty /== [] then
            lisp_error('Unknown compound type specifier', [^y])
        else
            false
        endif
    endif
enddefine;


define subtypep(x, y);
    dlocal Subtypep_flag = true;
    if Subtypep(x, y, 2:11) then
        true, true
    else
        nil, lisp_true(Subtypep_flag)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Apr 26 1994
        Subtypep rewritten for CLOS, also fixed handling of logical types
        (which fixes BR isl-fr.4427), and upgraded handling of compound array
        and numeric types as per Steele 1990 chapter 6. Also handles compound
        form of CONS type specifier (in anticipation of ANSI Common Lisp).
--- John Williams, Dec 16 1993
        Now uses Type_= instead of sys_= to compare type specifiers.
--- John Williams, Jul 13 1992
        Fixed first part of BR isl-fr.4427
 */
