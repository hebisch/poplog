/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/equals.p
 > Purpose:         Lisp predicates EQUAL and EQUALP
 > Author:          John Williams, March 8 1987 (see revisions)
 > Documentation:   CLtL, p77-82
 > Related Files:   C.all/lisp/src/types.p, C.all/lisp/src/defstruct.p
 */

lisp_compile_mode;

section $-lisp;

lconstant procedure (Try_array_equal, Try_array_equalp);

vars procedure struct_field_equal = nonop =;


/* EQUAL */

define equal(x, y);
    lvars k1, k2;
    dlocal struct_field_equal = equal;

    if x == y then
        return(true)
    elseif issimple(x) or issimple(y) then
        return(false)
    endif;

    fast_datakey(x) -> k1;
    fast_datakey(y) -> k2;

    if k1 == symbol_key or k2 == symbol_key then
        false
    elseif is_number_key(k1) or is_number_key(k2) then
        x ==# y
    elseif k1 == character_key or k2 == character_key then
        false
    elseif k1 == pair_key or k2 == pair_key then
        k1 == k2
            and equal(fast_front(x), fast_front(y))
            and equal(fast_back(x), fast_back(y))
    elseif k1 == pathname_key or k2 == pathname_key then
        k1 == k2 and x = y
    else
        Try_array_equal(x, y, k1, k2)
    endif
enddefine;


define lconstant Try_array_equal(x, y, k1, k2);
    lvars a, lo1, lo2, hi1, hi2, subscr;

    if (isarray(x) ->> a)
    and arrayrank(a) == 1 then
        fast_1d_array_info(a, x) -> (x, k1, lo1, hi1, );
    elseif is_vector_key(k1) then
        1, fast_vector_length(x) -> (lo1, hi1)
    else
        return(false)
    endif;

    if (isarray(y) ->> a)
    and arrayrank(a) == 1 then
        fast_1d_array_info(a, y) -> (y, k2, lo2, hi2, );
    elseif is_vector_key(k2) then
        1, fast_vector_length(y) -> (lo2, hi2)
    else
        return(false)
    endif;

    unless (k1 == k2)
    and (k1 == bitvector_key or k1 == string_key)
    and ((hi1 fi_- lo1) == (hi2 fi_- lo2))
    do
        return(false)
    endunless;

    if k1 == bitvector_key then
        /* Can use fast_subscrs for comparing bit-vectors
            if lo1 and lo2 are byte-aligned.
        */
        lblock;
            lvars lq1, lq2, hq1, hr1;
            if  (lo1 fi_// 8 -> lq1) == 1
            and (lo2 fi_// 8 -> lq2) == 1
            then
                hi1 fi_// 8 -> hq1 -> hr1;
                while lq1 fi_< hq1 do
                    lq1 fi_+ 1 -> lq1;
                    lq2 fi_+ 1 -> lq2;
                    returnunless
                        (fast_subscrs(lq1, x) == fast_subscrs(lq2, y))
                        (false)
                endwhile;
                returnif (hr1 == 0) (true);     /* No bits left over */
                lq1 fi_<< 3 fi_+ 1 -> lo1;
                lq2 fi_<< 3 fi_+ 1 -> lo2
            endif
        endlblock;
        fast_subscrbitvector
    else
        fast_subscrs
    endif -> subscr;

    until lo1 fi_> hi1 do
        returnunless
            (fast_apply(lo1, x, subscr) == fast_apply(lo2, y, subscr))
            (false);
        lo1 fi_+ 1 -> lo1;
        lo2 fi_+ 1 -> lo2
    enduntil;
    true
enddefine;


/* EQUALP */

define equalp(x, y);
    lvars k1, k2;
    dlocal struct_field_equal = equalp;

    if x == y then
        return(true)
    elseif issimple(x) or issimple(y) then
        return(false)
    endif;

    fast_datakey(x) -> k1;
    fast_datakey(y) -> k2;

    if k1 == symbol_key or k2 == symbol_key then
        false
    elseif is_number_key(k1) or is_number_key(k2) then
        sys_=(x, y)
    elseif k1 == character_key or k2 == character_key then
        k1 == k2
            and caseless_=(fast_char_code(x), fast_char_code(y))
    elseif k1 == pair_key or k2 == pair_key then
        k1 == k2
            and equalp(fast_front(x), fast_front(y))
            and equalp(fast_back(x), fast_back(y))
    elseif is_record_key(k1) or is_record_key(k2) then
        k1 == k2 and x = y
    elseif isproperty(x) then
        isproperty(y) and property_equal(x, y, equalp)
    elseif isproperty(y) then
        isproperty(x) and property_equal(x, y, equalp)
    else
        Try_array_equalp(x, y, k1, k2)
    endif
enddefine;


lconstant
    Temp_b1 = writeable [1 0],
    Temp_b2 = writeable [1 0],
    Temp_b1_back = back(Temp_b1),
    Temp_b2_back = back(Temp_b2),
    ;


define lconstant Try_array_equalp(x, y, k1, k2);
    lvars a, fp, r1, r2, b1, b2, lo1, lo2, hi1, hi2,
            procedure (subscr1, subscr2);

    false -> b1;
    if (isarray(x) ->> a) then
        fast_array_info(a, x) -> (r1, b1, x, lo1, hi1, fp, subscr1);
        if fp then
            false -> b1
        endif
    elseif is_vector_key(k1) then
        1, 1, fast_vector_length(x), lisp_fast_subscr_p(x)
            -> (r1, lo1, hi1, subscr1)
    else
        return(false)
    endif;

    false -> b2;
    if (isarray(y) ->> a) then
        fast_array_info(a, y) -> (r2, b2, y, lo2, hi2, fp, subscr2);
        if fp then
            false -> b2
        endif
    elseif is_vector_key(k2) then
        1, 1, fast_vector_length(y), lisp_fast_subscr_p(y)
            -> (r2, lo2, hi2, subscr2)
    else
        return(false)
    endif;

    returnunless (r1 == r2) (false);
    returnunless ((hi1 fi_- lo1) == (hi2 fi_- lo2)) (false);

    unless b1 do
        0 -> fast_front(Temp_b1);
        hi1 fi_- lo1 -> fast_front(Temp_b1_back);
        Temp_b1 -> b1
    endunless;
    unless b2 do
        0 -> fast_front(Temp_b2);
        hi2 fi_- lo2 -> fast_front(Temp_b2_back);
        Temp_b2 -> b2
    endunless;
    returnunless (sys_=(b1, b2)) (false);

    until lo1 fi_> hi1 do
        returnunless
            (equalp(fast_apply(lo1, x, subscr1),
                    fast_apply(lo2, y, subscr2)))
            (false);
        lo1 fi_+ 1 -> lo1;
        lo2 fi_+ 1 -> lo2
    enduntil;
    true
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 23 1994
        Try_array_equal no longer assumes last byte of a vector padded
        with 0's (for safety).
--- John Williams, Dec 23 1993
        Upgraded to Steele 1990 p103-9. equalp now works on hash tables.
        equal no longer recursively descends record class structures.
        Also, equal now compares bit vectors byte by byte if possible.
--- John Williams, Aug 31 1993
        Temp_b1 and Temp_b2 now writeable.
 */
