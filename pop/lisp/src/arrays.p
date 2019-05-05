/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/arrays.p
 > Purpose:         Common Lisp Arrays
 > Author:          John Williams, April 27 1987 (see revisions)
 > Documentation:   CLtL, p286-298
 > Related Files:   C.all/lisp/src/seq.p
 */

lisp_compile_mode;

section $-lisp;

/*
Types of array:
    simple-base-string      =   pop string
    simple-vector           =   pop vector
    simple-bit-vector       =   see LIB * BITVECTORS
    ordinary array          =   pop array
    adjustable array        =   pop array partially applied to []
    displaced array         =   pop array that uses another array's vector
    fill-pointer vector     =   array whose PDPROPS is fill-pointer
*/


/* Utilities */

syssynonym("arrayrank",         "pdnargs");
syssynonym("fast_fill_pointer", "pdprops");


define isadjustable(a);
    isclosure(a) and datalength(a) == 0
enddefine;


defprop is_displaced_array;

define arrays_displaced_to =
    newanyproperty([], 32, 1, 48, false, false, "tmparg", [], false)
enddefine;


define array_used_size() with_nargs 1;
    nonop fi_-(arrayvector_bounds()) fi_+ 1
enddefine;


constant procedure (checkr_fp, elt, listp, nth);


/* Array constants */

constant
    array_rank_limit        =  256,
    array_dimension_limit   =  (2 ** 24),           /* Taken from ACL */
    array_total_size_limit  =  pop_max_int,
    ;


/* Array recognisers */

define arrayp(x);
    if isarray(x) or isvectorclass(x) then true else false endif
enddefine;


define simple_array_p(x);
    if (isarray(x) == x) or isvectorclass(x) then true else false endif
enddefine;


define adjustable_array_p(a);
    isarray(a) and isadjustable(a)
enddefine;


define array_key(a);
    isvectorclass(a) or fast_datakey(arrayvector(a))
enddefine;


/* Constructing arrays */

define lconstant Checkr_bounds(dims) -> (bounds, size, rank);
    lvars dim;
    unless listp(dims) do
        conspair(dims, []) -> dims
    endunless;
    1 -> size;
    0 -> rank;
    [% for dim in_cl_list dims do
        unless isinteger(dim)
        and dim fi_>= 0
        and dim fi_< array_dimension_limit do
            lisp_error('Invalid array dimension', [^dim])
        endunless;
        dim * size -> size;         /* size might be a biginteger */
        rank fi_+ 1 -> rank;
        0, dim fi_- 1
    endfor %] -> bounds;
    if rank fi_>= array_rank_limit then
        lisp_error('Array rank (~D) >= ARRAY-RANK-LIMIT (~D)',
                    [^rank ^array_rank_limit])
    endif;
    if size >= array_total_size_limit then
        lisp_error('Array total size (~D) >= ARRAY-TOTAL-SIZE-LIMIT (~D)',
                    [^size ^array_total_size_limit])
    endif
enddefine;


define lconstant Flatseq(seq, bounds, for_string);
    lvars lo, hi, i;
    if bounds == nil then
        seq
    else
        fast_destpair(fast_destpair(bounds)) -> (lo, hi, bounds);
        fast_for i from lo to hi do
            Flatseq(elt(seq, i), bounds, for_string);
            if for_string then char_code() endif
        endfast_for
    endif
enddefine;


define make_array(dims, type, init, contents, adjust, fp, disp, offset) -> a;
    lvars bounds, len, rank, key;
    dlocal poparray_by_row = false;

    Checkr_bounds(dims) -> (bounds, len, rank);

    if type == nil then
        if disp /== nil then
            array_key(disp)
        else
            vector_key
        endif -> key
    elseif (etype_->_key(type) ->> key) then
        unless disp == nil
        or array_key(disp) == key then
            lisp_error('Inconsistent :ELEMENT-TYPE and :DISPLACED-TO options',
                       [^type ^disp])
        endunless
    else
        lisp_error('Invalid array :ELEMENT-TYPE specifier', [^type])
    endif;

    /* Build arrayvector for the new array */

    if disp /== nil then
        if adjustable_array_p(disp) then
            @:DISPLACED -> adjust
        endif;
        disp
    else
        0 -> offset;
        if contents /== nil then
            Flatseq(contents, bounds, key == string_key);
            class_cons(key)(len)
        else
            if init == nil then
                key_->_init(key) -> init
            endif;
            if key == string_key then
                char_code(init) -> init
            endif;
            initvectorclass(len, init, key)
        endif
    endif -> a;

    returnif(rank == 1 and adjust == nil and disp == nil and fp == nil);

    newanyarray(bounds,
                a,
                if key == string_key then lisp_subscrs endif,
                offset)
        -> a;

    if adjust /== nil then
        consclosure(a, 0) -> a;
        true -> sys_writeable_prop(a);
        if disp /== nil then
            disp -> is_displaced_array(a);
            if adjust == @:DISPLACED then   /* i.e. disp is adjustable */
                conspair(a, arrays_displaced_to(disp))
                    -> arrays_displaced_to(disp)
            endif
        endif
    endif;

    if fp /== nil then
        checkr_fp(fp, a) -> fast_fill_pointer(a);
        true -> sys_writeable_prop(a)
    endif
enddefine;


/* General array access */

define lconstant Array_needed =
    type_error(% @ARRAY %)
enddefine;


define lconstant Array_sub_err(a, subs);
    lisp_error('Wrong number of array subcripts', [^a ^subs])
enddefine;


define aref(a, subs);
    lvars n;
    destlist(subs) -> n;
    if isarray(a) then
        if n == arrayrank(a) then
            fast_apply(a)
        else
            Array_sub_err(a, subs)
        endif
    elseif isvectorclass(a) then
        if n == 1 then
            fast_vector_access(a)       /* index on stack */
        else
            Array_sub_err(a, subs)
        endif
    else
        Array_needed(a)
    endif
enddefine;


define updaterof aref(a, subs) with_nargs 3;
    lvars n;
    destlist(subs) -> n;
    if isarray(a) then
        if n == arrayrank(a) then
            -> fast_apply(a)
        else
            Array_sub_err(a, subs)
        endif
    elseif isvectorclass(a) then
        if n == 1 then
            -> n;
            -> fast_vector_access(n, a)
        else
            Array_sub_err(a, subs)
        endif
    else
        Array_needed(a)
    endif
enddefine;


define aref1(a, i);
    if isvectorclass(a) then
        fast_vector_access(i, a)
    else
        fast_apply(i, a)
    endif
enddefine;


define updaterof aref1(a, i) with_nargs 3;
    if isvectorclass(a) then
        -> fast_vector_access(i, a)
    else
        -> fast_apply(i, a)
    endif
enddefine;


define row_major_aref(a, i);
    lvars lo;
    if isvectorclass(a) then
        fast_vector_access(i, a)
    else
        arrayvector_bounds(a) -> lo ->;
        arrayvector(a) -> a;
        fast_vector_access(check_positive(i) fi_+ lo fi_- 1, a)
    endif
enddefine;


define updaterof row_major_aref(a, i) with_nargs 3;
    lvars lo;
    if isvectorclass(a) then
        -> fast_vector_access(i, a)
    else
        arrayvector_bounds(a) -> lo ->;
        arrayvector(a) -> a;
        -> fast_vector_access(check_positive(i) fi_+ lo fi_- 1, a)
    endif
enddefine;


/* Simple vector access */

define lconstant Check_simple_vector(item);
    unless isvector(item) do
        type_error(item, @SIMPLE-VECTOR)
    endunless
enddefine;


define svref(vec, i);
    Check_simple_vector(vec);
    fast_subscrv(checkr_vector_index(i, vec), vec)
enddefine;


define updaterof svref(vec, i) with_nargs 3;
    Check_simple_vector(vec);
    -> fast_subscrv(checkr_vector_index(i, vec), vec)
enddefine;


/* Array information */

define array_element_type() with_nargs 1;
    key_->_etype(array_key())
enddefine;


define array_rank(a);
    if isarray(a) then
        arrayrank(a)
    elseif isvectorclass(a) then
        1
    else
        Array_needed(a)
    endif
enddefine;


define array_dimensions(a);
    if isvectorclass(a) then
        [^(fast_vector_length(a))]
    else
        lblock
            lvars lo, hi;
            boundslist(a) -> a;
            [% until a == [] do
                fast_destpair(fast_destpair(a)) -> (lo, hi, a);
                hi fi_- lo fi_+ 1
            enduntil %]
        endlblock
    endif
enddefine;


define array_dimension(a, dim);
    nth(dim, array_dimensions(a))
enddefine;


define array_size(a);
    if isvectorclass(a) then
        fast_vector_length(a)
    else
        array_used_size(a)
    endif
enddefine;


define array_bounds(a);
    if isvectorclass(a) then
        conspair(1, conspair(fast_vector_length(a), []))
    else
        boundslist(a)
    endif
enddefine;


define array_in_bounds_p(a, subs);
    lvars bounds, save, s, lo, hi;
    array_bounds(a) -> bounds;
    subs -> save;
    until bounds == [] do
        if subs == [] then
            Array_sub_err(a, save)
        endif;
        fast_destpair(subs) -> (s, subs);
        unless isinteger(s) do
            lisp_error('Invalid array subscript', [^s])
        endunless;
        fast_destpair(fast_destpair(bounds)) -> (lo, hi, bounds);
        returnif (s fi_< lo or s fi_> hi) (nil)
    enduntil;
    unless subs == [] do
        Array_sub_err(a, save)
    endunless;
    true
enddefine;


define array_row_major_index(a, subs) -> i;
    /* Won't work on Pop-11 non-zero origin arrays */
    lvars dims, d, s;
    0 -> i;
    array_dimensions(a) -> dims;
    until dims == [] or subs == [] do
        fast_destpair(dims) -> (d, dims);
        fast_destpair(subs) -> (s, subs);
        unless isinteger(s) and s fi_>= 0 and s fi_< d do
            lisp_error('Array subscript ~D out of range', [^s ^a])
        endunless;
        fast_for d in dims do
            s fi_* d -> s
        endfast_for;
        s fi_+ i -> i
    enduntil;
    unless subs == dims do
        if subs == [] then
            lisp_error('Too few subscripts for array', [^a])
        else
            lisp_error('Excess array subscript(s)', [^subs ^a])
        endif
    endunless
enddefine;


/* Fill-pointers and adjustable arrays */

define lconstant Fp_array_needed(a);
    lisp_error('Array does not have fill-pointer', [^a])
enddefine;


define is_fp_array(a) -> fp;
    unless isarray(a)
    and arrayrank(a) == 1
    and isinteger(fast_fill_pointer(a) ->> fp) do
        false -> fp
    endunless
enddefine;


define fill_pointer(a) -> fp;
    unless (is_fp_array(a) ->> fp) do
        Fp_array_needed(a)
    endunless
enddefine;


define checkr_fp(fp, a) -> fp;
    lvars n;
    array_used_size(a) -> n;
    if fp == true then
        n -> fp
    elseunless isinteger(fp)
    and fp fi_>= 0
    and fp fi_<= n do
        lisp_error('Bad value for fill-pointer', [^fp ^a])
    endif
enddefine;


define updaterof fill_pointer(fp, a);
    unless is_fp_array(a) do
        Fp_array_needed(a)
    endunless;
    checkr_fp(fp, a) -> fast_fill_pointer(a)
enddefine;


define vector_push(item, a) -> fp;
    fill_pointer(a) -> fp;
    if fp fi_< array_used_size(a) then
        item -> fast_apply(fp, a);
        fp fi_+ 1 -> fast_fill_pointer(a)
    else
        nil -> fp
    endif
enddefine;


define vector_pop(a);
    lvars fp;
    fill_pointer(a) fi_- 1 -> fp;
    if fp fi_< 0 then
        lisp_error('Array fill-pointer is zero', [^a])
    else
        fast_apply(fp, a);
        fp -> fast_fill_pointer(a)
    endif
enddefine;


define lconstant Rebuild_arrays_displaced_to(a);
    lvars d, lo;
    for d in arrays_displaced_to(a) do
        ;;; [Rebuilding ^d to be displaced to ^a] =>
        unless isadjustable(d) do
            lisp_error('System error in Rebuild_arrays_displaced_to', [^a ^d])
        endunless;
        arrayvector_bounds(d) -> lo ->;
        newanyarray(
            boundslist(d),
            a,
            lo fi_- 1) -> pdpart(d);
        updater(pdpart(d)) -> updater(d);
        Rebuild_arrays_displaced_to(d)
    endfor
enddefine;


define lconstant Update_array(new, a, disp);
    lvars d;
    new -> pdpart(a);
    updater(new) -> updater(a);
    if (is_displaced_array(a) ->> d) then
        ;;; [Removing ^a from the displaced_to list of ^d] =>
        fast_ncdelete(a, arrays_displaced_to(d))
            -> arrays_displaced_to(d)
    endif;
    disp -> is_displaced_array(a);
    if disp then
        conspair(a, arrays_displaced_to(disp))
            -> arrays_displaced_to(disp)
    endif;
    Rebuild_arrays_displaced_to(a)
enddefine;


define vector_push_extend(item, a, more) -> fp;
    lvars lo, hi, len;
    dlocal poparray_by_row = false;

    fill_pointer(a) -> fp;
    arrayvector_bounds(a) -> lo -> hi;
    hi fi_- lo fi_+ 1 -> len;

    if fp fi_>= len then
        lblock;
            lvars v, key, newlen, newv, init;

            unless isadjustable(a) do
                lisp_error('Cannot extend non-adjustable array', [^a])
            endunless;
            arrayvector(a) -> v;
            fast_datakey(v) -> key;

            if pop_true(more) then
                check_positive(more)
            else
                ;;; defaults
                if len fi_< 16 then
                    16
                else
                    len
                endif
            endif -> more;

            len fi_+ more -> newlen;
            fast_apply(newlen, class_init(key)) -> newv;
            move_subvector(lo, v, 1, newv, len);
            key_->_init(key) -> init;
            if key == string_key then
                fast_char_code(init) -> init
            endif;
            set_subvector(init, len fi_+ 1, newv, more);
            newanyarray([0 ^(newlen fi_- 1)],
                        newv,
                        if key == string_key then lisp_subscrs endif)
                -> newv;
            Update_array(newv, a, false)
        endlblock
    endif;

    item -> fast_apply(fp, a);
    fp fi_+ 1 -> fast_fill_pointer(a)
enddefine;


define adjust_array(a, dims, type, init, contents, fp, disp, offset) -> a;
    lvars key, bounds, rank, new;
    dlvars init;
    dlocal poparray_by_row = false;

    array_key(a) -> key;
    Checkr_bounds(dims) -> (bounds, /* len */, rank);
    unless rank == array_rank(a) do
        lisp_error('Non-matching numbers of dimensions', [^a ^dims])
    endunless;

    if type /== nil then
        unless etype_->_key(type) do
            lisp_error('Invalid array :ELEMENT-TYPE specifier', [^type])
        endunless
    endif;

    unless (pop_true(disp) ->> disp)
    or (pop_true(contents) ->> contents) do
        if init == nil then
            key_->_init(key) -> init
        endif;
        procedure();
            dlvars sl, init_p;
            stacklength() -> sl;
            if isprocedure(a) then
                a
            else
                fast_vector_access(% a %)
            endif -> init_p;

            define dlocal pop_exception_handler(_, _, idstring, _);
                if idstring = 'array-subscr:type-intrange' then
                    setstacklength(sl);
                    exitfrom(init, init_p)
                else
                    false
                endif
            enddefine;

            newanyarray(bounds, init_p, key,
                        if key == string_key then lisp_subscrs endif)
        endprocedure()
    else
        if contents then
            0 -> offset;
            class_cons(key)
                (#| Flatseq(contents, bounds, key == string_key) |#)
                -> contents
        endif;
        newanyarray(bounds,
                    contents or disp,
                    if key == string_key then lisp_subscrs endif,
                    offset)
    endunless -> new;

    if adjustable_array_p(a) then
        Update_array(new, a, disp)
    else
        new -> a
    endif;

    if fp /== nil then
        fp -> fill_pointer(a)
    endif
enddefine;


/* Bit arrays */

define isbitarray(a);
    isarray(a) and isbitvector(arrayvector(a))
enddefine;


define lconstant Bit_array_needed =
    type_error(% [^@ARRAY ^@BIT] %)
enddefine;


define cl_bit(a, subs);
    lvars n;
    destlist(subs) -> n;
    if isbitarray(a) then
        if arrayrank(a) == n then
            fast_apply(a)
        else
            Array_sub_err(a, subs)
        endif
    elseif isbitvector(a) then
        if n == 1 then
            checkr_vector_index(a) -> n;
            fast_subscrbitvector(n, a)
        else
            Array_sub_err(a, subs)
        endif
    else
        Bit_array_needed(a)
    endif
enddefine;


define updaterof cl_bit(a, subs) with_nargs 3;
    lvars n;
    destlist(subs) -> n;
    if isbitarray(a) then
        if arrayrank(a) == n then
            -> fast_apply(a)
        else
            Array_sub_err(a, subs)
        endif
    elseif isbitvector(a) then
        if n == 1 then
            checkr_vector_index(a) -> n;
            -> fast_subscrbitvector(n, a)
        else
            Array_sub_err(a, subs)
        endif
    else
        Bit_array_needed(a)
    endif
enddefine;


define lconstant Destbitarray(a, temp_bounds);
    lvars hi;
    if isbitarray(a) then
        arrayvector(a), boundslist(a), arrayvector_bounds(a)
    elseif isbitvector(a) then
        fast_vector_length(a) -> hi;
        hi fi_- 1 -> fast_front(fast_back(temp_bounds));
        a, temp_bounds, hi, 1
    else
        Bit_array_needed(a)
    endif
enddefine;


lconstant
    Different_bounds = 'Bit arrays have different rank/dimensions',
    sub_s = fast_subscrs,
    sub_b = fast_subscrbitvector;


define lconstant Do_bits(a1, a2, a3, bit_op) -> a3;
    lvars lo1, hi1, b1, v1,
          lo2, hi2, b2, v2,
          lo3, hi3, b3, v3,
          lq1, lq2, lq3, hq1, hr1;

    /* Get base bit-vectors, min/max subscripts, and boundslists */

    Destbitarray(a1, #_< writeable [0 0] >_#) -> lo1 -> hi1 -> b1 -> v1;
    Destbitarray(a2, #_< writeable [0 0] >_#) -> lo2 -> hi2 -> b2 -> v2;

    unless sys_=(b1, b2) do
        lisp_error(Different_bounds, [^a1 ^a2])
    endunless;

    if a3 == true then
        a1 -> a3;
        v1, b1, hi1, lo1
    else
        unless pop_true(a3) do
            copy(a1) -> a3
        endunless;
        Destbitarray(a3, #_< writeable [0 0] >_#)
    endif -> lo3 -> hi3 -> b3 -> v3;

    unless sys_=(b1, b3) do
        lisp_error(Different_bounds, [^a1 ^a3])
    endunless;

    /* Can use fast_subscrs for processing bit-vectors if start indexes
        lo1, lo2, and lo3 are all byte-aligned
    */
    if  (lo1 fi_// 8 -> lq1) == 1
    and (lo2 fi_// 8 -> lq2) == 1
    and (lo3 fi_// 8 -> lq3) == 1
    then
        hi1 fi_// 8 -> hq1 -> hr1;
        while lq1 fi_< hq1 do
            lq1 fi_+ 1 -> lq1;
            lq2 fi_+ 1 -> lq2;
            lq3 fi_+ 1 -> lq3;
            fi_&&(fast_apply(
                sub_s(lq1, v1),
                sub_s(lq2, v2),
                bit_op), 255) -> sub_s(lq3, v3)
        endwhile;
        returnif (hr1 == 0);    /* No bits left over */
        lq1 fi_<< 3 fi_+ 1 -> lo1;
        lq2 fi_<< 3 fi_+ 1 -> lo2;
        lq3 fi_<< 3 fi_+ 1 -> lo3;
    endif;

    /* Apply bit_op to each (remaining) pair of bits */

    for step (lo1 fi_+ 1 -> lo1,
              lo2 fi_+ 1 -> lo2,
              lo3 fi_+ 1 -> lo3)
    till (lo1 fi_> hi1)
    do
        fast_apply(
            sub_b(lo1, v1),
            sub_b(lo2, v2),
            bit_op) fi_&& 1 -> sub_b(lo3, v3)
    endfor
enddefine;


define lconstant Andc1(x, y); y fi_&&~~ x enddefine;

define lconstant Orc1(x, y); (fi_~~ x) fi_|| y enddefine;


constant procedure (
    bit_and     =   Do_bits(% nonop fi_&& %),
    bit_ior     =   Do_bits(% nonop fi_|| %),
    bit_xor     =   Do_bits(% nonop fi_||/& %),
    bit_eqv     =   Do_bits(% nonop fi_||/& <> nonop fi_~~ %),
    bit_nand    =   Do_bits(% nonop fi_&& <> nonop fi_~~ %),
    bit_nor     =   Do_bits(% nonop fi_|| <> nonop fi_~~ %),
    bit_andc1   =   Do_bits(% Andc1 %),
    bit_andc2   =   Do_bits(% nonop fi_&&~~ %),
    bit_orc1    =   Do_bits(% Orc1 %),
    bit_orc2    =   Do_bits(% nonop fi_~~ <> nonop fi_|| %),
    );


define bit_not(a1, a2) -> a2;
    lvars lo1, hi1, b1, v1,
          lo2, hi2, b2, v2,
          lq1, lq2, hq1, hr1;

    Destbitarray(a1, #_< writeable [0 0] >_#) -> lo1 -> hi1 -> b1 -> v1;

    if a2 == true then
        a1 -> a2;
        v1, b1, hi1, lo1
    else
        unless pop_true(a2) do
            copy(a1) -> a2
        endunless;
        Destbitarray(a2, #_< writeable [0 0] >_#)
    endif -> lo2 -> hi2 -> b2 -> v2;

    unless sys_=(b1, b2) do
        lisp_error(Different_bounds, [^a1 ^a2])
    endunless;

    /* Can use fast_subscrs if start indexes lo1 and lo2 are byte-aligned */

    if  (lo1 fi_// 8 -> lq1) == 1
    and (lo2 fi_// 8 -> lq2) == 1
    then
        hi1 fi_// 8 -> hq1 -> hr1;
        while lq1 fi_< hq1 do
            lq1 fi_+ 1 -> lq1;
            lq2 fi_+ 1 -> lq2;
            fi_~~ sub_s(lq1, v1) -> sub_s(lq2, v2)
        endwhile;
        returnif (hr1 == 0);        /* No bits left to do */
        lq1 fi_<< 3 fi_+ 1 -> lo1;
        lq2 fi_<< 3 fi_+ 1 -> lo2;
    endif;

    /* Do (remaining) bits individually */

    for step (lo1 fi_+ 1 -> lo1,
              lo2 fi_+ 1 -> lo2)
    till (lo1 fi_> hi1)
    do
        if sub_b(lo1, v1) == 0 then 1 else 0 endif -> sub_b(lo2, v2)
    endfor
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 17 1996
        pop_exception_handler in adjust_array now checks for the specific
        id-string 'array-subscr:type-intrange'.
--- John Williams, Feb 15 1996
        adjust_array re-defines pop_exception_handler rather than prmishap.
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Feb 15 1994
        array_dimension_limit now a fixnum (ala Steele 1990 p446).
--- John Williams, Dec 10 1993
        lconstant Isbitarray renamed as permanent identifier isbitarray.
--- John Williams, Jul 12 1993
        No longer uses cons_with.
--- John Williams, Feb 24 1992
        Fixed bug in -vector_push_extend- (cf BR isl-fr.4360)
 */
