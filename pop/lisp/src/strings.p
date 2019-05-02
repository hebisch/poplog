/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/strings.p
 > Purpose:         String manipulation functions
 > Author:          John Williams, Nov  1 1990 (see revisions)
 > Documentation:   CLtL, Ch 18
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

define lconstant String_needed
    = type_error(% @STRING %)
enddefine;


define lconstant Simple_string_needed
    = type_error(% @SIMPLE-STRING %)
enddefine;


/* 18.1 String access */

define is_string_array(item);
    isarray(item) and isstring(arrayvector(item)) and arrayrank(item) == 1
enddefine;


define schar(s, i);
    unless isstring(s) do
        Simple_string_needed(s)
    endunless;
    lisp_subscrs(checkr_vector_index(i, s), s)
enddefine;


define updaterof schar(s, i) with_nargs 3;
    unless isstring(s) do
        Simple_string_needed(s)
    endunless;
    -> lisp_subscrs(checkr_vector_index(i, s), s)
enddefine;


define lisp_char(s, i);
    if isstring(s) then
        lisp_subscrs(checkr_vector_index(i, s), s)
    elseif is_string_array(s) then
        fast_apply(i, s)
    else
        String_needed(s)
    endif
enddefine;


define updaterof lisp_char(s, i) with_nargs 3;
    if isstring(s) then
        -> lisp_subscrs(checkr_vector_index(i, s), s)
    elseif is_string_array(s) then
        -> fast_apply(i, s)
    else
        String_needed(s)
    endif
enddefine;


/* Checking and making strings */

define make_string(size, char, type);
    /* Ignore type for now */
    initvectorclass(size, char_code(char), string_key)
enddefine;


define coerce_string(item) -> item;
    unless stringp(item) do
        if issymbol(item) then
            symbol_name(item) -> item
        elseif ischaracter(item) then
            consstring(fast_char_code(item), 1) -> item
        else
            String_needed(item)
        endif
    endunless
enddefine;


define checkr_string_ends(s, start, finish, sym_ok) -> s -> start -> finish;
    lvars lo, hi;
    1 -> lo;
    if isstring(s) then
        fast_vector_length(s) -> hi
    elseif is_string_array(s) then
        arrayseq_bounds(s) -> hi -> lo;
        arrayvector(s) -> s
    elseif issymbol(s) and sym_ok then
        symbol_string(s) -> s;
        fast_vector_length(s) -> hi
    elseif ischaracter(s) and sym_ok then
        lconstant Temp = writeable 'x';
        s -> lisp_subscrs(1, Temp);
        Temp -> s;
        1 -> hi
    else
        String_needed(s)
    endif;
    check_seq_bounds(start, finish, lo, hi) -> start -> finish
enddefine;


/* String comparison */

define lconstant String_=(s1, s2, i1, end1, i2, end2, test_p);
    checkr_string_ends(s1, i1, end1, true) -> s1 -> i1 -> end1;
    checkr_string_ends(s2, i2, end2, true) -> s2 -> i2 -> end2;
    returnunless((end1 fi_- i1) == (end2 fi_- i2)) (false);
    until i1 fi_> end1 do
        returnunless
            (fast_apply(fast_subscrs(i1, s1), fast_subscrs(i2, s2), test_p))
            (false);
        i1 fi_+ 1 -> i1;
        i2 fi_+ 1 -> i2
    enduntil;
    true
enddefine;


define string_=  =
    String_=(% nonop == %)
enddefine;


define string_equal =
    String_=(% caseless_= %)
enddefine;


define lconstant String_compare(s1, s2, i1, end1, i2, end2, test_p);
    lvars c1, c2;
    checkr_string_ends(s1, i1, end1, true) -> s1 -> i1 -> end1;
    checkr_string_ends(s2, i2, end2, true) -> s2 -> i2 -> end2;
    until (i1 fi_> end1) or (i2 fi_> end2) do
        fast_subscrs(i1, s1) -> c1;
        fast_subscrs(i2, s2) -> c2;
        returnunless
            (c1 == c2)
            (fast_apply(c1, c2, test_p) and i1 fi_- 1);
        i1 fi_+ 1 -> i1;
        i2 fi_+ 1 -> i2;
    enduntil;
    fast_apply(end1 fi_- i1, end2 fi_- i2, test_p) and (i1 fi_- 1)
enddefine;

define string_<  =
    String_compare(% nonop fi_< %)
enddefine;


define string_>  =
    String_compare(% nonop fi_> %)
enddefine;


define string_<=  =
    String_compare(% nonop fi_<= %)
enddefine;


define string_>=  =
    String_compare(% nonop fi_>= %)
enddefine;


define string_/=  =
    String_compare(% nonop /== %)
enddefine;


define lconstant Caseless_string_compare(s1, s2, i1, end1, i2, end2, test_p);
    lvars c1, c2;
    checkr_string_ends(s1, i1, end1, true) -> s1 -> i1 -> end1;
    checkr_string_ends(s2, i2, end2, true) -> s2 -> i2 -> end2;
    until (i1 fi_> end1) or (i2 fi_> end2) do
        uppertolower(fast_subscrs(i1, s1)) -> c1;
        uppertolower(fast_subscrs(i2, s2)) -> c2;
        returnunless
            (c1 == c2)
            (fast_apply(c1, c2, test_p) and i1 fi_- 1);
        i1 fi_+ 1 -> i1;
        i2 fi_+ 1 -> i2;
    enduntil;
    fast_apply(end1 fi_- i1, end2 fi_- i2, test_p) and (i1 fi_- 1)
enddefine;


define string_lessp =
    Caseless_string_compare(% nonop fi_< %)
enddefine;


define string_greaterp =
    Caseless_string_compare(% nonop fi_> %)
enddefine;


define string_not_greaterp =
    Caseless_string_compare(% nonop fi_<= %)
enddefine;


define string_not_lessp =
    Caseless_string_compare(% nonop fi_>= %)
enddefine;


define string_not_equal =
    Caseless_string_compare(% nonop /== %)
enddefine;


/* String trim */

define lconstant String_trim(chars, s, left, right);
    lvars lo, hi;
    checkr_string_ends(s, nil, nil, true) -> s -> lo -> hi;
    unless isstring(chars) do
        consstring(dest_seq(chars, true)) -> chars
    endunless;
    if left then
        while (lo fi_<= hi) and strmember(fast_subscrs(lo, s), chars) do
            lo fi_+ 1 -> lo
        endwhile
    endif;
    if right then
        while (hi fi_>= lo) and strmember(fast_subscrs(hi, s), chars) do
            hi fi_- 1 -> hi
        endwhile
    endif;
    substring(lo, hi fi_- lo fi_+ 1, s)
enddefine;


define string_left_trim =
    String_trim(% true, false %)
enddefine;


define string_right_trim =
    String_trim(% false, true %)
enddefine;


define string_trim =
    String_trim(% true, true %)
enddefine;


/* Case conversion */

define lconstant Mapstring(s, start, finish, pdr, copying) -> s;
    lvars i;
    checkr_string_ends(s, start, finish, copying) -> s -> start -> finish;
    if copying then
        copy(s) -> s
    endif;
    fast_for i from start to finish do
        fast_apply(fast_subscrs(i, s), pdr) -> fast_subscrs(i, s)
    endfast_for
enddefine;


define string_downcase =
    Mapstring(% uppertolower, true %)
enddefine;


define string_upcase =
    Mapstring(% lowertoupper, true %)
enddefine;


define nstring_downcase =
    Mapstring(% uppertolower, false %)
enddefine;


define nstring_upcase =
    Mapstring(% lowertoupper, false %)
enddefine;


lvars procedure Case_convert_p;

define lconstant Capitalize(char);
    fast_apply(char, Case_convert_p);
    if isalphacode(char) or isnumbercode(char) then
        uppertolower
    else
        lowertoupper
    endif -> Case_convert_p
enddefine;


define string_capitalize() with_nargs 3;
    lowertoupper -> Case_convert_p;
    Mapstring(Capitalize, true)
enddefine;


define nstring_capitalize() with_nargs 3;
    lowertoupper -> Case_convert_p;
    Mapstring(Capitalize, false)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Apr 20 1994
        char no longer observes fill-pointers (draft ANSI(2) 16-5)
--- John Williams, Aug 31 1993
        Upgraded to Steele 1990. Tidied up.
--- John Williams, Feb 21 1992
        Changed -check_string- to -checkr_string_ends- (cf BR isl-fr.4412)
--- John Williams, Nov  1 1990
        Fixed bug in -String_trim-
*/
