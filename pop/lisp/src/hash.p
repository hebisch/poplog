/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/hash.p
 > Purpose:         Common Lisp hash-tables
 > Author:          John Williams, Jul 22 1987 (see revisions)
 > Documentation:   CLtL, ch 16
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

constant procedure (equal, equalp, hash_table_test, symbol_function);


/* Hashing procedures */

define lconstant Hash_list(l, hash_p);
    dlocal pop_hash_lim;

    #_< syshash(pair_key) >_#;      ;;; Running total kept on stack
    if pop_hash_lim /== 0 then
        while ispair(l) do
        pop_hash_lim fi_- 1 -> pop_hash_lim;
        fast_apply(fast_destpair(l) -> l, hash_p);
        unless isinteger(dup()) do
            syshash()
        endunless;
        nonop fi_+ ();
        returnif(pop_hash_lim == 0);
        endwhile;
        if l /== [] then
        fi_+ fast_apply(l, hash_p)
        endif
    endif
enddefine;


define eql_hash(item);
    if iscompound(item) and is_number_key(fast_datakey(item)) then
        syshash(item)
    else
        item
    endif
enddefine;


define equal_hash(item);
    lvars key, a;

    define lconstant Try_string_or_bitvector(item, key, lo, hi);
        if key == string_key
        or key == bitvector_key then
            hash_vector_between(item, lo, hi)
        else
            item
        endif
    enddefine;

    if iscompound(item) then
        fast_datakey(item) -> key;
        if key == symbol_key
        or key == character_key then
            item
        elseif is_number_key(key) then
            syshash(item)
        elseif key == pair_key then
            Hash_list(item, equal_hash)
        elseif key == pathname_key then
            syshash(pathname_key) fi_+ equal_hash(pt_name(item))
        elseif is_vector_key(key) then
            Try_string_or_bitvector(item, key, 1, fast_vector_length(item))
        elseif (isarray(item) ->> a)
        and arrayrank(a) == 1 then
            Try_string_or_bitvector(erase(fast_1d_array_info(a, item)))
        else
            item
        endif
    else
        item
    endif
enddefine;


define equalp_hash(item);
    lvars key, a;

    define lconstant Hash_elements_between(r, b, v, lo, hi, fp, subscr);
        lvars len;
        hi fi_- lo fi_+ 1 -> len;
        if len == 0 then
            r
        else
            equalp_hash(fast_apply(lo, v, subscr));
            if len fi_> 1 then
                fi_+ equalp_hash(fast_apply(hi, v, subscr))
                fi_+ len
            endif
        endif
    enddefine;

    if iscompound(item) then
        fast_datakey(item) -> key;
        if key == symbol_key then
            syshash(item)
        elseif is_number_key(key) then
            number_coerce(item, decimal_0)
        elseif key == character_key then
            uppertolower(fast_char_code(item))
        elseif key == pair_key then
            Hash_list(item, equalp_hash)
        elseif is_record_key(key) then
            syshash(key) fi_+ equalp_hash(fast_record_access(false, item))
        elseif is_vector_key(key) then
            Hash_elements_between(
                1, false, item,
                1, fast_vector_length(item), false,
                lisp_fast_subscr_p(item))
        elseif (isarray(item) ->> a) then
            Hash_elements_between(fast_array_info(a, item))
        elseif isproperty(item) then
            syshash(hash_table_test(item))
        else
            syshash(item)
        endif
    else
        item
    endif
enddefine;


/* Making hash tables */

lconstant
    Eql_hash_ref    = consref(eql_hash),
    Equal_hash_ref  = consref(equal_hash),
    ;



define make_hash_table(test, size, rehash_size, rehash_threshold, Weak);
    lvars hash, testname, gctype;

    /* Determine hashing function and equality test */
    if test == @EQ
    or test == symbol_function(@EQ) then
        false, false, @EQ
    elseif test == @EQL
    or test == symbol_function(@EQL) then
        Eql_hash_ref, nonop ==#, @EQL
    elseif test == @EQUAL
    or test == symbol_function(@EQUAL) then
        Equal_hash_ref, equal, @EQUAL
    elseif test == @EQUALP
    or test == symbol_function(@EQUALP) then
        equalp_hash, equalp, @EQUALP
    else
        lisp_error(
            @SIMPLE-TYPE-ERROR,
            {^@:DATUM ^test
             ^@:EXPECTED-TYPE [^@MEMBER ^@EQ ^@EQL ^@EQUAL ^@EQUALP]
             ^@:FORMAT-STRING 'Unrecognised hash table equality test: ~S'
             ^@:FORMAT-ARGUMENTS [^test]
            })
    endif -> (hash, test, testname);

    /* Round table size up to nearest power of two */
    unless isinteger(size) and size fi_>= 0 do
        lisp_error(
            @SIMPLE-TYPE-ERROR,
            {^@:DATUM ^size
             ^@:EXPECTED-TYPE [^@INTEGER 1]
             ^@:FORMAT-STRING 'Illegal hash table size: ~S'
             ^@:FORMAT-ARGUMENTS [^size]
            })
    endunless;
        if size == 0 then 1 -> size endif;
    round_hash_table_size(size) -> size;

    /* Convert rehash size to power of two */
    if rehash_size == nil then
        1
    elseif (isinteger(rehash_size) and rehash_size fi_> 0) then
        1
    elseif (isdecimal(rehash_size) and rehash_size > 1) then
        /* Convert new/old size ratio to power of 2 */
        ceiling(log(rehash_size), #_<log(2)>_#) ->
    else
        lisp_error(
            @SIMPLE-TYPE-ERROR,
            {^@:DATUM ^rehash_size
             ^@:EXPECTED-TYPE [^@OR [^@INTEGER 1] [^@FLOAT [1.0]]]
             ^@:FORMAT-STRING 'Illegal rehash size: ~S'
             ^@:FORMAT-ARGUMENTS [^rehash_size]
            })
    endif -> rehash_size;

    /* Convert rehash threshold (occupancy ratio) into number of entries */
    if rehash_threshold == nil then
        round(size * 0.7)
    else
        unless isreal(rehash_threshold)
        and rehash_threshold >= 0
        and rehash_threshold <= 1
        do
            lisp_error(
                @SIMPLE-TYPE-ERROR,
                {^@:DATUM ^rehash_threshold
                 ^@:EXPECTED-TYPE [^@REAL 0 1]
                 ^@:FORMAT-STRING 'Illegal rehash threshold: ~S'
                 ^@:FORMAT-ARGUMENTS [^rehash_threshold]
                })
        endunless;
        round(size * rehash_threshold)
    endif -> rehash_threshold;

    /* convert Weak to newanyproperty 7th arg format */
    if Weak == nil then
        "perm" -> gctype
    elseunless (list_assoc(Weak,
                      #_< [^@:KEY tmparg ^@:VALUE tmpval ^@:EITHER tmpboth] >_#
                    ) ->> gctype)
    then
        lisp_error(
            @SIMPLE-TYPE-ERROR,
            {^@:DATUM ^Weak
             ^@:EXPECTED-TYPE [^@MEMBER ^@NIL ^@:KEY ^@:VALUE ^@:EITHER]
             ^@:FORMAT-STRING 'Invalid value supplied for :WEAK argument: ~S'
             ^@:FORMAT-ARGUMENTS [^Weak]
            })
    endif;

    /* Make hash table */
    newanyproperty
        ([], size, rehash_size, rehash_threshold,
         hash, test, gctype, false, false)
        -> hash;
    testname -> pdprops(hash);
    hash
enddefine;


/* Accessing hash tables */

define gethash(key, prop, default);
    if (fast_get_prop_entry(key, checkr_hash_table(prop)) ->> key) then
        fast_prop_entry_value(key), true
    else
        defaults default nil;
        default, nil
    endif
enddefine;


define updaterof gethash(value, key, prop, default);
    pop_true(value) -> checkr_hash_table(prop)(key)
enddefine;


define remhash(key, prop);
    fast_kill_prop_entry(key, checkr_hash_table(prop))
enddefine;


define maphash(pdr, prop);
    checkr_hash_table(prop) -> prop;
    checkr_function(pdr) -> pdr;
    appproperty(prop, procedure() with_nargs 2;
                         lisp_apply(pdr, 2, 0)
                      endprocedure);
    nil
enddefine;


define clrhash() with_nargs 1;
    clearproperty(dup(checkr_hash_table()))
enddefine;


define hash_table_count() with_nargs 1;
    datalength(checkr_hash_table())
enddefine;


define sxhash(item);
    lvars n;
    if isinteger(equal_hash(item) ->> n) then
        if n fi_>= 0 then
            n
        else
            fi_~~ n
        endif
    else
        syshash(n)
    endif
enddefine;


/* WITH-HASH-TABLE-ITERATOR */

define make_hash_table_iterator() with_nargs 1;
    Prop_entry_state_init(checkr_hash_table())
enddefine;


define hash_table_iterator_next(s);
    lvars e;
    if (Prop_entry_state_next(s) ->> e) then
        true, fast_prop_entry_arg(e), fast_prop_entry_value(e)
    else
        nil, nil, nil
    endif
enddefine;


/* Hash-table accessor functions */

define hash_table_size() -> size with_nargs 1;
    hash_table_info() -> (size, , , )
enddefine;


define hash_table_test() with_nargs 1;
    lvars prop, test;
    checkr_hash_table() -> prop;
    if issymbol(pdprops(prop) ->> test) then
        test
    else
        hash_table_info(prop) -> (, , , test);
        test or nonop ==
    endif
enddefine;


define hash_table_rehash_size() with_nargs 1;
    lvars expand;
    hash_table_info() -> (, , expand, );
    if expand then
        number_coerce(1 fi_<< expand, 1.0s0)
    else
        nil
    endif
enddefine;


define hash_table_rehash_threshold(prop);
    lvars size, count, expand;
    hash_table_info(prop) -> (size, count, expand, );
    if expand then
        number_coerce((datalength(prop) fi_+ count) / size, 1.0s0)
    else
        nil
    endif
enddefine;


/* Improve hashing of some Lisp datatypes */

pt_name <> syshash -> class_hash(pathname_key);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 20 1995
        Fixed bug in handling of rehash_threshold when this is a float.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Dec 23 1993
        EQUALP hash tables now implemented. Hashing algorithms for EQL
        and EQUAL hash tables re-written.
--- John Williams, Dec 10 1993
        Now uses round_hash_table_size.
--- John Williams, Dec  9 1993
        Checkr_prop renamed checkr_hash_table, and now defined in
        SRC * LISPCORE.P. Added hash_table_size, hash_table_test,
        hash_table_rehash_size, hash_table_rehash_threshold, and
        support for WITH-HASHTABLE-ITERATOR.
--- John Williams, Jun  6 1989
        Changes relating to -newanyproperty- 7th arg.
--- John Williams, May 11 1988
        Fixed updaterof -gethash- (now calls -pop_true-).
--- John Williams, Sep  7 1987
        Fixed BR johnw.64 (handling of :REHASH-THRESHOLD).
 */
