/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/seq.p
 > Purpose:         Common Lisp sequence functions
 > Author:          John Williams, Dec 7 1986 (see revisions)
 > Documentation:   CLtL, p245-261
 > Related Files:   C.all/lisp/src/seq.lsp
 */

lisp_compile_mode;

section $-lisp;

fastprocs repeat, for, front, back, destpair, +, -, <, >, <=, >=;


constant procedure (copy_list, listp, nth);


/* Recognising sequences */

define vectorp(x);
    if isvectorclass(x) then true else isarray(x) and arrayrank(x) == 1 endif
enddefine;


define stringp(x);
    isstring(x)
        or
    (isarray(x) and arrayrank(x) == 1 and isstring(arrayvector(x)))
enddefine;


define bitvectorp(x);
    isbitvector(x)
        or
    (isarray(x) and arrayrank(x) == 1 and isbitvector(arrayvector(x)))
enddefine;


define sequencep(x);
    listp(x) or vectorp(x)
enddefine;


define is_array_seq(x) -> x;
    unless (isarray(x) ->> x) and arrayrank(x) == 1 do
        false -> x
    endunless
enddefine;


define lconstant Seq_needed(item);
    if item == [] then
        lisp_error('Non-empty sequence needed', [[]])
    else
        type_error(item, @SEQUENCE)
    endif
enddefine;


/* Checking sequence bounds */

define arrayseq_bounds(a) -> hi -> lo;
    lvars fp;
    arrayvector_bounds(a) -> lo -> hi;
    if isinteger(fast_fill_pointer(a) ->> fp) then
        fp + lo - 1 -> hi
    endif
enddefine;


define Vseq_bounds(seq) -> (seq, lo, hi);
    if isvectorclass(seq) then
        1 -> lo;
        fast_vector_length(seq) -> hi
    elseif is_array_seq(seq) then
        arrayseq_bounds(seq) -> hi -> lo;
        arrayvector(seq) -> seq
    else
        Seq_needed(seq)
    endif
enddefine;


define check_seq_bounds(user_lo, user_hi, lo, hi) -> lo -> hi;
    lvars real_hi;
    if pop_true(user_hi) then
        hi -> real_hi;
        lo + check_positive(user_hi) - 1 -> hi;
        if hi > real_hi then
            lisp_error('End-index (~D) exceeds sequence length', [^user_hi])
        endif
    endif;
    if pop_true(user_lo) then
        lo + check_positive(user_lo) -> lo;
        if lo > (hi + 1) then
            lisp_error('Start-index (~D) exceeds end-index (~D)',
                       [^user_lo ^user_hi])
        endif
    endif
enddefine;


define lconstant Check_hi_lo(hi, lo) -> n;
    if pop_true(hi) then
        check_positive(hi) - lo -> n;
        if n < 0 then
            lisp_error('Start index (~D) exceeds end index (~D)', [^lo ^hi])
        endif
    else
        false -> n
    endif
enddefine;


define checkr_array_seq_index(i, seq) -> i;
    lvars fp;
    if (fast_fill_pointer(seq) ->> fp)
    and i >= fp then
        lisp_error('Index (~D) exceeds fill-pointer (~D)', [^i ^fp])
    endif
enddefine;


/* Sequence access */

define elt(seq, i);
    if listp(seq) then
        nth(i, seq)
    elseif isvectorclass(seq) then
        fast_vector_access(i, seq)
    elseif is_array_seq(seq) then
        fast_apply(checkr_array_seq_index(i, seq), seq)
    else
        Seq_needed(seq)
    endif
enddefine;


define updaterof elt(seq, i) with_nargs 3;
    if ispair(seq) then
        -> nth(i, seq)
    elseif isvectorclass(seq) then
        -> fast_vector_access(i, seq)
    elseif is_array_seq(seq) then
        -> fast_apply(checkr_array_seq_index(i, seq), seq)
    else
        Seq_needed(seq)
    endif
enddefine;


define seq_length(seq) -> len;
    if listp(seq) then
        0 -> len;
        until endp(seq) do
            len + 1 -> len;
            back(seq) -> seq
        enduntil
    elseif isvectorclass(seq) then
        fast_vector_length(seq) -> len
    elseif is_array_seq(seq) then
        fast_fill_pointer(seq) or array_used_size(seq) -> len
    else
        Seq_needed(seq)
    endif
enddefine;


/* Copying portions of sequences */

define lconstant Seq_back(seq, i, flag);
    if ispair(seq) then
        back(seq)
    else
        lisp_error('~A index (~D) exceeds sequence length', [^flag ^i])
    endif
enddefine;


define lconstant Seq_destpair(seq, i, flag);
    if ispair(seq) then
        destpair(seq)
    else
        lisp_error('~A index (~D) exceeds sequence length', [^flag ^i])
    endif
enddefine;


define lconstant Subvseq(seq, start, finish) -> newseq;
    lvars lo, hi;
    Vseq_bounds(seq) -> (seq, lo, hi);
    check_seq_bounds(start, finish, lo, hi) -> start -> finish;
    finish - start + 1 -> finish;
    class_init(fast_datakey(seq))(finish) -> newseq;
    move_subvector(start, seq, 1, newseq, finish)
enddefine;


define subseq(seq, lo, hi);
    if listp(seq) then
        repeat check_positive(lo) times
            Seq_back(seq, lo, 'Start') -> seq
        endrepeat;
        if (Check_hi_lo(hi, lo) ->> lo) then
            [% repeat lo times
                Seq_destpair(seq, hi, 'End') -> seq
            endrepeat %]
        else
            copy_list(seq)
        endif
    else
        Subvseq(seq, lo, hi)
    endif
enddefine;


define copy_seq(seq);
    if isvectorclass(seq) then
        copy(seq)
    elseif listp(seq) then
        copy_list(seq)
    else
        Subvseq(seq, 0, false)
    endif
enddefine;


define nreverse(seq) -> seq;
    lvars i, j, procedure subscr, vec;
    if listp(seq) then
        seq -> i;
        [] -> seq;
        until endp(i) do
            (back(i), i, seq) -> (i, seq, back(i))
        enduntil
    else
        for Vseq_bounds(seq) -> (vec, i, j);
            class_fast_subscr(fast_datakey(vec)) -> subscr;
        step i + 1 -> i;
             j - 1 -> j
        till j <= i do
            subscr(j, vec), subscr(i, vec) -> subscr(j, vec) -> subscr(i, vec)
        endfor
    endif
enddefine;


define reverse(seq) -> new;
    if listp(seq) then
        [] -> new;
        until endp(seq) do
            conspair(destpair(seq) -> seq, new) -> new
        enduntil
    else
        nreverse(copy_seq(seq)) -> new
    endif
enddefine;


/* Constructing and exploding sequences */

define lconstant Checkr_stype_key(type) -> (key, xtype);
    stype_->_key(type) -> (key, xtype);
    unless key do
        lisp_error('Invalid sequence type specifier', [^type])
    endunless
enddefine;


define lconstant Check_stype_size(xtype, size);
    lvars dims;
    if ispair(xtype) then
        nth(2, xtype) -> dims;
        if ispair(dims) then
            unless back(dims) == []
            and (front(dims) == @* or front(dims) == size) do
                lisp_error(
                    'Sequence length (~D) incompatible with type specifier',
                    [^size ^xtype])
            endunless
        endif
    endif
enddefine;


define make_sequence(type, size, init);
    lvars key;
    Checkr_stype_key(type) -> (key, type);
    Check_stype_size(type, size);
    if init == nil then
        key_->_init(key) -> init
    endif;
    if key == string_key then
        char_code(init) -> init
    endif;
    if key == pair_key then
        check_positive(size) ->;
        conslist(repeat size times init endrepeat, size)
    else
        initvectorclass(size, init, key)
    endif
enddefine;


define explode_seq(seq, for_string);
    lvars s;
    if ispair(seq) then
        if for_string then
            applist(seq, char_code)
        else
            destlist(seq) ->
        endif
    elseif isstring(seq) then
        if for_string then
            deststring(seq) ->
        else
            appdata(seq, fast_code_char)
        endif
    elseif isvectorclass(seq) then
        if for_string then
            appdata(seq, char_code)
        else
            explode(seq)
        endif
    elseif (is_array_seq(seq) ->> s) then
        if for_string then
            appdata(s, char_code)
        else
            explode(s)
        endif;
        if isinteger(fast_fill_pointer(seq) ->> for_string) then
            erasenum(datalength(s) - for_string)
        endif
    elseunless seq == [] do
        Seq_needed(seq)
    endif
enddefine;


define dest_seq(seq, for_string);
    #| explode_seq(seq, for_string) |#
enddefine;


define fill_vseq(seq, for_string);
    lvars lo, hi, key, procedure usubscr, i;
    Vseq_bounds(seq) -> (seq, lo, hi);
    fast_datakey(seq) -> key;
    if key == string_key and not(for_string) then
        #_< updater(lisp_subscrs) >_#
    else
        updater(class_fast_subscr(key))
    endif -> usubscr;
    for i from hi by -1 to lo do
        usubscr(i, seq)
    endfor
enddefine;


define concatenate(type, seqs);
    lvars key, cons_pdr, for_string, len;
    Checkr_stype_key(type) -> (key, type);
    if key == pair_key then
        conslist
    else
        class_cons(key)
    endif -> cons_pdr;
    cons_pdr == consstring -> for_string;
    (#| until seqs == nil do
            explode_seq(destpair(seqs) -> seqs, for_string)
        enduntil
    |#) -> len;
    Check_stype_size(type, len);
    fast_apply(len, cons_pdr)
enddefine;


/* Removing & deleting from sequences */

define lconstant Apply_seq_test(key, test) with_nargs 3;
    lisp_apply((), key, 1, 1);
    lisp_apply(test, 1, 1);
    pop_true()
enddefine;


define lconstant List_remove_if(test, key, list, lo, hi, count, from_end) -> list;
    lvars start_list, removed, sl, n, item;

    list -> start_list;
    false -> removed;
    stacklength() -> sl;

    if pop_true(lo) then
        repeat check_positive(lo) times
            Seq_destpair(list, lo, 'Start') -> list
        endrepeat
    else
        0 -> lo
    endif;

    if from_end then
        if (Check_hi_lo(hi, lo) ->> n) then
            repeat n times
                Seq_destpair(list, hi, 'End') -> list
            endrepeat
        else
            destlist(list) -> n
        endif;
        repeat n times
            quitif(count == 0);
            -> item;
            if Apply_seq_test(item, key, test) then
                unless removed do
                    start_list -> list;
                    repeat stacklength() - sl + 1 times
                        back(list) -> list
                    endrepeat
                endunless;
                count - 1 ->> count -> removed
            elseif removed then
                conspair(item, list) -> list
            endif
        endrepeat
    elseif (Check_hi_lo(hi, lo) ->> n) then
        repeat n times
            quitif(count == 0);
            Seq_destpair(list, hi, 'End') -> list -> item;
            if Apply_seq_test(item, key, test) then
                count - 1 ->> count -> removed
            else
                item
            endif
        endrepeat
    else
        until endp(list) or count == 0 do
            destpair(list) -> list -> item;
            if Apply_seq_test(item, key, test) then
                count - 1 ->> count -> removed
            else
                item
            endif
        enduntil
    endif;

    if removed then
        repeat stacklength() - sl times
            conspair(list) -> list
        endrepeat
    else
        setstacklength(sl);
        start_list -> list
    endif
enddefine;


define lconstant List_delete_if(test, key, list, lo, hi, count, from_end) -> result;
    lvars n, next;

    if pop_true(lo)
    and check_positive(lo) > 0 then
        list -> result;
        repeat lo - 1 times
            Seq_back(list, lo, 'Start') -> list
        endrepeat
    else
        conspair(false, list) ->> list -> result;
        0 -> lo
    endif;

    if from_end then
        if (Check_hi_lo(hi, lo) ->> n) then
            repeat (n + 1 ->> n) times
                list;
                Seq_back(list, hi, 'End') -> list
            endrepeat
        else
            0 -> n;
            until endp(list) do
                list;
                back(list) -> list;
                n + 1 -> n
            enduntil
        endif;
        until n == 1 or count == 0 do
            -> list;
            if Apply_seq_test(front(list), key, test) then
                count - 1 -> count;
                ->> next;
                back(list) -> back(next)
            endif;
            n - 1 -> n
        enduntil;
        erasenum(n)

    elseif (Check_hi_lo(hi, lo) ->> n) then
        until n == 0 or count == 0 do
            n - 1 -> n;
            Seq_back(list, hi, 'End') -> next;
            if Apply_seq_test(front(next), key, test) then
                count - 1 -> count;
                back(next) -> back(list)
            else
                next -> list
            endif
        enduntil
    else
        until count == 0 or endp(back(list) ->> next) do
            if Apply_seq_test(front(next), key, test) then
                count - 1 -> count;
                back(next) -> back(list)
            else
                next -> list
            endif
        enduntil
    endif;

    if lo == 0 then
        back(result) -> result
    endif
enddefine;


define lconstant Vector_remove_if(test, key, seq, lo, hi, count, from_end) -> seq;
    lvars start, len, dkey, for_string, procedure subscr,
            removed, sl, i, item, templist;

    Vseq_bounds(seq) -> (seq, start, len);
    check_seq_bounds(lo, hi, start, len) -> lo -> hi;

    fast_datakey(seq) -> dkey;
    dkey == string_key -> for_string;
    class_fast_subscr(dkey) -> subscr;

    false -> removed;
    stacklength() -> sl;

    for i from start to lo - 1 do
        subscr(i, seq)
    endfor;
    if for_string then
        lisp_subscrs -> subscr
    endif;
    if from_end then
        [] -> templist;
        for i from hi by -1 to lo do
            subscr(i, seq) -> item;
            if count > 0 and Apply_seq_test(item, key, test) then
                count - 1 -> count;
                true -> removed
            elseif for_string then
                conspair(char_code(item), templist) -> templist
            else
                conspair(item, templist) -> templist
            endif
        endfor;
        hi + 1 -> i
    else
        for i from i to hi do
            quitif(count == 0);
            subscr(i, seq) -> item;
            if Apply_seq_test(item, key, test) then
                count - 1 -> count;
                true -> removed
            elseif for_string then
                char_code(item)
            else
                item
            endif
        endfor
    endif;
    if removed then
        if from_end then
            while ispair(templist) do
                sys_grbg_destpair(templist) -> templist
            endwhile
        endif;
        if for_string then
            fast_subscrs -> subscr
        endif;
        for i from i to len do
            subscr(i, seq)
        endfor;
        stacklength() - sl -> sl;
        fast_apply(sl, class_cons(dkey)) -> seq
    else
        setstacklength(sl)
    endif
enddefine;


define remove_if(test, seq, from_end, lo, hi, count, key, ncopy);

    checkr_function(test) -> test;
    checkr_function(key) -> key;
    ;;; Assumes count is an integer
    pop_true(from_end) -> from_end;

    fast_apply(
        test, key, seq, lo, hi, count, from_end,
        if listp(seq) then
            if pop_true(ncopy) then
                List_delete_if
            else
                List_remove_if
            endif
        else
            Vector_remove_if
        endif)
enddefine;


/* Removing duplicates */

define lconstant Apply_seq_test2(x, y, key, test);
    lisp_apply(x, key, 1, 1);
    lisp_apply(y, key, 1, 1);
    lisp_apply(test, 2, 1);
    pop_true()
enddefine;


define lconstant Isduplicate(item, list, key, test);
    until endp(list) do
        if Apply_seq_test2(item, destpair(list) -> list, key, test) then
            return(true)
        endif
    enduntil;
    false
enddefine;


define lconstant Isduplicate_from_end(item, list, key, test, here);
    until list == here or endp(list) do
        if Apply_seq_test2(item, destpair(list) -> list, key, test) then
            return(true)
        endif
    enduntil;
    false
enddefine;


define lconstant Del_dup(list, key, test) -> list;
    lvars next;
    until endp(back(list) ->> next) do
        if Isduplicate(front(next), back(next), key, test) then
            back(next) -> back(list)
        else
            next -> list
        endif
    enduntil
enddefine;


define lconstant Del_dup_from_end(list, key, test) -> list;
    lvars start, next;
    back(list) -> start;
    until endp(back(list) ->> next) do
        if Isduplicate_from_end(front(next), start, key, test, next) then
            back(next) -> back(list)
        else
            next -> list
        endif
    enduntil
enddefine;


define lconstant List_remove_duplicates(list, from_end, test, lo, hi, key);
    lvars result, work, lastwk;

    conspair(false, nil) ->> result -> work;
    if pop_true(lo) then
        repeat check_positive(lo) times
            conspair(Seq_destpair(list, lo, 'Start') -> list, nil)
                ->> back(work) -> work
        endrepeat
    endif;

    work -> lastwk;
    if (Check_hi_lo(hi, lo or 0) ->> lo) then
        repeat lo times
            conspair(Seq_destpair(list, hi, 'End') -> list, nil)
                ->> back(lastwk) -> lastwk
        endrepeat
    else
        until endp(list) do
            conspair(destpair(list) -> list, nil)
                ->> back(lastwk) -> lastwk
        enduntil
    endif;

    list -> back(if pop_true(from_end) then
                          Del_dup_from_end(work, key, test)
                      else
                          Del_dup(work, key, test)
                      endif);
    back(result)
enddefine;


define lconstant List_delete_duplicates(list, from_end, test, lo, hi, key) -> result;
    lvars work, n;

    if pop_true(lo) and check_positive(lo) > 0 then
        list -> result;
        repeat lo - 1 times
            Seq_back(list, lo, 'Start') -> list
        endrepeat
    else
        0 -> lo;
        conspair(false, list) ->> result -> list
    endif;

    list -> work;
    if (Check_hi_lo(hi, lo) ->> n) then
        repeat n times
            Seq_back(list, hi, 'End') -> list
        endrepeat;
        back(list);
        nil -> back(list)
    else
        nil
    endif -> back(if pop_true(from_end) then
                           Del_dup_from_end(work, key, test)
                       else
                           Del_dup(work, key, test)
                       endif);

    if lo == 0 then
        back(result) -> result
    endif
enddefine;


define remove_duplicates(seq, lo, hi, from_end, test, key, ncopy) -> seq;
    lvars start, len, dkey, procedure subscr, sl, i, list, newlen;

    checkr_function(key) -> key;
    checkr_function(test) -> test;

    if listp(seq) then
        if pop_true(ncopy) then
            List_delete_duplicates
        else
            List_remove_duplicates
        endif(seq, from_end, test, lo, hi, key) -> seq;
        return
    endif;

    Vseq_bounds(seq) -> (seq, start, len);
    check_seq_bounds(lo, hi, start, len) -> lo -> hi;
    fast_datakey(seq) -> dkey;
    class_fast_subscr(dkey) -> subscr;
    stacklength() -> sl;

    for i from start to (lo - 1) do
        subscr(i, seq)
    endfor;

    [% false,
    if dkey == string_key then
        for i from i to hi do lisp_subscrs(i, seq) endfor
    else
        for i from i to hi do subscr(i, seq) endfor
    endif %] -> list;

    if pop_true(from_end) then
        Del_dup_from_end
    else
        Del_dup
    endif (list, key, test) ->;

    dest_seq(back(list), dkey == string_key) -> newlen;

    if pop_true(ncopy) and newlen == (hi - lo + 1) then
        /* Nothing deleted - so return original */
        setstacklength(sl)
    else
        for i from i to len do subscr(i, seq) endfor;
        class_cons(dkey)(stacklength() - sl) -> seq
    endif
enddefine;


/* Sorting sequences */

constant procedure (symbol_function);

defprop
    numeric_sort_pred,
    char_sort_pred,
    stable_numeric_sort_pred,
    stable_char_sort_pred,
    ;


define setup_sort_preds();
    /* This procedure is run after building the Lisp system, in
        C.all/lisp/src/clisp.p
       Use valof to beat fastprocs declarations.
    */
    valof("<")      -> numeric_sort_pred(symbol_function(@<));
    valof("<=")     -> numeric_sort_pred(symbol_function(@<=));
    valof(">")      -> numeric_sort_pred(symbol_function(@>));
    valof(">=")     -> numeric_sort_pred(symbol_function(@>=));
    valof("<=")     -> stable_numeric_sort_pred(symbol_function(@<=));
    valof(">=")     -> stable_numeric_sort_pred(symbol_function(@>=));
    valof("fi_<")   -> char_sort_pred(symbol_function(@CHAR<));
    valof("fi_<=")  -> char_sort_pred(symbol_function(@CHAR<=));
    valof("fi_>")   -> char_sort_pred(symbol_function(@CHAR>));
    valof("fi_>=")  -> char_sort_pred(symbol_function(@CHAR>=));
    valof("fi_<=")  -> stable_char_sort_pred(symbol_function(@CHAR<=));
    valof("fi_>=")  -> stable_char_sort_pred(symbol_function(@CHAR>=));
enddefine;


define lconstant Stable_apply_seq_test2(x, y, key, test);
    lisp_apply(x, key, 1, 1) -> x;
    lisp_apply(y, key, 1, 1) -> y;
    if pop_true(lisp_apply(x, y, test, 2, 1)) then
        true
    elseif pop_true(lisp_apply(y, x, test, 2, 1)) then
        false
    else
        1
    endif
enddefine;


define sort_seq(seq, pred, key, stable) -> seq;
    lvars before_p, for_string = false, list;
    fastprocs frozval;

    lconstant Before_p_clos
        = writeable Apply_seq_test2(% identfn, identfn %);

    dlocal
        % pdpart(Before_p_clos) %,
        % frozval(1, Before_p_clos) %,
        % frozval(2, Before_p_clos) %,
        ;

    returnif(seq == []);
    checkr_function(pred) -> pred;
    if key == nil then
        ;;; defaults
        identfn
    else
        checkr_function(key)
    endif -> key;

    if pop_true(stable) then
        unless key == identfn
        and (stable_numeric_sort_pred(pred) ->> before_p)
        or  ((stable_char_sort_pred(pred) ->> before_p)
                and
             (stringp(seq) ->> for_string))
        do
            Stable_apply_seq_test2 -> pdpart(Before_p_clos);
            Before_p_clos -> before_p
        endunless
    else
        unless key == identfn
        and (numeric_sort_pred(pred) ->> before_p)
        or  ((char_sort_pred(pred) ->> before_p)
                and
             (stringp(seq) ->> for_string))
        do
            Apply_seq_test2 -> pdpart(Before_p_clos);
            Before_p_clos -> before_p
        endunless
    endif;
    if before_p == Before_p_clos then
        key -> frozval(1, Before_p_clos);
        pred -> frozval(2, Before_p_clos)
    endif;

    if ispair(seq) then
        nc_listsort(seq, before_p) -> seq
    else
        /* Convert vector to a list, and sort that. Much quicker than
            using a "quicksort" algorithm on the vector directly
            (twice as quick for 5000 items).
        */
        [% explode_seq(seq, for_string) %] -> list;
        nc_listsort(list, before_p) -> list;
        until list == [] do
            sys_grbg_destpair(list) -> list
        enduntil;
        fill_vseq(seq, for_string)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  3 1996
        elt now works on nil (as it does in ACL 4.3).
--- John Williams, Oct 27 1995
        Added fastprocs declarations.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Sep  1 1993
        More improvements to sort_seq.
--- John Williams, Aug 27 1993
        Upgraded to Steele 1990.
        sort_seq now optimises common sort predicates.
--- John Williams, Jul 29 1993
        The stable option to sort_seq now works correctly.
        Updater of elt given correct pdnargs (3).
--- John Williams, Jul 28 1993
        Uses new nc_listsort.
--- John Williams, Jul 27 1993
        Fixed bug in updater of elt.
--- John Williams, Jul 12 1993
        No longer uses cons_with.
 */
