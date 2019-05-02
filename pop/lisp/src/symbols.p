/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/symbols.p
 > Purpose:         Symbol manipulating procedures
 > Author:          John Williams, April 27 1987 (see revisions)
 > Documentation:   CLtL, Chapter 10, p163-171
 > Related Files:   SRC * LISPCORE.P
 */

lisp_compile_mode;

section $-lisp;

define symbol_name() with_nargs 1;
    copy(symbol_string())
enddefine;


define symbol_status(sym) -> flag;
    lvars pkg;
    if (symbol_package(sym) ->> pkg) == nil then
        nil -> flag
    else
        find_symbol(symbol_string(sym), pkg) -> flag ->
    endif
enddefine;


define keywordp(item);
    issymbol(item) and symbol_package(item) == keyword_package
enddefine;


define conskeyword(sym);
    sysintern(symbol_string(sym), keyword_package)
enddefine;


/* Property lists */

lconstant procedure Plist_key = fast_front;

define lconstant Checkr_plist(list) -> list;
    fast_back(list) -> list;
    unless ispair(list) do
        lisp_error('Property list not of even length', [])
    endunless
enddefine;


define lconstant Plist_val() with_nargs 1;
    fast_front(Checkr_plist())
enddefine;

define updaterof Plist_val() with_nargs 2;
    -> fast_front(Checkr_plist())
enddefine;


define lconstant Plist_next() with_nargs 1;
    fast_back(Checkr_plist())
enddefine;

define updaterof Plist_next() with_nargs 2;
    -> fast_back(Checkr_plist())
enddefine;


define getf(list, key, default) -> default;
    until endp(list) do
        if Plist_key(list) == key then
            Plist_val(list) -> default;
            return
        endif;
        Plist_next(list) -> list
    enduntil;
    defaults default nil;
enddefine;


define putf(list, key, item) -> result;
    list -> result;
    until endp(list) do
        if Plist_key(list) == key then
            item -> Plist_val(list);
            return
        endif;
        Plist_next(list) -> list
    enduntil;
    conspair(key, conspair(item, result)) -> result
enddefine;


define get(sym, key, default);
    getf(symbol_plist(sym), key, default)
enddefine;


define updaterof get(item, sym, key, default);
    putf(symbol_plist(sym), key, item) -> symbol_plist(sym)
enddefine;


define get_properties(list, keylist);
    until endp(list) do
        if lmember(Plist_key(list), keylist) then
            return(Plist_key(list), Plist_val(list), list)
        endif;
        Plist_next(list) -> list
    enduntil;
    nil, nil, nil
enddefine;


define remf(list, key) -> bool -> result;
    lvars prev = false;
    list -> result;
    until endp(list) do
        if Plist_key(list) == key then
            true -> bool;
            Plist_next(list)
                -> (if prev then Plist_next(prev) else result endif);
            return
        endif;
        list -> prev;
        Plist_next(list) -> list
    enduntil;
    nil -> bool
enddefine;


define remprop(sym, key) -> key;
    remf(symbol_plist(sym), key) -> key -> symbol_plist(sym)
enddefine;


define fast_get(sym, key, default);
    symbol_plist(sym) -> sym;
    until sym == [] do
        if fast_front(sym) == key then
            return(fast_front(fast_back(sym)))
        endif;
        fast_back(fast_back(sym)) -> sym
    enduntil;
    default
enddefine;


/* Generating symbols */

vars gensym_count = 0;

lvars gentemp_count = 0;


define gensymbol(x);
    lvars name = 'G';
    if isintegral(x) then
        make_symbol(name sys_>< x)
    else
        if x then
            get_simple_string(x) -> name
        endif;
        make_symbol(name sys_>< gensym_count);
        gensym_count + 1 -> gensym_count
    endif
enddefine;


define gentemp(prefix, pkg) -> sym;
    lvars name;
    defaults prefix 'T', pkg package;
    get_simple_string(prefix) -> prefix;
    repeat
        prefix sys_>< gentemp_count -> name;
        intern(name, pkg) -> name -> sym;
        quitunless(pop_true(name));
        gentemp_count + 1 -> gentemp_count
    endrepeat
enddefine;


/* Documentation strings */

constant procedure (coerce_string, f_name, fname_sym);

defprop doc_prop;


define lconstant Checkr_doc_item(item) -> item;
    lvars name;
    if ispair(item) then
        fname_sym(item) -> item
    elseif isprocedure(item)
    and issymbol(f_name(item) ->> name) then
        name -> item
    endif
enddefine;


define lconstant Checkr_doc_prop(doc_type, upd) -> prop;
    check_name(doc_type, 'documentation type');
    unless (doc_prop(doc_type) ->> prop) do
        if upd then
            newanyproperty([], 8, 1, 10, false, false, "tmparg", false, false)
                ->> doc_prop(doc_type)
        else
            not
        endif -> prop
    endunless
enddefine;


define documentation(item, doc_type);
    Checkr_doc_prop(doc_type, false)(Checkr_doc_item(item))
enddefine;


define updaterof documentation(string, item, doc_type);
    if string == [] then
        false
    else
        coerce_string(string)
    endif -> Checkr_doc_prop(doc_type, true)(Checkr_doc_item(item))
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        documentation now copes with function names.
--- John Williams, Jan 18 1994
        gensymbol now increments gensym_count unless given an integer as
        argument. This is consistent with Allegro Common Lisp.
--- John Williams, Aug 11 1993
        Fixed gensymbol for Steele 1990.
 */
