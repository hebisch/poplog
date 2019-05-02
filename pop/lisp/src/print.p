/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lisp/src/print.p
 > Purpose:         Printing procedures for built in data-types
 > Author:          John Williams, Nov 28 1985 (see revisions)
 > Documentation:   CLtL, p283-289
 > Related Files:   C.all/lisp/src/pprint.p, C.all/lisp/src/defstruct.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs destpair, for, frozval, subscrs;

constant procedure (fboundp, pr_sym_name);

vars procedure process_pid = not;


global vars
    ;;; global 'cos used in format_print
    print_array     =   true,
    print_escape    =   true,
    print_gensym    =   true,
    print_radix     =   nil,
    ;


global vars active (
    print_base   = integer_range_variable
                    (% ident pop_pr_radix, 2, 36, @*PRINT-BASE* %),
        );


/* Utilities */

lconstant procedure Popintpr = sys_syspr;

define lconstant Popfloatpr() with_nargs 1;
    dlocal pop_pr_radix = 10, pop_pr_exponent = false;
    sys_syspr()
enddefine;


define lconstant Decpr() with_nargs 1;
    dlocal pop_pr_radix = 10;
    Popintpr()
enddefine;


lconstant Pr_unreadable_quote = '*PR-UNREADABLE-QUOTE*';

define lconstant Pr_unreadable();
    dlvars list;
    sysconslist() -> list;
    print_logical_block(
        '#<', '>',
        procedure();
            lvars item;
            dlocal list;  /* This proc called twice if print_circle is set */

            print_indent(@:CURRENT, 0);
            repeat
                destpair(list) -> (item, list);
                if item == Pr_unreadable_quote and ispair(list) then
                    _lisppr(destpair(list) -> list)
                elseif isstring(item) then
                    procedure() with_nargs 1;
                        dlocal print_escape = nil;
                        pr_sym_name()
                    endprocedure(item)
                else
                    _lisppr(item)
                endif;
                quitif(list == []);
                cucharout(`\s`);
                print_newline(@:FILL)
            endrepeat
        endprocedure)
enddefine;


/* Printing arrays, vectors, strings etc */

define lconstant Short_array_pr(a);
    Pr_unreadable(popstackmark, 'ARRAY',
                    array_element_type(a), array_dimensions(a))
enddefine;


define lconstant Pr_bv_between(bv, lo, hi);
    dlvars bv, lo, hi;
    print_logical_block(
        '#*', false,
        procedure();
            lvars i;
            for i from lo to hi do
                cucharout(fast_subscrbitvector(i, bv) fi_+ `0`)
            endfor
        endprocedure)
enddefine;


define lconstant Pr_bitvector(bv);
    if print_array /== nil
    or print_readably /== nil
    then
        Pr_bv_between(bv, 1, fast_vector_length(bv))
    else
        Short_array_pr(bv)
    endif
enddefine;


define lconstant Ppr_string_between(str, lo, hi);
    lvars i, c;
    cucharout(`"`);
    for i from lo to hi do
        subscrs(i, str) -> c;
        if c == `\\` or c == `"` then
            cucharout(`\\`)
        endif;
        cucharout(c)
    endfor;
    cucharout(`"`)
enddefine;


define lconstant Pr_string_between(str, lo, hi);
    lvars i;
    if print_escape /== nil
    or print_readably /== nil
    then
        Ppr_string_between(str, lo, hi)
    else
        for i from lo to hi do
            cucharout(subscrs(i, str))
        endfor
    endif
enddefine;


define lconstant Pr_string(str);
    Pr_string_between(str, 1, fast_vector_length(str))
enddefine;


define lconstant Pr_vec_between(vec, lo, hi);
    dlvars vec, lo, hi, procedure subscr;
    lisp_fast_subscr_p(vec) -> subscr;
    print_logical_block(
        '(', ')',
        procedure();
            lvars i;
            dlocal length_printed = 0;
            print_indent(@:CURRENT, 0);
            for i from lo to hi do
                CHECK_PR_LEN;
                _lisppr(subscr(i, vec));
                unless i == hi do
                    cucharout(`\s`);
                    print_newline(@:FILL)
                endunless
            endfor
        endprocedure)
enddefine;


define lconstant Pr_vector(vec);
    dlocal depth_printed;

    if print_array /== nil
    or print_readably /== nil
    then
        unless isvector(vec) do
            CHECK_PRINT_READABLY vec
        endunless;
        CHECK_PR_LEVEL;
        cucharout(`#`);
        Pr_vec_between(vec, 1, fast_vector_length(vec))
    else
        Short_array_pr(vec)
    endif
enddefine;


define lconstant Pr_array(a);
    lvars rank;
    dlvars I, procedure Printer, Vec;

    if print_array == nil
    and print_readably == nil
    then
        fast_chain(a, Short_array_pr)
    endif;

    arrayvector(a) -> Vec;
    array_rank(a) -> rank;

    if isvector(Vec) then
        Pr_vec_between          /* Wrong if printing prettily */
    elseif isstring(Vec) then
        if rank == 1 then
            Pr_string_between
        else
            Ppr_string_between
        endif
    elseif isbitvector(Vec) then
        Pr_bv_between
    else
        CHECK_PRINT_READABLY a;
        Pr_vec_between
    endif -> Printer;

    if rank == 0 then
        cucharout(`#`);
        cucharout(`0`);
        cucharout(`A`);
        cucharout(`\s`);
        _lisppr(a())
    elseif rank == 1 then
        if Printer == Pr_vec_between then
            cucharout(`#`)
        endif;
        Printer(Vec, arrayseq_bounds(a))
    else

        define lconstant Array_pr(bounds);
            lvars hi, lo;
            dlocal depth_printed, length_printed = 0;

            CHECK_PR_LEVEL;
            destpair(destpair(bounds)) -> bounds -> hi -> lo;
            if bounds == [] then
                I fi_+ (hi fi_- lo) -> hi;
                Printer(Vec, I, hi);
                hi fi_+ 1 -> I
            else
                print_logical_block(
                    '(', ')',
                    procedure();
                        for lo from 0 to (hi fi_- lo) do
                            CHECK_PR_LEN;
                            Array_pr(bounds);
                            unless lo == hi do
                                cucharout(`\s`);
                                print_newline(@:FILL)
                            endunless;
                        endfor
                    endprocedure)
            endif
        enddefine;

        arrayvector_bounds(a) -> I ->;
        cucharout(`#`);
        Decpr(rank);
        cucharout(`A`);
        Array_pr(boundslist(a))
    endif
enddefine;


/* Printing Lists */

define lconstant Do_list(list, f_mode);
    dlvars item, save;
    dlocal depth_printed;

    front(list) -> item;
    if (item == @QUOTE or item == @FUNCTION) and islistlength(list, 2) then
        print_logical_block(
            false, false,
            procedure();
                if item == @FUNCTION then cucharout(`#`) endif;
                cucharout(`'`);
                _lisppr(front(back(list)))
            endprocedure)
    else
        CHECK_PR_LEVEL;
        f_mode and (datakey(item) == symbol_key) and fboundp(item)
            -> f_mode;
        print_logical_block(
            '(', ')',
            procedure();
                lvars nl_kind = (if f_mode then @:LINEAR else @:FILL endif),
                      l;
                dlocal length_printed = 0;
                repeat
                    CHECK_PR_LEN;
                    destpair(list) -> (item, list);
                    if ispair(item) then
                        unless print_by_key_hook(item) do
                            Do_list(item, f_mode)
                        endunless
                    else
                        _lisppr(item)
                    endif;
                    quitif(atom(list));
                    cucharout(`\s`);
                    returnif(pc_list_tail_hook(list));
                    if f_mode
                    and length_printed == 1 then
                        datalength(symbol_string(item)) -> l;
                        print_indent(@:BLOCK,
                                     if l <= 8 then l + 1 else 3 endif);
                        print_newline(@:FILL)
                    else
                        print_newline(nl_kind)
                    endif
                endrepeat;
                unless list == [] do
                    cucharout(`\s`);
                    cucharout(`.`);
                    cucharout(`\s`);
                    _lisppr(list)
                endunless
            endprocedure)
    endif
enddefine;


define lconstant Pr_list() with_nargs 1;
    Do_list(true)
enddefine;


/* Printing symbol names */

define lconstant No_escapes_needed(s, procedure isalpha);
    lvars l, c, i;
    fast_vector_length(s) -> l;
    returnif(l == 0) (false);
    subscrs(1, s) -> c;
    if c == `+` or c == `-` then
        returnif(l == 1) (true);
        subscrs(2, s) -> c
    endif;
    if isdigitcode(c, pop_pr_radix) then
        subscrs(l, s) -> c;
        if isdigitcode(c, pop_pr_radix) or c == `.` then
            return(false)
        endif
    endif;
    for c with_index i in_string s do
        unless isalpha(c)
                or (c fi_>= `0` and c fi_<= `9`)
                or strmember(c, '&*+-/<=>_')
        do
            return(false)
        endunless
    endfor;
    true
enddefine;


define lconstant Escape_pr(s);
    cucharout(`|`);
    appdata(s, 
                procedure(c);
                    if c = `\\` or c = `|` then
                        cucharout(`\\`);
                    endif;
                    cucharout(c);
                endprocedure);
    cucharout(`|`);
enddefine;


lvars
    First               =   true,
    Print_case          =   @:UPCASE,
    procedure
    Print_case_convert  =   lowertoupper,
    ;


define global active print_case;
    Print_case
enddefine;


define updaterof active print_case(new);
    if new == @:UPCASE then
        lowertoupper
    elseif new == @:DOWNCASE then
        uppertolower
    elseif new == @:CAPITALIZE then
        procedure(c);
            if First then lowertoupper(c) else uppertolower(c) endif;
            not(isalphacode(c) or isnumbercode(c)) -> First
        endprocedure
    elseif new == nil then
        identfn
    else
        warn('Ignoring illegal value for *PRINT-CASE*', [^new]) ->;
        return
    endif -> Print_case_convert;
    new -> Print_case
enddefine;


define lconstant Pr_sym_name(s);
    lvars c;
    dlocal First = true;
    for c in_string s do
        cucharout(Print_case_convert(c))
    endfor
enddefine;


define pr_sym_name(name);
    lvars rcase, isalpha, c;
    dlocal Print_case_convert;

    if print_escape == nil then
        Pr_sym_name(name)
    else
        readtable_case_sym(readtable) -> rcase;
        if rcase == @:UPCASE then
            isuppercode
        elseif rcase == @:DOWNCASE then
            islowercode
        else
            isalphacode
        endif -> isalpha;
        if No_escapes_needed(name, isalpha) then
            if isalpha == isalphacode then
                if rcase == @:INVERT
                and (all_same_case(name) ->> c)
                and Print_case_convert /== identfn
                then
                    if isuppercode(c) then
                        uppertolower
                    else
                        lowertoupper
                    endif -> Print_case_convert;
                    Pr_sym_name(name)
                else
                    appdata(name, cucharout)
                endif
            else
                Pr_sym_name(name)
            endif
        else
            Escape_pr(name)
        endif
    endif
enddefine;


define lconstant Pr_package(pkg);
    lvars name;
    CHECK_PRINT_READABLY pkg;
    package_name(pkg) -> name;
    Pr_unreadable(
        popstackmark,
        unless name == nil then
            name
        endunless,
        'PACKAGE',
        if name == nil then
            '@', address_of(package)
        endif)
enddefine;


define lconstant Pr_symbol(sym);
    lvars name, pkg, pname, status;
    symbol_string(sym) -> name;
    symbol_package(sym) -> pkg;
    if pkg == nil then
        if print_readably /== nil
        or (print_escape /== nil and print_gensym /== nil)
        then
            cucharout(`#`);
            cucharout(`:`)
        endif
    elseif print_escape /== nil
    or print_readably /== nil
    then
        if pkg == keyword_package then
            cucharout(`:`)
        else
            unless pkg == package
            or (find_symbol(name, package) ->) == sym do
                if (package_name(pkg) ->> pname) == nil then
                    Pr_package(pkg)
                else
                    pr_sym_name(pname)
                endif;
                cucharout(`:`);
                find_symbol(name, pkg) -> (, status);
                if status == @:INTERNAL then
                    cucharout(`:`)
                endif
            endunless
        endif
    endif;
    pr_sym_name(name)
enddefine;


procedure(sym);
    dlocal print_case, print_escape = true, print_gensym = true;
    @:UPCASE -> print_case;
    cucharout(`@`);
    Pr_symbol(sym);
endprocedure -> class_print(symbol_key);


/* Printing numbers */

define lconstant Pr_float(f);
    lvars expchar, expt;
    dlocal popdprecision = true, pop_pr_places;

    if issimple(f) then
        #_< intof(float_digits(decimal_0) * log(2) / log(10)) >_#,
        (default_float_char /== `S` and `f`)
    else
        #_< intof(float_digits(ddecimal_0) * log(2) / log(10)) >_#,
        (default_float_char == `S` and `d`)
    endif -> (pop_pr_places, expchar);

    if f < 0 then
        negate(f) -> f;
        cucharout(`-`)
    endif;

    0 -> expt;
    unless sys_=(f, 0) do
        if f < 1e-3 or f >= 1e7 then
            intof(log10(f)) -> expt;
            f / (10 ** expt) -> f
        endif
    endunless;

    Popfloatpr(f);
    if expchar then
        cucharout(expchar);
        Decpr(expt)
    elseunless expt == 0 do
        cucharout(`e`);
        Decpr(expt)
    endif
enddefine;


define lconstant Pr_radix();
    cucharout(`#`);
    if pop_pr_radix == 2 then
        `b`
    elseif pop_pr_radix == 8 then
        `o`
    elseif pop_pr_radix == 16 then
        `x`
    else
        Decpr(pop_pr_radix);
        `r`
    endif;
    cucharout()
enddefine;


define lconstant Pr_integer(i);
    if print_radix == nil then
        Popintpr(i)
    elseif pop_pr_radix == 10 then
        Popintpr(i);
        cucharout(`.`)
    else
        Pr_radix();
        Popintpr(i)
    endif
enddefine;


define lconstant Pr_ratio(r);
    if print_radix /== nil then
        Pr_radix()
    endif;
    Popintpr(destratio(r) -> r);
    cucharout(`/`);
    Popintpr(r)
enddefine;


define lconstant Pr_complex(c);
    dlvars c;
    print_logical_block(
        '#C(', ')',
        procedure();
            print_indent(@:CURRENT, 0);
            _lisppr(destcomplex(c) -> c);
            cucharout(`\s`);
            print_newline(@:FILL);
            _lisppr(c)
        endprocedure)
enddefine;


/* Unreadable objects, booleans, etc */

define lconstant Pr_bytespec(spec);
    dlvars spec;
    if read_eval == nil then
        CHECK_PRINT_READABLY spec
    endif;
    print_logical_block(
        '#.(', ')',
        procedure();
            print_indent(@:CURRENT, 0);
            _lisppr(@BYTE);
            cucharout(`\s`);
            print_newline(@:FILL);
            _lisppr(frozval(1, spec));
            cucharout(`\s`);
            print_newline(@:FILL);
            _lisppr(frozval(2, spec))
        endprocedure)
enddefine;


define lconstant Pr_pathname(path);
    lvars s;
    namestring(path) -> s;
    if print_escape == nil
    and print_readably == nil then
        appdata(s, cucharout)
    else
        cucharout(`#`);
        cucharout(if pt_device(path) == @:LOGICAL then `L` else `P` endif);
        _lisppr(s)
    endif
enddefine;


define lconstant Pr_random_state(r);
    dlvars r, seed;
    unless (random_seed(r) ->> seed) do
        apply_random_state(pop_max_int, r) ->;
        random_seed(r) -> seed
    endunless;
    print_logical_block(
        '#S(', ')',
        procedure();
            print_indent(@:CURRENT, 0);
            _lisppr(@RANDOM-STATE);
            cucharout(`\s`);
            print_newline(@:FILL);
            _lisppr(seed)
        endprocedure)
enddefine;


define lconstant Pr_boolean() with_nargs 1;
    if () then
        Pr_symbol(@T)
    elseif print_readably /== nil then
        Pr_symbol(@POP11:FALSE)
    else
        Pr_unreadable(popstackmark, 'FALSE')
    endif
enddefine;


define lconstant Pr_nil() with_nargs 1;
    ->;
    if print_pretty /== nil and depth_printed fi_> 0 then
        cucharout(`(`);
        cucharout(`)`)
    else
        Pr_symbol(@NIL)
    endif
enddefine;


define lconstant Pr_character(char);
    lvars name;
    if print_escape /== nil
    or print_readably /== nil
    then
        cucharout(`#`);
        cucharout(`\\`);
        if (char_name(char) ->> name) /== nil then
            appdata(name, cucharout);
            return
        endif
    endif;
    cucharout(fast_char_code(char))
enddefine;


define lconstant Pr_hash_table(h);
    CHECK_PRINT_READABLY h;
    Pr_unreadable(
        popstackmark,
        'HASHTABLE',
        if issymbol(pdprops(h)) then
            @:TEST,
            pdprops(h)
        endif,
        @:SIZE,
        property_size(h))
enddefine;


define lconstant Pr_procedure(pdr);
    lvars name, fni;
    if isarray(pdr) then
        fast_chain(pdr, Pr_array)
    endif;
    if isproperty(pdr) then
        fast_chain(pdr, Pr_hash_table)
    endif;
    if isbytespecpdr(pdr) then
        fast_chain(pdr, Pr_bytespec)
    endif;
    f_name(pdr) -> name;
    unless issymbol(name) or isstring(name) do
        fast_chain(pdr, pr_pop11_simple)
    endunless;
    CHECK_PRINT_READABLY pdr;
    Pr_unreadable(
        popstackmark,
        if isundef(pdr) then
            'UNDEFINED'
        endif,
        if generic_function_p(pdr) then
            'GENERIC'
        endif,
        'FUNCTION',
        Pr_unreadable_quote, name,
        if print_escape /== nil
        and not(isundef(pdr)) then
            function_info(pdr) -> fni;
            lisp_true(f_min(fni));
            lisp_true(f_max(fni));
            lisp_true(f_results(fni))
        endif)
enddefine;


define lconstant Pr_process(ps);
    lvars pid;
    CHECK_PRINT_READABLY ps;
    Pr_unreadable(
        popstackmark,
        if not(isliveprocess(ps)) then 'DEAD' endif,
        'PROCESS',
        if (process_pid(ps) ->> pid) then pid endif)
enddefine;


define lconstant Pr_readtable(r);
    CHECK_PRINT_READABLY r;
    Pr_unreadable(
        popstackmark, readtable_case_sym(r), 'READTABLE', readtable_id(r))
enddefine;


define lconstant Pr_stream(stream);
    lvars In, Out, name;
    CHECK_PRINT_READABLY stream;

    define lconstant Trim_trailing_nulls(s) -> s;
        if isstring(s) then
            substring(1, skipchar_back(0, fast_vector_length(s), s) or 0, s)
                -> s
        endif
    enddefine;

    stream_source(stream) -> In;
    stream_dest(stream) -> Out;
    if isident(In) then idval(In) -> In endif;
    if isident(Out) then idval(Out) -> Out endif;
    if In == pop_undef then stream_read_p(stream) -> In endif;
    if Out == pop_undef then stream_write_p(stream) -> Out endif;

    Pr_unreadable(
        popstackmark,
        if isdevice(In) and (device_full_name(In) ->> name) then
            if In == Out then
                'FILE-IO-STREAM'
            else
                'FILE-INPUT-STREAM'
            endif;
            pathname(name)
        elseif isdevice(Out) and (device_full_name(Out) ->> name) then
            'FILE-OUTPUT-STREAM', pathname(name)
        elseif stringp(In) then
            'STRING-INPUT-STREAM', Pr_unreadable_quote, Trim_trailing_nulls(In)
        elseif stringp(Out) then
            'STRING-OUTPUT-STREAM', Pr_unreadable_quote, Trim_trailing_nulls(Out)
        elseif issymbol(In) then
            'SYNONYM-STREAM-TO', In
        elseif listp(In) then
            'CONCATENATED-STREAM', In
        elseif listp(Out) then
            'BROADCAST-STREAM', Out
        elseif In and Out then
            /* Note cannot distinguish echo streams */
            'TWO-WAY-STREAM', In, Out
        elseif In then
            'INPUT-STREAM', In
        elseif Out then
            'OUTPUT-STREAM', Out
        else
            'NULL-STREAM'
        endif)
enddefine;


define lconstant Pr_undef(u);
    CHECK_PRINT_READABLY u;
    Pr_unreadable(popstackmark, 'UNDEF', if (undefword(u) ->> u) then u endif)
enddefine;


define lconstant Pr_termin() with_nargs 1;
    cucharout()
enddefine;


/* Set up key/print-procedure table */

define lconstant Pr_pop11_record(item);
    dlvars item;
    dlocal depth_printed, length_printed;
    CHECK_PR_LEVEL;
    print_logical_block(
        '#S(', ')',
        procedure();
            lvars i;
            print_indent(@:CURRENT, 0);
            CHECK_PR_LEN;
            _lisppr(class_name(sys_class_of(item)));
            for i from 1 to length(item) do
                cucharout(`\s`);
                CHECK_PR_LEN;
                print_newline(@:FILL);
                _lisppr(fast_record_access(i, item))
            endfor
        endprocedure)
enddefine;


define lconstant Choose_print_procedure(key, prop);
    if is_vector_key(key) then
        Pr_vector ->> prop(key)
    elseif class_print(key) == sys_syspr then
        if is_record_key(key)
        and (key_of_dataword(class_dataword(key)) == key) then
            Pr_pop11_record
        else
            pr_pop11_simple
        endif
    else
        syspr
    endif
enddefine;


unless isproperty(lisp_class_print) do
    newproperty([], 32, false, "perm") -> lisp_class_print;
    Choose_print_procedure -> property_active(lisp_class_print)
endunless;

Pr_bitvector    -> lisp_class_print(bitvector_key);
Pr_integer      -> lisp_class_print(biginteger_key);
Pr_boolean      -> lisp_class_print(boolean_key);
Pr_character    -> lisp_class_print(character_key);
Pr_complex      -> lisp_class_print(complex_key);
Pr_float        -> lisp_class_print(ddecimal_key);
Pr_float        -> lisp_class_print(decimal_key);
Pr_integer      -> lisp_class_print(integer_key);
Pr_list         -> lisp_class_print(pair_key);
Pr_nil          -> lisp_class_print(nil_key);
Pr_package      -> lisp_class_print(package_key);
Pr_pathname     -> lisp_class_print(pathname_key);
Pr_procedure    -> lisp_class_print(procedure_key);
Pr_process      -> lisp_class_print(process_key);
Pr_random_state -> lisp_class_print(random_state_key);
Pr_ratio        -> lisp_class_print(ratio_key);
Pr_readtable    -> lisp_class_print(readtable_key);
Pr_stream       -> lisp_class_print(stream_key);
Pr_string       -> lisp_class_print(string_key);
Pr_symbol       -> lisp_class_print(symbol_key);
Pr_termin       -> lisp_class_print(termin_key);
Pr_undef        -> lisp_class_print(undef_key);
Pr_vector       -> lisp_class_print(vector_key);
sys_syspr       -> lisp_class_print(word_key);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Dec 16 1994
        pr_sym_name now recognises print_case = nil (as set by the
        ~(...~) format directive).
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun  7 1994
        Pr_symbol now copes with deleted packages.
--- John Williams, Apr 26 1994
        _lisppr now uses PRINT-OBJECT.
--- John Williams, Feb 14 1994
        Pr_array now prints a zero-dimensional array as #0A NIL, not #0ANIL.
--- John Williams, Dec 21 1993
        Now uses is_vector_key and is_record_key.
--- John Williams, Aug 11 1993
        Pr_pathname uses #p syntax. Added writeable where appropriate.
--- John Williams, Jan 16 1991
        Changed -class_spec- to -class_field_spec-
 */
