/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lisp/src/util.p
 > Purpose:         General utility procedures
 > Author:          John Williams, June 1 1987 (see revisions)
 */

lisp_compile_mode;

section $-lisp => @;


define erase1_false() with_nargs 1; ->; false enddefine;

define erase1_true() with_nargs 1;  ->; true enddefine;


/* Using fast procedures */

define define_fast_accessors(key);
    lvars pvec, p, n, slow, w;
    unless iskey(key) and is_record_key(key) do
        mishap(key, 1, 'RECORD CLASS KEY NEEDED')
    endunless;
    cons_access(initl(datalength(key)), class_field_spec(key), false, 0)
        -> pvec;
    for p with_index n in_vector pvec do
        class_access(n, key) -> slow;
        pdprops(slow) -> w;
        "fast_" <> w -> w;
        pop11_define_declare(w, false, false, false);
        sysPASSIGN(p, w)
    endfor
enddefine;


define syntax fastprocs;
    lvars item;
    dlocal pop_vm_flags = (pop_vm_flags || VM_NOPROT_LVARS);
    until (readitem() ->> item) == ";" do
        nextif(item == ",");
        sysLCONSTANT(item, "macro");
        if isdefined("fi_" <> item) then
            sysPASSIGN("fi_" <> item, item)
        else
            sysPASSIGN("fast_" <> item, item)
        endif
    enduntil;
    ";" :: proglist -> proglist
enddefine;


define macro SLOW;
    lvars item;
    readitem() -> item;
    word_identifier(item, current_section, true)
enddefine;


/* Repeated procedure application */

define fast_sysrepeat(num, pdr);
    while num fi_> 0 do
        fast_apply(pdr);
        num fi_- 1 -> num
    endwhile
enddefine;


/* Numerical */

constant macro
    decimal_0   =  0.0s0,
    ddecimal_0  =  0.0d0,
    ;


define reciprocal(n);
    1 / n
enddefine;


define check_radix(radix);
    unless isinteger(radix) and radix fi_>= 2 and radix fi_<= 36 do
        mishap(radix, 1, 'Radix must be between 2 and 36 (inclusive)')
    endunless
enddefine;


define isdigitcode(code, radix);
    ;;; assumes 2 <= radix <= 36
    if radix fi_> 10 then
        radix fi_- 10 -> radix;
        if code fi_>= `0` and code fi_<= `9` then
            code fi_- `0`
        elseif code fi_>= `A` and code fi_< (`A` fi_+ radix) then
            code fi_- 55
        elseif code fi_>= `a` and code fi_< (`a` fi_+ radix) then
            code fi_- 87
        else
            false
        endif
    elseif code fi_>= `0` and code fi_< (`0` fi_+ radix) then
        code fi_- `0`
    else
        false
    endif
enddefine;


/* For the compiler */

constant
    4 _biclear  =   nonop fi_&&~~,
    4 _bimask   =   nonop fi_&&,
    4 _biset    =   nonop fi_||,
    6 _bitst    =   nonop &&/=_0,
    ;


define 7 x is_closure_of y;
    pdpart(x) == y
enddefine;


define is_protected_closure() with_nargs 1;
    isclosure() == 1
enddefine;


define is_lexical_closure(p);
    lvars i;
    returnunless (is_protected_closure(p)) (false);
    fast_for i from 1 to datalength(p) do
        returnunless (isident(fast_frozval(i, p))) (false)
    endfast_for;
    true
enddefine;


/* Lists and vectors */

syssynonym("fast_subscrv0", "fast_prolog_arg");

constant 7 ==_nil = class_recognise(nil_key);

define caar =
    car <> car
enddefine;


define cadr =
    cdr <> car
enddefine;


define cdar =
    car <> cdr
enddefine;


define cddr =
    cdr <> cdr
enddefine;


define dotconslist(list) -> list;
    ;;; like nonexported "Conslist"
    lvars item;
    until (->> item) == popstackmark do
        conspair(item, list) -> list
    enduntil
enddefine;


define acons() with_nargs 3;
    ;;; N.B. not the same as Clisp acons
    conspair(conspair())
enddefine;


define pushnew(item, list) -> list;
    ;;; N.B. not the same as Clisp pushnew
    unless fast_lmember(item, list) do
        conspair(item, list) -> list
    endunless
enddefine;


define item_or_front(item) -> item;
    if ispair(item) then
        fast_front(item) -> item
    endif
enddefine;


define 8 list starts_with item;
    ispair(list) and fast_front(list) == item
enddefine;


define islistlength(list, n);
    until n == 0 or atom(list) do
        fast_back(list) -> list;
        n fi_- 1 -> n
    enduntil;
    list == [] and n == 0
enddefine;


define listlength_>=(list, n);
    until n == 0 do
        if atom(list) then return(false) endif;
        fast_back(list) -> list;
        n fi_- 1 -> n
    enduntil;
    true
enddefine;


define ncdelete_if(list, p) -> list;
    lvars l;
    returnif(list == []);
    while apply(front(list), p) do
        back(list) -> list;
        returnif(list == [])
    endwhile;
    list -> l;
    until back(l) == [] do
        if apply(front(back(l)), p) then
            back(back(l)) -> back(l)
        else
            back(l) -> l
        endif
    enduntil
enddefine;


define all_different(first, rest, eq_p, pre_op);
    lvars item;

    /* Used by /=, CHAR/=, and CHAR-NOT-EQUAL */

    pre_op(first) -> first;
    ncmaplist(rest, pre_op) -> rest;
    until rest == [] do
        fast_for item in rest do
            returnif(eq_p(item, first)) (nil)
        endfast_for;
        fast_destpair(rest) -> rest -> first
    enduntil;
    true
enddefine;


define vmember(item, vector);
    lvars i;
    fast_for i from 1 to fast_vector_length(vector) do
        if fast_subscrv(i, vector) == item then
            return(i)
        endif
    endfast_for;
    false
enddefine;


define checkr_vector_index(i, vec);
    unless isinteger(i)
    and i fi_>= 0
    and i fi_< fast_vector_length(vec) do
        mishap(i, vec, 2, 'Invalid array subscript')
    endunless;
    i fi_+ 1
enddefine;


define on_stack(thing);
    lvars n = 1, item;
    repeat
        subscr_stack(n) -> item;
        if item == popstackmark then
            return(false)
        elseif item == thing then
            return(true)
        else
            n fi_+ 1 -> n
        endif
    endrepeat
enddefine;


/* Insertion procedure used when creating ordered list
    of applicable methods */

define insert(item, list, procedure before_p) -> list;
    lvars l, prev;
    if list == [] then
        conspair(item, list) -> list
    else
        list -> l;
        false -> prev;
        repeat
            if l == [] or before_p(item, front(l)) then
                conspair(item, l);
                if prev then
                    -> back(prev)
                else
                    -> list
                endif;
                return
            else
                l -> prev;
                back(l) -> l
            endif
        endrepeat
    endif
enddefine;


/* Iterating over Common Lisp lists (using endp as end test) */

define :for_extension global in_cl_list(varlist, fast);
    lvars start_lab, end_lab, list_var, item_var;
    dlocal pop_new_lvar_list;

    sysNEW_LABEL() -> start_lab;
    sysNEW_LABEL() -> end_lab;
    pop11_loop_start(start_lab);
    pop11_loop_end(end_lab);

    unless islistlength(varlist, 1) do
        mishap('NEED ONE VARIABLE FOR IN_CL_LIST', varlist)
    endunless;
    fast_front(varlist) -> item_var;

    sysNEW_LVAR() -> list_var;
    pop11_comp_expr_to("do") ->;
    sysPOP(list_var);

    sysLABEL(start_lab);
    sysPUSH(list_var);
    sysCALL("ident endp");
    sysIFSO(end_lab);
    sysPUSH(list_var);
    sysCALL("fast_destpair");
    sysPOP(list_var);
    sysPOP(item_var);
    pop11_comp_stmnt_seq_to(popclosebracket) ->;
    sysGOTO(start_lab);
    sysLABEL(end_lab);
enddefine;


/* Iterating over records */

define lconstant Check_record(item);
    unless isrecordclass(item) then
        mishap(item, 1, 'RECORD TYPE OBJECT NEEDED')
    endunless
enddefine;


define :for_extension global in_record() with_nargs 2;
    ;;; !! DOESN'T WORK CORRECTLY ON LISTS !!
    subscr_loop(false, fast_record_access, Check_record)
enddefine;


define :with_index_hook in_record() with_nargs 3;
    subscr_loop(fast_record_access, Check_record)
enddefine;


/* Syntax for defining expandable properties
    used for various tables of system information
*/

define syntax defprop;
    lvars name, prop;
    until nextitem() == ";" do
        readitem() -> name;
        nextif(name == ",");
        nextif(isword(name) and isdeclared(name) and isproperty(valof(name)));
        sysSYNTAX(name, "procedure", true);
        newanyproperty([], 8, 1, 10, false, false, "tmparg", false, false)
            ->> prop -> valof(name);
        name -> pdprops(prop);
   enduntil;
enddefine;


/* Pop-11 code-planting procedure for initializing &optional vars */

define global syntax defaults;
    lvars lab, var;
    until nextitem() == ";" do
        sysNEW_LABEL() -> lab;
        readitem() -> var;
        if var == "," then
            readitem() -> var
        endif;
        sysPUSH(var);
        sysIFSO(lab);
        pop11_comp_expr();
        sysPOP(var);
        sysLABEL(lab)
    enduntil
enddefine;


/* Case conversion etc */

define caseless_=(c1, c2);
    lowertoupper(c1) == lowertoupper(c2)
enddefine;


define all_same_case(string);
    lvars char, upper = false, lower = false;
    fast_for char in_string string do
        if isuppercode(char) then
            returnif(lower) (false);
            `A` -> upper
        elseif islowercode(char) then
            returnif(upper) (false);
            `a` -> lower
        endif
    endfast_for;
    lower or upper
enddefine;


/* Symbol <-> string conversions */

define vars sym_to_filename() with_nargs 1;
    uppertolower(symbol_string())
enddefine;


define string_to_sym(string);
    lvars pstring, pkg, i;
    package ->> pstring -> pkg;
    if (locchar(`:`, 1, string) ->> i) then
        substring(1, i fi_- 1, string) -> pstring;
        if locchar(`:`, i fi_+ 1, string) == (i fi_+ 1) then
            i fi_+ 1 -> i
        endif;
        substring(i fi_+ 1, fast_vector_length(string) fi_- i, string) -> string;
        if pstring = '#' then
            nil
        elseif fast_vector_length(pstring) == 0 then
            keyword_package
        else
            find_package(pstring)
        endif -> pkg
    endif;
    if pkg == nil then
        make_symbol(string)
    elseif ispackage(pkg) then
        sysintern(string, pkg)
    else
        mishap(pstring, 1, 'Non-existent package')
    endif
enddefine;


5 -> item_chartype(`@`);

define global macro @ -> sym;
    lvars c, n, i, pkg;
    0 -> n;
    repeat
        nextchar(readitem) -> c;
        quitif(c == termin);
        item_chartype(c) -> i;
        nextif(i == 6 and n == 0);
        quitif(i fi_> 4 and i fi_< 10);
        c;
        n fi_+ 1 -> n;
    endrepeat;
    c -> nextchar(itemread);
    string_to_sym(consstring(n)) -> sym;
    if ispair(proglist_macro_pair) then
        sym -> hd(proglist_macro_pair)
    endif;
    if lisp_system_building then
        symbol_package(sym) -> pkg;
        if pkg == lisp_package
        or pkg == poplog_package then
            export(sym, pkg)
        endif
    endif
enddefine;


/* Io, reporting errors */

vars debug_io, query_io, raw_io, terminal_io, trace_output,
        procedure str_output;

vars active (standard_input, standard_output, error_output);

constant procedure (lisp_error, lisp_cerror, warn, warn_once, advise);

#_IF not(VED_LOADED)
global vars procedure vedscreenbell = identfn;
#_ENDIF


define lconstant Convert_word(item);
    dlocal package = lisp_package;
    if isword(item) then
        string_to_sym(fast_word_string(item))
    elseif ispair(item) then
        maplist(item, Convert_word)
    else
        item
    endif
enddefine;


define type_error(got, expecting);
    Convert_word(expecting) -> expecting;
    fast_chain(
        @TYPE-ERROR, {^@:EXPECTED-TYPE ^expecting ^@:DATUM ^got},
        lisp_error)
enddefine;


define type_cerror(cstring, got, expecting);
    fast_chain(
        cstring or 'Carry on regardless',
        @TYPE-ERROR,
        {^@:EXPECTED-TYPE ^expecting ^@:DATUM ^got},
        lisp_cerror)
enddefine;


define simple_type_error(message, got, expecting);
    fast_chain(
        @SIMPLE-TYPE-ERROR,
        {^@:EXPECTED-TYPE ^expecting
         ^@:DATUM ^got
         ^@:FORMAT-STRING ^message
         ^@:FORMAT-ARGUMENTS [^got]
        },
        lisp_error)
enddefine;

define check_positive(item) -> item;
    unless isinteger(item) and item fi_>= 0 do
        simple_type_error('Integer >= 0 needed', item, @UNSIGNED-BYTE);
    endunless
enddefine;

define name_error(item, kind);
    fast_chain(
        @POPLOG:BAD-NAME-ERROR,
        {% @:DATUM, item, @:KIND, Convert_word(kind) %},
        lisp_error)
enddefine;


define package_error(message, involving, pkg);
    fast_chain(
        @PACKAGE-ERROR,
        {^@:MESSAGE ^message ^@:INVOLVING ^involving ^@:PACKAGE ^pkg},
        lisp_error)
enddefine;


define redefine_cerror(name, type);
    fast_chain(
        'Go ahead', @POPLOG:REDEFINE-ERROR, {^@:NAME ^name ^@:TYPE ^type},
        lisp_cerror)
enddefine;


define program_error(message, involving);
    fast_chain(
        @PROGRAM-ERROR, {^@:MESSAGE ^message ^@:INVOLVING ^involving},
        lisp_error)
enddefine;


define control_error(message, involving);
    fast_chain(
        @CONTROL-ERROR, {^@:MESSAGE ^message ^@:INVOLVING ^involving},
        lisp_error)
enddefine;

define program_cerror(cstring, message, involving);
    fast_chain(
        cstring or 'Carry on regardless',
        @PROGRAM-ERROR,
        {^@:MESSAGE ^message ^@:INVOLVING ^involving},
        lisp_cerror)
enddefine;


define check_name(item, kind);
    unless issymbol(item) do
        name_error(item, kind)
    endunless
enddefine;


define checkr_name_from_list(list, kind) -> name;
    if ispair(list) then
        check_name(fast_front(list) ->> name, kind)
    else
        program_error('Missing ~(~A~) name', [^kind])
    endif
enddefine;


/* For lisp environment enquiries (Common Lisp Manual p447-8) */

define lisphost(item);
    dlocal pr = sys_syspr;
    if (valof("pophost")(item) ->> item) then
        item sys_>< ''
    else
        ''
    endif
enddefine;


/* For active variables */

define boolean_variable(id);
    lisp_true(idval(id))
enddefine;

define updaterof boolean_variable(value, id);
    pop_true(value) -> idval(id)
enddefine;


define read_only_variable(id, sym);
    idval(id)
enddefine;

define updaterof read_only_variable(value, id, sym);
    lisp_cerror('Pretend it never happened',
                'Cannot assign ~S to ~S',
                [^value ^sym])
enddefine;


define integer_range_variable(id, lo, hi, sym);
    idval(id)
enddefine;

define updaterof integer_range_variable(value, id, lo, hi, sym);
    unless (isinteger(value) and value fi_>= lo and value fi_<= hi) do
        lisp_cerror('Assignment not performed',
                    'Integer >= ~D and <= ~D needed',
                    [% lo, hi, sym, value %]);
        return
    endunless;
    value -> idval(id)
enddefine;


weak global vars $-propsheet_ident_class;

define syntax propsheet_idents;
    lvars word, id;
    lconstant AP = conspair(0, 1);
    until (readitem() ->> word) == ";" do
        nextif(word == ",");
        nextunless(testdef propsheet_ident_class);
        identof(word) -> id;
        unless isactive(id) then
            syscancel(word);
            sysSYNTAX(word, AP, false);     ;;; Assumes it's an untyped var
            sysGLOBAL(word);
            fast_idval(% id %) -> nonactive_valof(word);
            identof(word) -> id
        endunless;
        "active" -> weakref propsheet_ident_class(id)
    enduntil;
    ";" :: proglist -> proglist
enddefine;


/* Symbol <-> word conversions */

define consword_identifier(s);
    lvars root, i, j;
    if isstartstring('$-', s) then
        pop_section, 3
    else
        current_section, 1
    endif -> (root, i);
    while (issubstring('$-', i, s) ->> j) do
        section_subsect(consword(substring(i, j - i, s)), root, false)
            -> root;
        j + 2 -> i
    endwhile;
    if i == 1 then
        consword(s)
    else
        word_identifier(consword(allbutfirst(i - 1, s)), root, "undef")
    endif
enddefine;


define vars sym_to_word(sym);
    symbol_string(sym) -> sym;
    if isuppercode(all_same_case(sym)) then
        uppertolower(sym) -> sym
    endif;
    if issubstring('$-', sym) then
        consword_identifier(sym)
    else
        consword(sym)
    endif
enddefine;




define vars word_to_sym(word);
    lvars pkg;
    if ispackage(word) then
        word -> pkg;
        -> word
    else
        pop11_package -> pkg
    endif;
    fast_word_string(word) -> word;
    if islowercode(all_same_case(word)) then
        lowertoupper(word) -> word
    endif;
    sysintern(word, pkg)
enddefine;


/* Procedure for generating unique string function names */

constant procedure genstring_prop = newmapping([], 32, 0, true);

define genstring(string);
    lvars n;
    dlocal pr = sys_syspr;
    genstring_prop(string) -> n;
    n + 1 -> genstring_prop(string);
    string sys_>< n
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Feb 20 1997
        sym_to_word now creates a word identifier if the symbol name
        includes '$-'.
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  6 1995
        Added is_lexical_closure.
--- John Williams, Jun  1 1995
        Added definition for vedscreenbell if Ved not loaded.
--- John Williams, May 15 1995
        @NAME-ERROR now @BAD-NAME-ERROR (due to name clash with CLX).
--- John Williams, May  5 1995
        Added in_record syntax.
--- John Williams, Apr 12 1995
        Added simple_type_error.
--- John Williams, Apr  3 1995
        all_same_case now permanent identifier.
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Mar 15 1995
        Added procedures for signalling typed errors.
--- John Williams, Feb 27 1995
        Added erase1_false and erase1_true.
--- John Williams, Feb  2 1995
        Added operator is_closure_of.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun  7 1994
        Changes to macro @ for POPLOG package.
--- John Williams, Apr 27 1994
        Added declaration for warn_once.
--- John Williams, Aug 27 1993
        Added fast_sysrepeat, all_different, and genstring.
--- John Williams, Aug 11 1993
        Added in_cl_list for loop syntax. Removed lisp_os_type.
--- John Williams, Jul 12 1993
        defprop no longer uses popdefineconstant.
--- John Williams, Oct  8 1990
        -sym_to_word- and -word_to_sym- now leave mixed-case items alone
--- John Williams, Jun  6 1989
        Changed -newanyproperty- 7th arg from -false- to "tmparg"
 */
