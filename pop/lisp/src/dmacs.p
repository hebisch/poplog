/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/dmacs.p
 > Purpose:         Dispatch macro characters
 > Author:          John Williams, Jan 6 1987 (see revisions)
 > Documentation:   CLtL, p351
 > Related Files:   C.all/lisp/src/umacs.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs repeat, for, destpair, front, back;

constant procedure (eval, sti_maker, structure_info);

vars
    lex_dmac_char     =  false,
    lex_dmac_subchar  =  false,
    lex_dmac_num      =  nil,
    ;


define exec_non_dmac();
    mishap(CHARACTER lex_dmac_char, CHARACTER lex_dmac_subchar, 2,
            'Undefined character after ~C')
enddefine;


define exec_illegal_dmac();
    mishap(CHARACTER lex_dmac_char, CHARACTER lex_dmac_subchar, 2,
            'Illegal character after ~C')
enddefine;


define dmac(dt);
    lvars num, char, pdr;
    dlocal lex_dmac_char lex_dmac_subchar lex_dmac_num lex_eof_error;
    lexchar -> lex_dmac_char;
    false -> lex_dmac_subchar;
    nil -> lex_dmac_num;
    0 -> num;
    while isnumbercode(cucharin() ->> char) do
        num * 10 + (char fi_- `0`) ->> num -> lex_dmac_num
    endwhile;
    unless isinteger(char) do
        true -> lex_eof_error;
        eof()
    endunless;
    lowertoupper(char) -> lex_dmac_subchar;
    fast_subscrv0(lex_dmac_subchar, dt) -> pdr;
    if user_char_mac(pdr) then
        standard_input, CHARACTER lex_dmac_subchar, lex_dmac_num;
        apply_user_char_mac(pdr, 3)
    else
        fast_apply(pdr)
    endif
enddefine;


define lconstant Check_num_not_supplied();
    if (read_suppress == nil) and (lex_dmac_num /== nil) then
        mishap(0, 'Unnecessary integer specifier')
    endif
enddefine;


lconstant macro
    TEST_READ_SUPPRESS =
        [if read_suppress /== nil then
            lispreaditem() ->;
            return(nil)
        endif;];


/* "#" Dispatch macro procedures */

define lconstant #_escape();
    lvars char, sl, name;
    dlocal lex_eof_error;
    unless lex_dmac_num == nil do
        warn('Ignoring old-style font information', [^lex_dmac_num]) ->;
    endunless;
    cucharin() -> char;
    if char == termin then
        true -> lex_eof_error;
        eof();
    endif;
    (#| char;
        until (lexget(), AT_TOKEN_END) do
            lexchar;
        enduntil;
        lexput();
    |#) -> sl;
    if read_suppress /== nil then
        erasenum(sl);
        return(nil)
    endif;
    if sl == 1 then
        conscharacter()
    else
        consstring(sl) -> name;
        name_char(name) -> char;
        if char == nil then
            lisp_error('Unknown character name after #\\', [^name])
        endif;
        char
    endif
enddefine;


define lconstant #_quote();
    Check_num_not_supplied();
    [^@FUNCTION ^(lispreaditem())];
enddefine;


/* Vectors, arrays, and structures */

define lconstant Checkr_vec_size(got, wanted, errstring) -> got;
    if wanted /== nil then
        if got > wanted then
            conslist(got - wanted) -> got;
            mishap('Excess ' <> errstring <> '(s) supplied', got)
        elseif got == 0 and wanted /== 0 then
            mishap(0, 'No initial ' <> errstring <> 's supplied')
        endif;
        repeat wanted - got times dup() endrepeat;
        wanted -> got
    endif
enddefine;


define lconstant #_bra();
    dlvars start, size;
    dlocal lex_eof_error = true;
    lexstacklength -> start;
    lex_dmac_num -> size;

    define dlocal lexket();
        Checkr_vec_size(stacklength() fi_- start, size, 'vector element');
        chainfrom(#_bra, consvector)
    enddefine;

    repeat lispreaditem() endrepeat
enddefine;


define lconstant #_*();
    TEST_READ_SUPPRESS;
    while (lexget(), lexchar == `1` or lexchar == `0`) do
        lexchar fi_- `0`
    endwhile;
    lexput();
    unless AT_TOKEN_END do
        mishap(CHARACTER lexchar, 1, '~S is not a binary digit')
    endunless;
    consbitvector(Checkr_vec_size(stacklength() fi_- lexstacklength,
                                  lex_dmac_num,
                                  'binary digit'))
enddefine;


define lconstant #_a();
    lvars rank, seq, temp, dims;
    TEST_READ_SUPPRESS;
    check_positive(lex_dmac_num) -> rank;
    procedure();
        dlocal lexbqdepth = 0;
        lispreaditem()
    endprocedure() ->> seq -> temp;
    [% repeat rank times
        seq_length(temp);
        unless dup() == 0 do
            elt(temp, 0) -> temp
        endunless
    endrepeat %] -> dims;
    make_array(dims, true, nil, seq, nil, nil, nil, 0)
enddefine;


define lconstant #_s();
    lvars list, name, sti, key;
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    procedure();
        dlocal lexbqdepth = 0;
        lispreaditem()
    endprocedure() -> list;
    unless ispair(list) do
        mishap(list, 1, 'Non-empty list needed after #S')
    endunless;
    destpair(list) -> (name, list);
    check_name(name, "structure");
    if name == @RANDOM-STATE then
        funcall(@MAKE-RANDOM-STATE, list)
    elseif name == @PATHNAME then
        funcall(@PATHNAME, list)
    elseif structure_info(name) ->> sti then
        [% until endp(list) do
            conskeyword(destpair(list) -> list);
            quitif(endp(list));
            destpair(list) -> list
        enduntil %] -> list;
        funcall(sti_maker(sti), list)
    elseif (is_pop11_type(name) ->> key) then
        destlist(list);
        if is_record_key(key) then
            unless (/* length of list */) == datalength(key) do
                mishap(name, list, 2, 'Wrong number of fields for structure')
            endunless;
        endif;
        fast_apply(class_cons(key))
    else
        mishap(name, 1, 'Unrecognised structure type')
    endif
enddefine;


define lconstant #_p();
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    pathname(lispreaditem())
enddefine;


define lconstant #_l();
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    logical_pathname(lispreaditem())
enddefine;


/* Reading numbers in non-decimal radix */

define lconstant Do_#_r(radix);
    lvars neg, num, den, digit, n;
    false ->> neg -> num;
    lexget();
    if lexchar == `-` then
        true -> neg
    elseunless lexchar == `+` do
        lexput()
    endif;
    repeat
        0 -> den;
        false -> digit;
        while (lexget(), isdigitcode(lexchar, radix) ->> n) do
            den * radix + n -> den;
            true -> digit
        endwhile;
        if digit then
            quitif(AT_TOKEN_END);
            if not(num) and lexchar == `/` then
                den -> num;
                nextloop
            endif
        endif;
        mishap(CHARACTER lexchar, radix, 2, '~S is not a radix ~D digit')
    endrepeat;
    lexput();
    if num then
        num / den
    else
        den
    endif;
    if neg then negate() endif
enddefine;


define lconstant #_r();
    TEST_READ_SUPPRESS;
    check_radix(lex_dmac_num);
    Do_#_r(lex_dmac_num)
enddefine;


define lconstant #_b();
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    Do_#_r(2)
enddefine;


define lconstant #_o();
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    Do_#_r(8)
enddefine;


define lconstant #_x();
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    Do_#_r(16)
enddefine;


define lconstant #_c();
    Check_num_not_supplied();
    funcall(@COMPLEX, lispreadform())
enddefine;


define lconstant #_:();
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    if (lexget(), AT_TOKEN_END, lexput()) then
        mishap(0, 'Misplaced package marker')
    endif;
    nil -> lexpkg;
    false -> lexnum;
    exitto(lispreaditem)
enddefine;


/* Circular structures */

defclass rref { rcont };

lvars Derref_done = [];


define derref(form);
    lvars item, key, i;
    dlocal Derref_done;
    lconstant macro
        RETURN_IF_DONE
            =
        [returnif(fast_lmember(form, Derref_done));
         conspair(form, Derref_done) -> Derref_done;
        ];

    if ispair(form) then
        RETURN_IF_DONE;
        if isrref(fast_front(form) ->> item) then
            rcont(item) -> fast_front(form)
        elseif iscompound(item) then
            derref(item)
        endif;
        if isrref(fast_back(form) ->> item) then
            rcont(item) -> fast_back(form)
        elseif iscompound(item) then
            derref(item)
        endif
    elseif (isrecordclass(form) ->> key) then
        RETURN_IF_DONE;
        for item with_index i in_record form do
            if isrref(item) then
                rcont(item) -> fast_record_access(i, form)
            elseif iscompound(item) then
                derref(item)
            endif
        endfor
    else
        if isarray(form) then
            arrayvector(form) -> form
        endif;
        RETURN_IF_DONE;
        if isvector(form) then
            for item with_index i in_vector form do
                if isrref(item) then
                    rcont(item) -> fast_subscrv(i, form)
                elseif iscompound(item) then
                    derref(item)
                endif
            endfor
        endif
    endif
enddefine;


define lconstant Check_rref_num(num);
    unless isinteger(num) do
        if isintegral(num) then
            mishap(num, 1, 'Read-time label too large')
        else
            mishap(0, 'No read-time label specified')
        endif
    endunless
enddefine;


define lconstant #_=() -> form;
    lvars num, ref;
    if read_suppress /== nil then
        chainfrom(lispreaditem, lispreaditem)
    endif;
    lex_dmac_num -> num;
    Check_rref_num(num);
    if isproperty(lexref) then
        if lexref(num) then
            mishap(num, 1, 'Duplicate label #~D=')
        endif
    else
        newproperty([], 8, false, true) -> lexref
    endif;
    consrref(false) ->> ref -> lexref(num);
    lispreaditem() ->> form -> rcont(ref);
    if form == ref then
        mishap(num, 1, 'Label #~D= cannot refer to itself')
    endif
enddefine;


define lconstant #_#();
    lvars num, ref, form;
    if read_suppress /== nil then
        return(nil)
    endif;
    lex_dmac_num -> num;
    Check_rref_num(num);
    unless isrref(lexref(num) ->> ref) do
        mishap(num, 1, 'Reference to undefined label #~D=')
    endunless;
    if (rcont(ref) ->> form) then
        form
    else
        true -> lexrefs;
        ref
    endif
enddefine;


/* Read-time evaluation */

define lconstant #_dot();
    lvars form;
    TEST_READ_SUPPRESS;
    Check_num_not_supplied();
    lispreadform() -> form;
    if read_eval == nil then
        lisp_error('Cannot evaluate: *read-eval* is nil', [^form])
    else
        eval([^@VALUES ^form]);
        str_input(standard_input) -> cucharin
    endif
enddefine;


define lconstant Isfeature(form);
    lvars item;
    if ispair(form) then
        destpair(form) -> form -> item;
        if item == @:AND then
            for item in_cl_list form do
                returnunless(Isfeature(item)) (false)
            endfor;
            true
        elseif item == @:OR then
            for item in_cl_list form do
                returnif(Isfeature(item)) (true)
            endfor;
            false
        elseif item == @:NOT and islistlength(form, 1) then
            not(Isfeature(front(form)))
        else
            mishap(conspair(item, form), 1, 'Invalid "feature" expression')
        endif
    else
        for item in_cl_list features do
            if item == form then
                return(true)
            elseif issymbol(item)
            and symbol_package(item) /== keyword_package
            and conskeyword(item) == form then
                warn('Non keyword symbol in *features* list', [^item]) ->;
                return(true)
            endif
        endfor;
        false
    endif
enddefine;


define lconstant Skipnext();
    dlocal read_suppress = true;
    lispreaditem() ->;
enddefine;


define lconstant Readfeature();
    dlocal package = keyword_package;
    lispreaditem();
enddefine;


define lconstant #_+();
    if read_suppress /== nil then
        Readfeature() ->;
        lispreaditem()          /* Should be nil */
    else
        Check_num_not_supplied();
        unless Isfeature(Readfeature()) do
            Skipnext()
        endunless;
        chainfrom(lispreaditem, lispreaditem)
    endif
enddefine;


define lconstant #_-();
    if read_suppress /== nil then
        Readfeature() ->;
        lispreaditem()          /* Should be nil */
    else
        Check_num_not_supplied();
        if Isfeature(Readfeature()) then
            Skipnext()
        endif;
        chainfrom(lispreaditem, lispreaditem)
    endif
enddefine;


/* Bracketed comments */

define lconstant #_|();
    lvars hash, bar, nesting, lastchar, char;
    Check_num_not_supplied();
    lex_dmac_char -> hash;
    lex_dmac_subchar -> bar;
    1 -> nesting;
    cucharin() -> lastchar;
    until nesting == 0 do
        cucharin() -> char;
        if char == termin then
            consstring(hash, 1) -> hash;
            consstring(bar, 1) -> bar;
            mishap(0, 'Unterminated ' <> hash <> bar
                        <> ' .... ' <> bar <> hash <> ' comment')
        elseif lastchar == bar and char == hash then
            nesting fi_- 1 -> nesting
        elseif lastchar == hash and char == bar then
            nesting fi_+ 1 -> nesting
        endif;
        char -> lastchar
    enduntil;
    chainfrom(lispreaditem, lispreaditem)
enddefine;


/* Install built in "#" read-macros */

make_dmac_char(CHARACTER `#`, true, readtable) ->;


readtable_pdr(CHARACTER `#`, readtable)
    -> readtable_pdr(CHARACTER `#`, lisp_default_readtable);


define lconstant Insert_dmac(c, pdr);
    set_dmac_char(CHARACTER `#`, CHARACTER c, pdr, readtable) ->;
enddefine;


Insert_dmac(`\\`,  #_escape);
Insert_dmac(`'`,   #_quote);
Insert_dmac(`(`,   #_bra);
Insert_dmac(`A`,   #_a);
Insert_dmac(`S`,   #_s);
Insert_dmac(`P`,   #_p);
Insert_dmac(`L`,   #_l);
Insert_dmac(`R`,   #_r);
Insert_dmac(`B`,   #_b);
Insert_dmac(`O`,   #_o);
Insert_dmac(`X`,   #_x);
Insert_dmac(`C`,   #_c);
Insert_dmac(`*`,   #_*);
Insert_dmac(`:`,   #_:);
Insert_dmac(`=`,   #_=);
Insert_dmac(`#`,   #_#);
Insert_dmac(`.`,   #_dot);
Insert_dmac(`+`,   #_+);
Insert_dmac(`-`,   #_-);
Insert_dmac(`|`,   #_|);
Insert_dmac(`<`,   exec_illegal_dmac);
Insert_dmac(`\s`,  exec_illegal_dmac);
Insert_dmac(`\t`,  exec_illegal_dmac);
Insert_dmac(`\n`,  exec_illegal_dmac);
Insert_dmac(`\^L`, exec_illegal_dmac);
Insert_dmac(`\r`,  exec_illegal_dmac);
Insert_dmac(`)`,   exec_illegal_dmac);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, May 16 1995
        Fixed bug in derref (in_record can't cope with lists).
--- John Williams, May  5 1995
        Uses in_record syntax.
--- John Williams, Apr 12 1995
        Added #_l (for reading in logical pathnames).
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Dec 16 1994
        derref now handles structures as well as lists and vectors.
--- John Williams, Oct 18 1994
        Fixed bugs in derref, #= and ##.
--- John Williams, Apr 26 1994
        #+ and #- now work correctly if called when *read-suppress* is
        true.
--- John Williams, Dec 21 1993
        #s now uses is_record_key.
--- John Williams, Aug 11 1993
        *features* now reads in keyword package. #p implemented. #. now
        tests *read-eval*. #, removed. #\ warns if font information
        supplied.
--- John Williams, Jul  9 1993
        Uses defclass instead of recordclass
--- John Williams, Feb 24 1992
        #b, #o, and #x now cope with signs (c.f. BR isl-fr.4411)
--- John Williams, Jan 16 1991
        Changed -class_spec- to -class_field_spec-
 */
