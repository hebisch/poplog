/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/chars.p
 > Purpose:         Characters
 > Author:          John Williams, Dec 7 1986 (see revisions)
 > Documentation:   CLtL, p233-244
 > Related Files:   C.all/src/lispcore.p
 */

lisp_compile_mode;

section $-lisp;

constant
    char_code_limit   =  datalength(character_table)
    ;


define syntax CHARACTER;
    if isinteger(nextreaditem()) then
        sysPUSHQ(conscharacter(readitem()))
    else
        pop11_comp_expr();
        sysCALLQ(fast_code_char)
    endif
enddefine;


/* Predicates on characters */

define graphic_char_p(char);
    char_code(char) -> char;
    (char fi_>= `\s`) and (char fi_< `\^?`)
enddefine;


define standard_char_p(char);
    ;;; Strictly speaking, should mishap on non-characters
    ;;; But this version is useful as the type predicate for STANDARD-CHAR
    ischaracter(char) and (graphic_char_p(char) or char == CHARACTER `\n`)
enddefine;


define string_char_p() with_nargs 1;
    /* Not needed in Steele 1990 - will remove once type stuff is redone */
    ischaracter()
enddefine;


define digit_char_p(char, radix);
    defaults radix 10;
    check_radix(radix);
    isdigitcode(char_code(char), radix) or nil
enddefine;


define alphanumericp(char);
    char_code(char) -> char;
    isalphacode(char) or isnumbercode(char)
enddefine;


/* Character comparisons */

define compare_chars(N, pred);
    lvars i, y;
    char_code();
    fast_for i from 1 to (N fi_- 1) do
        -> y;
        char_code();
        unless fast_apply(dup(), y, pred) do
            erasenum(N - i);
            return(nil)
        endunless;
    endfast_for;
    ->;
    true
enddefine;


define compare_CHARS(N, pred);      /* Caseless version */
    lvars i, y;
    lowertoupper(char_code());
    fast_for i from 1 to (N fi_- 1) do
        -> y;
        lowertoupper(char_code());
        unless fast_apply(dup(), y, pred) do
            erasenum(N - i);
            return(nil)
        endunless;
    endfast_for;
    ->;
    true
enddefine;


define char_/=() with_nargs 2;
    all_different(nonop ==, char_code)
enddefine;


define char_not_equal() with_nargs 2;
    all_different(nonop ==, #_< char_code <> lowertoupper >_#)
enddefine;


/* Character conversions */

define character(item);
        if ischaracter(item) then
                item
    elseif isintegral(item) then
        conscharacter(item)
    else
        get_simple_string(item) -> item;
        if fast_vector_length(item) == 1 then
            lisp_subscrs(1, item)
        else
            lisp_error('One element string needed', [^item])
        endif
    endif
enddefine;


define char_upcase() with_nargs 1;
    CHARACTER lowertoupper(char_code())
enddefine;


define char_downcase() with_nargs 1;
    CHARACTER uppertolower(char_code())
enddefine;


define digit_char(int, radix);
    defaults radix 10;
    check_radix(radix);
    if isinteger(int) and int fi_>= 0 and int fi_< radix then
        CHARACTER (int fi_+ (if int fi_< 10 then 48 else 55 endif))
    else
        nil
    endif
enddefine;


/* Character names */

lconstant Char_name_table
            = initvectorclass(char_code_limit, nil, vector_key);

lblock;
    lvars c;
    for c from 0 to 31 do
        consstring(`^`, c + 64, 2) -> fast_subscrv0(c, Char_name_table)
    endfor
endlblock;

'Backspace' -> fast_subscrv0(`\b`,  Char_name_table);
'Newline'   -> fast_subscrv0(`\n`,  Char_name_table);
'Null'      -> fast_subscrv0(0,     Char_name_table);
'Page'      -> fast_subscrv0(`\^L`, Char_name_table);
'Return'    -> fast_subscrv0(`\r`,  Char_name_table);
'Rubout'    -> fast_subscrv0(`\^?`, Char_name_table);
'Space'     -> fast_subscrv0(`\s`,  Char_name_table);
'Tab'       -> fast_subscrv0(`\t`,  Char_name_table);


define char_name() with_nargs 1;
    fast_subscrv0(char_code(), Char_name_table)
enddefine;


define name_char(name);
    lowertoupper(get_simple_string(name)) -> name;
    if sys_=(name, 'NEWLINE') or sys_=(name, 'LINEFEED') then
        CHARACTER `\n`
    elseif sys_=(name, 'SPACE') then
        CHARACTER `\s`
    elseif sys_=(name, 'TAB') then
        CHARACTER `\t`
    elseif sys_=(name, 'PAGE') then
        CHARACTER `\^L`
    elseif sys_=(name, 'RUBOUT') then
        CHARACTER `\^?`
    elseif sys_=(name, 'RETURN') then
        CHARACTER `\r`
    elseif sys_=(name, 'BACKSPACE') then
        CHARACTER `\b`
    elseif sys_=(name, 'NULL') then
        CHARACTER 0
    elseif fast_vector_length(name) == 2
    and fast_subscrs(1, name) == `^`
    and (fast_subscrs(2, name) -> name,
         name fi_>= `@` and name fi_<= `_`)
    then
        CHARACTER (name fi_- 64)
    else
        nil
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 27 1993
        Upgraded to Steele 1990. Tidied up.
 */
