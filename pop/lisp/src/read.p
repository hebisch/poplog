/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/read.p
 > Purpose:         Common Lisp READ functions
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:   CLtL, p374-382
 > Related Files:   C.all/lisp/src/readutils.p
 */

lisp_compile_mode;

section $-lisp;


define lconstant Read(standard_input, lex_eof_error, lex_eof, nested, keepwhite);
    dlocal standard_input, lex_eof_error, lex_eof;
    defaults lex_eof_error true;

    if pop_true(nested) then
        nested_read(lispreaditem)
    else
        top_level_read(lispreaditem);
        if keepwhite and is_whitespace(lexchar) then
            lexchar -> read_standard_input()
        endif
    endif
enddefine;


define read() with_nargs 4;
    Read(false)
enddefine;


define read_keeping_white() with_nargs 4;
    Read(true)
enddefine;


define lconstant Skip_whitespace() -> char;
    while is_whitespace(cucharin() ->> char) do endwhile
enddefine;


define lconstant Readlistto(endchar);
    dlocal lex_eof_error = true;
    lvars char;

    char_code(endchar) -> endchar;
    [% repeat
        Skip_whitespace() -> char;
        quitif(char == endchar);
        char -> cucharin();
        lispreaditem()
    endrepeat %]
enddefine;


define read_delimited_list(endchar, standard_input, nested);
    dlocal standard_input;

    if pop_true(nested) then
        nested_read(endchar, Readlistto)
    else
        top_level_read(endchar, Readlistto)
    endif
enddefine;


define read_line(standard_input, lex_eof_error, lex_eof, nested);
    dlocal standard_input, lex_eof_error, lex_eof;
    defaults lex_eof_error true;
    lvars char;

    SET_CUCHARIN;
    cucharin() -> char;
    if char == termin then
        eof()
    else
        consstring
            (#| until char == `\n` or char == termin do
                    char;
                    cucharin() -> char
                enduntil
            |#)
    endif;
    lisp_true(char == termin)
enddefine;


define read_char(standard_input, lex_eof_error, lex_eof, nested);
    dlocal standard_input, lex_eof_error, lex_eof;
    defaults lex_eof_error true;

    read_standard_input();
    if dup() == termin then
        ->;
        eof()
    else
        fast_code_char()
    endif
enddefine;


define unread_char(char, standard_input);
    dlocal standard_input;

    if char == termin then
        char
    else
        char_code(char)
    endif -> read_standard_input();
    nil
enddefine;


define peek_char(peek_mode, standard_input, lex_eof_error, lex_eof, nested);
    dlocal peek_mode, standard_input, lex_eof_error, lex_eof;
    defaults lex_eof_error true;
    lvars char;

    SET_CUCHARIN;
    if peek_mode == nil then
        cucharin() -> char
    elseif peek_mode == true then
        Skip_whitespace() -> char
    else
        char_code(peek_mode) -> peek_mode;
        until (cucharin() ->> char) == peek_mode or char == termin do enduntil
    endif;
    char -> cucharin();
    if char == termin then
        eof()
    else
        CHARACTER char
    endif
enddefine;


define read_char_no_hang(stream, eof_error_p, eof_value, recursive_p);
    if listen(stream) then
        read_char(stream, eof_error_p, eof_value, recursive_p)
    else
        nil
    endif
enddefine;


define parse_integer(string, start, finish, radix, junk_ok);
    lvars char, neg = false, num, digit;
    dlocal standard_input;

    check_radix(radix);
    make_stringin_stream(string, start, finish) -> standard_input;
    SET_CUCHARIN;

    Skip_whitespace() -> char;
    if char == `+` or (char == `-` ->> neg) then
        cucharin() -> char
    endif;

    if (isdigitcode(char, radix) ->> num) then
        while (isdigitcode(cucharin() ->> char, radix) ->> digit) do
            num * radix + digit -> num
        endwhile;
        if neg then
            negate(num) -> num
        endif
    else
        if junk_ok == nil then
            mishap(string, 1, 'No integer found in ~S')
        else
            nil -> num
        endif
    endif;
    char -> cucharin();

    if junk_ok == nil then
        unless (Skip_whitespace() ->> char) == termin do
            mishap(CHARACTER char, string, 2, 'Junk character ~S found in ~S')
        endunless
    endif;

    num;
    get_stringin_index(standard_input)
enddefine;


define read_byte(standard_input, lex_eof_error, lex_eof);
    dlocal standard_input, lex_eof_error, lex_eof;
    defaults lex_eof_error true;

    read_standard_input();
    if dup() == termin then
        ->; eof()
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 30 1995
        Uses is_whitespace instead of lexget/lextype (for modularity).
--- John Williams, Aug 25 1995
        peek_char now sets global variable peek_mode (see lisp/src/streams.p).
        Moved SET_CUCHARIN statement in parse_integer to correct position.
        Added missing dlocal for standard_input in read_byte.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Jul 12 1993
        No longer uses cons_with.
--- John Williams, Apr 13 1992
        Compilation of PWM code conditional on #_IF DEF POP_HAS_PWM
--- John Williams, Nov 21 1990
        Fixed bug in -peek_char- (when peek_mode is T and stream is at eof)
--- John Williams, Aug 20 1990
        All procedures that take -lex_eof_error- as an optional
        argument default it to -true-.
 */
