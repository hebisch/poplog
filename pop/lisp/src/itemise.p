/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/itemise.p
 > Purpose:         Common Lisp lexical analysis
 > Author:          John Williams, Nov 28 1985 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/readutils.p
 */

lisp_compile_mode;

section $-lisp;

syssynonym("is_char_mac", "isclosure");

constant
    procedure   (apply_user_char_mac try_non_decimal user_char_mac)
    macro       (AT_TOKEN_END = [lextype == white or pdpart(lextype) == tmac],
                 AT_TOKEN_START = [lexcontinue == 1]),
    ;


define buildfloat();
    dlocal popdprecision;
    if lexpt then
        if lexflags _bitst _LEX_NEG_EXPT then
            negate(lexpt) -> lexpt
        endif;
        lexpt + lexfrac -> lexfrac
    endif;
    unless lexfrac == 0 do
        true -> popdprecision;
        lexnum * (10.0 ** lexfrac) -> lexnum
    endunless;
    ;;; now float number
    if lexfloatchar == `E` then
        default_float_char -> lexfloatchar
    endif;
    lexfloatchar == `D` or lexfloatchar == `L` -> popdprecision;
    lexnum + ddecimal_0
enddefine;


define builditem();
    lvars s, c;
    if lex_redo then
        try_non_decimal();
        unless lexcontinue then return endunless
    endif;
    if lexnum and (lexflags _bitst _LEX_GOT_DIGIT) then
        setstacklength(lexstacklength);
        if lexflags _bitst _LEX_NEG then
            negate(lexnum) -> lexnum
        endif;
        if lexfrac then
            buildfloat()
        elseif lextop then
            lextop / lexnum
        else
            lexnum
        endif
    elseif lexpkg /== nil and lexcase /== invertcase then
        reader_intern(stacklength() fi_- lexstacklength, lexpkg or package)
    else
        consdstring(stacklength() fi_- lexstacklength) -> s;
        if lexcase == invertcase then
            invertstring(s) -> s
        endif;
        if lexpkg == nil then
            ;;; from the #: macro in DMACS.P
            make_symbol(s)
        else
            sysintern(s, lexpkg)
        endif
    endif;
    false -> lexcontinue
enddefine;


define white();
    unless AT_TOKEN_START do
        builditem()
    endunless
enddefine;


define alpha();
    lexcase(lexchar);
    false -> lexnum
enddefine;


define constituent();
    lexchar;
    false -> lexnum
enddefine;


define tmac(pdr);
    if AT_TOKEN_START then
        if user_char_mac(pdr) then
            apply_user_char_mac(standard_input, CHARACTER lexchar, pdr, 2)
        else
            fast_apply(pdr)
        endif;
        false -> lexcontinue
    else
        lexput();
        builditem()
    endif
enddefine;


define ntmac(pdr);
    if AT_TOKEN_START then
        if user_char_mac(pdr) then
            apply_user_char_mac(standard_input, CHARACTER lexchar, pdr, 2)
        else
            fast_apply(pdr)
        endif;
        false -> lexcontinue
    else
        fast_chain(constituent)
    endif
enddefine;


define digit();
    lvars n;
    lexchar;
    if lexnum then
        lexchar fi_- `0` -> n;
        lexflags _biset _LEX_GOT_DIGIT -> lexflags;
        if lexpt then
            (lexpt * 10) + n -> lexpt
        else
            (lexnum * 10) + n -> lexnum;
            if lexfrac then
                lexfrac fi_- 1 -> lexfrac
            endif
        endif
    endif
enddefine;


define numsign();
    lexchar;
    if lexnum
    and (AT_TOKEN_START or (lexflags _bitst _LEX_SIGN_OK)) then
        (if lexchar == `-` then
            lexflags _biset (if lexpt then _LEX_NEG_EXPT else _LEX_NEG endif)
        else
            lexflags _biclear (if lexpt then _LEX_NEG_EXPT else _LEX_NEG endif)
        endif) _biclear #_< _LEX_SIGN_OK _biset _LEX_GOT_DIGIT >_#
            -> lexflags
    else
        false -> lexnum
    endif
enddefine;


define slash();
    lexchar;
    if lexnum
    and (lexflags _bitst _LEX_/_OK) and not(AT_TOKEN_START) then
        lexnum -> lextop;
        0 -> lexnum;
        lexflags _biclear #_< _LEX_/_OK
                                _biset _LEX_DOT_OK
                                _biset _LEX_EXPT_OK
                                _biset _LEX_GOT_DIGIT >_#
            -> lexflags
    else
        false -> lexnum
    endif
enddefine;


define expt();
    lexcase(lexchar);
    lowertoupper(lexchar) -> lexfloatchar;
    if lexnum
    and (lexflags _bitst _LEX_EXPT_OK) and not(AT_TOKEN_START) then
        0 -> lexpt;
        unless lexfrac do
            0 -> lexfrac
        endunless;
        (lexflags _biclear #_< _LEX_EXPT_OK
                                _biset _LEX_/_OK
                                _biset _LEX_DOT_OK
                                _biset _LEX_GOT_DIGIT >_#)
            _biset _LEX_SIGN_OK -> lexflags
    else
        false -> lexnum
    endif
enddefine;


define dot();
    lvars ndots;
    1 -> ndots;
    if AT_TOKEN_START then
        while (lexget(), lexchar == `.`) do
            1 fi_+ ndots -> ndots
        endwhile;
        lexput();                    ;;; the char that wasn't a dot
        if AT_TOKEN_END then
            if ndots == 1 then
                lexdot()             ;;; IDENTFN when not in a list
            endif;
            consstring(repeat ndots times `.` endrepeat, ndots) -> ndots;
            mishap(ndots, 1, 'Token may not consist solely of dots');
        elseunless ndots == 1 do
            false -> lexnum
        endif
    endif;
    repeat ndots times `.` endrepeat;
    false -> lex_redo;
    if lexnum
    and (lexflags _bitst _LEX_DOT_OK) then
        unless (lexget(), AT_TOKEN_END, lexput()) do
            0 -> lexfrac;
            lexflags _biclear #_< _LEX_DOT_OK _biset _LEX_/_OK >_#
                -> lexflags
        endunless
    else
        false -> lexnum
    endif
enddefine;


define single_escape();
    dlocal lex_eof_error;
    lexget();
    if lexchar == termin then
        true -> lex_eof_error;          ;;; force an error
        eof()
    endif;
    lexchar;
    false ->> lexnum -> lex_redo
enddefine;


define multiple_escape();
    lvars endchar;
    dlocal lex_eof_error;
    lexchar -> endchar;
    until (lexget(), lexchar == endchar) do
        if lextype == single_escape then
            lexget()
        endif;
        if lexchar == termin then
            true -> lex_eof_error;          ;;; force an error
            eof()
        endif;
                if read_suppress == nil then
            lexchar
                endif
    enduntil;
    false ->> lex_redo -> lexnum
enddefine;


define skipform();
    ;;; used when *READ-SUPPRESS* is true
    if is_char_mac(lextype) then
        lextype()
    else
        repeat
                   if lextype == multiple_escape then
                       multiple_escape()
                   elseif lextype == single_escape then
                       lexget()
                   endif;
                   lexget();
                   quitif(AT_TOKEN_END);
                endrepeat, lexput();
        nil
    endif
enddefine;

define colon();
    lvars pkg;
    if lexpkg then
        mishap(0, 'Duplicate package-marker')
    elseif AT_TOKEN_START then
        keyword_package -> lexpkg
    else
        consstring(stacklength() fi_- lexstacklength) -> pkg;
        unless (find_package(pkg) ->> lexpkg) do
            mishap(pkg, 1, 'Non-existent package: ~A')
        endunless
    endif;
    lexget();
    if lextype == colon then
        ;;; internal symbol reference
        lexget()
    endif;
    if AT_TOKEN_END then
        mishap(0, 'Token may not end with a package marker')
    endif;
    lexput();
    false ->> lexnum -> lex_redo;
enddefine;


define try_non_decimal();
    lvars procedure OP = nonop +, radix, top, i, char, digit, num;
    check_radix(read_base ->> radix);
    stacklength() fi_- lexstacklength -> top;
    fast_for i from top by -1 to 1 do
        subscr_stack(i) -> char;
        if isdigitcode(char, radix) ->> digit then
            OP(0, digit) -> num;
            fast_for i from i fi_- 1 by -1 to 1 do
                subscr_stack(i) -> char;
                if isdigitcode(char, radix) ->> digit then
                    OP(num * radix, digit) -> num;
                    nextloop
                elseif char == `/` and i /== 1 then
                    num -> top, 0 -> num;
                    fast_for i from i fi_- 1 by -1 to 1 do
                        subscr_stack(i) -> char;
                        if isdigitcode(char, radix) ->> digit then
                            (num * radix) + digit -> num;
                            nextloop
                        endif;
                        goto FAIL
                    endfast_for;
                    top / num -> num;
                    quitloop(2)
                endif;
                goto FAIL
            endfast_for;
            quitloop
        elseif i == top and i /== 1 then
            if char == `-` then
                nonop - -> OP;
                nextloop
            elseif char == `+` then
                nextloop
            endif
        endif;
        goto FAIL
    endfast_for;
    setstacklength(lexstacklength);
    num;
    false -> lexcontinue;
FAIL:
    if isnumbercode(char) then
        false -> lexnum
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 25 1995
        buildfloat now uses default_float_char.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Apr  3 1995
        Changes for readtable case.
 */
