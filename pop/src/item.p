/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/src/item.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------------------- ITEMISATION ---------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE '../lib/include/itemise.ph'
#_INCLUDE '../lib/include/vedscreendefs.ph'

constant
        procedure (sys_grbg_destpair, subscr_stack),
        5 (+:, -:)
    ;

weak constant
        procedure (consdstring, consvedstring, newproperty),
        Sys$-unichar_fractions
    ;

section $-Sys;

constant
        procedure (Stringin, Assemble_dfloat)
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys => popnewline, pop_longstrings,
                 incharitem, isincharitem, item_chartype, item_newtype,
                 nextchar, strnumber;

vars
    popnewline      = false,    ;;; controls production of newlines as items
    pop_longstrings = false,    ;;; allows strings over multiple lines
    ;

lconstant macro (

    ;;; short names for char types
    IT_TMN  = ITM_TERMIN,
    IT_LET  = ITM_ALPHA,
    IT_DIG  = ITM_DIGIT,
    IT_SGN  = ITM_SIGN,
    IT_UND  = ITM_UNDERSCORE,
    IT_SEP  = ITM_SEPARATOR,
    IT_SP   = ITM_WHITESPACE,
    IT_STR  = ITM_STRQUOTE,
    IT_CON  = ITM_CHARQUOTE,
    IT_SCN  = ITM_EOLCOMMENT,
    IT_BC1  = ITM_BRCOMMENT1,
    IT_BC2  = ITM_BRCOMMENT2,
    IT_ABC  = ITM_ABC,
    IT_FRC  = ITM_FRACTION,
    IT_EXT  = ITM_EXTENDING,
    IT_CMB  = ITM_COMBINING,
    IT_ENC  = ITM_ENCLOSING,
    IT_IGN  = ITM_IGNORE,
    IT_NTC  = ITM_NOTCHAR,

    ;;; Compact 4-bit types used 2-per-byte in Unicode tables
    it_NTC  = 0,
    it_LET  = 1,
    it_DIG  = 2,
    it_SGN  = 3,
    it_UND  = 4,
    it_SEP  = 5,
    it_SP   = 6,
    it_FRC  = 7,
    it_EXT  = 8,
    it_CMB  = 9,
    it_ENC  = 10,
    it_IGN  = 11,

    ;;; Frozvals of incharitem repeater
    FROZ_STREAM = 1,        ;;; char stream
    FROZ_7      = 2,        ;;; 7-bit table
    FROZ_8      = 3,        ;;; 8-bit table
    FROZ_16     = 4,        ;;; 16-bit procedure

    CURR_LEX_P = [ lexprocedures!V_WORDS[_int(lextype)] ],

    ERASECHAR1  = [->],
    ERASECHARS  = [erasenum(lexN)],
);


lvars
    lexprocedures,          ;;; table of character type procedures
    lextable_7,             ;;; current char table, 7-bit chars
    lextable_8,             ;;; current char table, 8-bit chars
    procedure lextable_16,  ;;; current char procedure, 16-bit chars
    lexN,                   ;;; current char count (base+combining)
    lextype,                ;;; contains type of current base char
    lexstream,              ;;; input stream in a ref

    ;;; This should default to false, but disabled for now
    reading_string = true,

    procedure lex_error = mishap,   ;;; Redefined by strnumber
    ;


;;; --- DEFAULT TABLES ---------------------------------------------------

    /*  Translation of 4-bit compact types to actual types
    */
lconstant compact_trans = consstring(#|
    IT_NTC, IT_LET, IT_DIG, IT_SGN, IT_UND, IT_SEP, IT_SP, IT_FRC,
    IT_EXT, IT_CMB, IT_ENC, IT_IGN
|#);

    /*  Construct a 128-byte string from 256 compact types
    */
define lconstant cons_it_string() -> s;
    lvars i, hi, lo, s = inits(128);
    for i from 128 by -1 to 1 do
        (), () -> (lo, hi);
        (hi<<4) || lo -> s(i)
    endfor
enddefine;

#_INCLUDE 'item_tables.ph'

    /*  Default lex type for Unicode char _uc in the range 0100 - FFFF
    */
define lconstant unichar_lextype(_uc);
    lvars _t, _uc = _int(_uc), _xc = _uc _sub _16:100, _hi = _shift(_xc, _-8);
    unichar_vec!V_WORDS[_hi] -> _t;
    if issimple(_t) then
        ;;; integer
        _int(_t) -> _t;
        if _shift(_t, _-4) _greq _uc then
            _t _bimask _16:F -> _t
        else
            _:it_NTC -> _t
        endif
    elseif _t then
        ;;; 128-byte string
        _shift(_xc, _-1) _bimask _16:7F -> _hi;
        _t!V_BYTES[_hi] -> _t;
        if _xc _bitst _1 then
            _shift(_t, _-4) -> _t
        else
            _t _bimask _16:F -> _t
        endif
    else
        ;;; false for the last block (FF00 - FFFF). Full- and half-width
        ;;; forms are translated to their equivalent chars
        _uc _sub (_16:FF01 _sub _:`!`) -> _uc;
        if _uc _lt _:`\^?` then     ;;; = DEL
            ;;; -> negated ASCII char
            if _uc == _:`\s` then
                ;;; FF00 not valid
                IT_NTC
        else
                _pint(_negate(_uc))
            endif
                else
            FF_block!V_SHORTS[_uc _sub _:`\^?`] -> _t;
            if _t _lteq _:ITM_LAST then
                ;;; (non-compact) char type
                _pint(_t)
            else
                ;;; char
                _pint(_negate(_t))
            endif
    endif;
        return
    endif;
    _pint(compact_trans!V_BYTES[_t])
enddefine;

lvars procedure default_lextable_16 = unichar_lextype;


;;; --- ITEMISING PROCEDURES -------------------------------------------------

define :inline lconstant LEX_GET(char, type);
    fast_cont(lexstream) -> char;
    if iscompound(char) then
        if char!KEY == procedure_key then
            fast_apply(char)
        elseif char!KEY == pair_key then
            sys_grbg_destpair(char) -> fast_cont(lexstream)
        else
            termin
        endif
    else
        termin
    endif -> char;
    if char == termin then
        char -> org_char;
        IT_TMN -> type
    else
        Checkr_dchar(char) fi_&& 16:FFFF ->> char -> org_char;
GET_TYPE:
        if char fi_<= 16:7F then
            ;;; 7-bit
            if popnewline and char == `\n` then
                IT_SEP -> type
            else
                _pint(lextable_7!V_BYTES[_int(char)]) -> type
            endif
        elseif char fi_<= 16:FF then
            ;;; 8-bit
            _pint(lextable_8!V_BYTES[_int(char) _sub _:16:80]) -> type
        else
            ;;; 16-bit
            if (lextable_16(char) ->> type) fi_< 0 then
                ;;; equivalent to another char
                -type -> char;
                goto GET_TYPE
            endif
        endif;
        if reading_string then org_char -> char endif
    endif
enddefine;

    /*  Get the next base character from the input stream
    */
define lconstant Lex_get_base() -> char;
    lvars char, type, org_char;
    while (LEX_GET(char, type); type == IT_IGN and not(reading_string)) do
    endwhile;
    type -> lextype;
    false -> lexN       ;;; means only base char read
enddefine;

    /*  Get any combining characters from the input stream
    */
define lconstant Lex_get_comb() -> ncomb;
    lvars char, type, ncomb, newtype = lextype, org_char;
    returnif(lexN) (lexN fi_- 1 -> ncomb);
    0 -> ncomb;
    repeat
        LEX_GET(char, type);
        if type == IT_CMB then
            unless newtype == IT_LET or newtype == IT_SEP
            or newtype == IT_SGN or newtype == IT_EXT or newtype == IT_UND
            or newtype fi_> ITM_LAST
            then
                if newtype == IT_DIG or newtype == IT_FRC then
                    IT_EXT
                else
                    IT_SGN
                endif -> newtype
            endunless;
            char;
            ncomb fi_+ 1 -> ncomb
        elseif type == IT_ENC then
            IT_SGN -> newtype;
            char;
            ncomb fi_+ 1 -> ncomb
        else
            quitunless(type == IT_IGN and not(reading_string))
        endif
    endrepeat;
    ;;; put back the original char
    conspair(org_char, fast_cont(lexstream)) -> fast_cont(lexstream);
    ncomb fi_+ 1 -> lexN;
    if newtype /== lextype then
        newtype -> lextype;
        false -> ncomb          ;;; false return means type has changed
    endif
enddefine;

define lconstant Lex_get_all();
    lvars type;
    Lex_get_base();
    lextype -> type;
    if dup() == `\n`    ;;; no combining chars allowed on newline
    or type == IT_CMB or type == IT_ENC or type == IT_NTC or type == IT_TMN
    then
        1 -> lexN
    else
        Lex_get_comb() -> ;
    endif
enddefine;

    /*  Put the current character back on the input stream
    */
define lconstant Lex_put(putspace);
    lvars putspace;
    if lextype /== IT_SP or putspace or dup() == `\n` then
        fast_repeat lexN or 1 times
            conspair((), fast_cont(lexstream)) -> fast_cont(lexstream)
        endrepeat
    else
        ERASECHAR1
    endif
enddefine;

    /*  Set char to be the current character with type type
    */
define :inline lconstant LEX_RESTORE(char, type);
    char, 1 -> lexN, type -> lextype
enddefine;

define lconstant Item_consword(num);
    lvars num;
    if num == 1 then
        ;;; consword is specially optimised for 1-char words
        chain((), 1, consword)
    else
        chain(consstring((), num), true, Cons_word)
    endif
enddefine;


;;; --- TERMIN ------------------------------------------------------------

define lconstant It_tmn();
    ;;; termin on stack
enddefine;


;;; --- SPACE -------------------------------------------------------------

define lconstant It_sp();
    repeat
        ERASECHAR1;
        Lex_get_all();
        quitunless(lextype == IT_SP)
    endrepeat;
    fast_chain(CURR_LEX_P)
enddefine;

;;; --- SEPARATOR ---------------------------------------------------------

define lconstant It_sep();
    chain((), lexN, Item_consword)
enddefine;


;;; --- DIGIT -------------------------------------------------------------

lconstant numsyntax_idstring = 'incharitem-num:syntax';

    /*  Test character is valid digit for radix and convert
    */
define lconstant Is_digit(char, radix);
    lvars radix, char, org_char, _z;
    char;           ;;; leave char on stack
    returnif(char == termin) (false);
    if char fi_<= `9` then
        char fi_- `0` -> char;
        return(0 fi_<= char and char fi_< radix and char)
    elseif char fi_< `A` then
        return(false)
    else
        char fi_- `7` -> char;          ;;; reduces `A` to 10, etc
        returnif(char fi_< radix) (char)
    endif;
    ;;; check for additional Unicode decimal digits
    dup() -> org_char;
    zero_digits@V_WORDS -> _z;
    until (org_char fi_- (_z!(w)++ -> _z) ->> char) fi_>= 0 do enduntil;
    char fi_< radix and char fi_<= 9 and char
enddefine;

define lconstant Is_fraction(char);
    lvars char;
    char;
    testdef ratio_key
    and list_assoc_val(char, RTWEAK unichar_fractions)
enddefine;

    /*  Read a basic number
    */
define lconstant Lex_read_basic_num(radix) -> number;
    lvars number, radix, type, _digit;
    unless Is_digit(radix) ->> number then
        if (Is_fraction() ->> number) then
            ;;; fraction
            ERASECHAR1;
            Lex_get_base();
            return
        else
            mishap(0, 'CAN\'T ITEMISE FRACTION LITERAL (ratios not loaded)')
        endif
    endunless;

    ;;; integer
    repeat
        ERASECHAR1;
        Lex_get_base();
        quitunless((Is_digit(radix) ->> _digit) and Lex_get_comb() == 0);
        number * radix + _digit -> number
    endrepeat;

    if dup() == `.` and Lex_get_comb() == 0 then
        ;;; could be floating point
        ERASECHAR1;
        lextype -> type;
        Lex_get_base();
        unless (Is_digit(radix) ->> _digit) and Lex_get_comb() == 0 then
            Lex_put(false);
            LEX_RESTORE(`.`, type)
        ;;; Valid digit -- must be a float.
        elseif testdef decimal_key then
            ;;; Put all the frac digits on
            ;;; the stack for Assemble_dfloat
            ERASECHAR1;
            false;              ;;; marks end of digits on stack
            repeat
                _digit;
                Lex_get_base();
                quitunless((Is_digit(radix) ->> _digit) and Lex_get_comb() == 0);
                ERASECHAR1
            endrepeat;
            Lex_put(false);
            FLWEAK Assemble_dfloat(number, radix) -> number;
            Lex_get_base()
        else
            mishap(0, 'CAN\'T ITEMISE FLOAT LITERAL (floating-point not loaded)')
        endunless
    elseif (Is_fraction() ->> _digit) and Lex_get_comb() == 0 then
        ;;; integer followed by fraction
        ERASECHAR1;
        number + _digit -> number;
        Lex_get_base()
    endif
enddefine;      /* Lex_read_basic_num */

define lconstant Lex_read_exp_num(_radix) -> number;
    lvars number, eval, type1, type2, _radix, _char, _negexp;
    dlocal FLWEAK popdprecision;

    define lconstant **_int(base, _power) -> result;
        lvars result = 1, base, _power = _int(_power);
        repeat
            if _power _bitst _1 then result * base -> result endif;
            quitif(_zero(_shift(_power, _-1) ->> _power));
            base * base -> base
        endrepeat
    enddefine;

    Lex_read_basic_num(_radix) -> number;
    () ->> _char;
    returnunless( (_char == `e` or _char == `d` or _char == `s`)
                    and Lex_get_comb() == 0);

    ;;; try for exponent
    ERASECHAR1;
    lextype -> type1;
    false -> _negexp;
    Lex_get_base();
    if Is_digit(10) and Lex_get_comb() == 0 then
        Lex_read_basic_num(10)
    elseif ((dup() ->> eval) == `-` or eval == `+`) and Lex_get_comb() == 0
    then
        ERASECHAR1;
        lextype -> type2;
        Lex_get_base();
        if Is_digit(10) and Lex_get_comb() == 0 then
            if eval == `-` then true -> _negexp endif;
            Lex_read_basic_num(10)
        else
            Lex_put(true);
            LEX_RESTORE(eval, type2);
            false
        endif
    else
        false
    endif -> eval;
    if eval then
        ;;; got exponent
        unless isinteger(eval) then
            lex_error(eval, 1, 'NON-INTEGER EXPONENT IN NUMBER',
                                    numsyntax_idstring)
        endunless;
        **_int(_radix, eval) -> eval;
        _char /== `s` -> FLWEAK popdprecision;
        number, eval, if _negexp then () / () else () * () endif -> number
    else
        Lex_put(true);
        LEX_RESTORE(_char, type1)
    endif
enddefine;      /* Lex_read_exp_num */

define lconstant Lex_read_number(_prec_minus) -> number;
    lvars   number, type1, type2, tmp, _char, _radix = 10,
            _prec_minus, _minus = false, _had_radix = false;
    dlocal  FLWEAK popdprecision = true;
    lconstant numsyntax = 'INVALID LEXICAL NUMBER SYNTAX';

    define lconstant Read_real(_radix) -> number;
        lvars   number = Lex_read_exp_num(_radix), denom, type1, type2,
                _radix;
        returnunless(isintegral(number)
                        and dup() == `_` and Lex_get_comb() == 0);
        ;;; try for ratio
        ERASECHAR1;
        lextype -> type1;
        Lex_get_base();
        if dup() == `/` and Lex_get_comb() == 0 then
            ERASECHAR1;
            lextype -> type2;
            Lex_get_base();
            if Is_digit(_radix) and Lex_get_comb() == 0 then
                ;;; ratio
                unless testdef ratio_key then
                    mishap(0, 'CAN\'T ITEMISE RATIO LITERAL (ratios not loaded)')
                elseunless isintegral(Lex_read_exp_num(_radix) ->> denom)
                then
                    lex_error(0, numsyntax, numsyntax_idstring)
                else
                    return(number / denom -> number)
                endunless
            endif;
            Lex_put(true);
            LEX_RESTORE(`/`, type2)
        endif;
        Lex_put(true);
        LEX_RESTORE(`_`, type1)
    enddefine;      /* Read_real */

    Read_real(_radix) -> number;
    if dup() == `:`
    and isinteger(number) and 2 fi_<= number and number fi_<= 36
    and Lex_get_comb() == 0
    then
        ;;; was radix
        ERASECHAR1;
        lextype -> type1;
        Lex_get_base();
        if dup() == `-` and Lex_get_comb() == 0 then
            ERASECHAR1;
            lextype -> type2;
            true -> _minus;
            Lex_get_base()
        endif;
        if Is_digit(number) then
            ;;; number was radix
            true -> _had_radix;
            number -> _radix;
            Read_real(_radix) -> number;
            if _minus then -number -> number endif
        else
            ;;; wasn't a radix
            if _minus then
                Lex_put(true);
                LEX_RESTORE(`-`, type2)
            endif;
            false -> _minus;
            Lex_put(true);
            LEX_RESTORE(`:`, type1)
        endif
    endif;
    if _prec_minus then
        if _minus then
            lex_error(0, numsyntax, numsyntax_idstring)
        endif;
        -number -> number;
        _had_radix -> _prec_minus
    endif;
    if dup() == `_` and Lex_get_comb() == 0 then
        stacklength() fi_- 1 -> tmp;
        Lex_get_base();
        () -> _char;
        if (_char == `+` or _char == `-`) and Lex_get_comb() == 0
        and dup(Lex_get_base()) == `:` and Lex_get_comb() == 0
        and (Is_digit(Lex_get_base(), _radix) or Is_fraction())
        and Lex_get_comb() == 0
        and not(_prec_minus)
        then
            ;;; complex number
            if testdef complex_key then
                () -> (, , tmp);
                Read_real(tmp, _radix) -> tmp;
                if _char == `+` then
                    nonop CXWEAK +: (number, tmp)
                else
                    nonop CXWEAK -: (number, tmp)
                endif -> number
            else
                mishap(0, 'CAN\'T ITEMISE COMPLEX LITERAL (complex numbers not loaded)')
            endif
        else
            erasenum(stacklength() fi_- tmp);
            lex_error(0, numsyntax, numsyntax_idstring)
        endif
    endif;
    Lex_put(false)
enddefine;      /* Lex_read_number */

    /*  Digit character type
    */
define lconstant It_dig =
    Lex_read_number(%false%)
enddefine;

    /*  Fraction character type
    */
define lconstant It_frc =
    Lex_read_number(%false%)
enddefine;


;;; --- \ IN CHARACTER CONSTANTS AND IN STRINGS --------------------------

lconstant bsseq_idstring = 'incharitem-bsseq:syntax';

    /*  Read a VED graphics char spec following \G (or a special space
        following \S).

        The graphics chars are 15 line-drawing characters and a few others,
        encoded in the range 16:81 - 16:90.

        The bottom 4 bits of the line drawing set are encoded thus:

            bit 0    right horizontal halfline (left end)
            bit 1    left horizontal halfline  (right end)
            bit 2    top vertical halfline     (bottom end)
            bit 3    bottom vertical halfline  (top end)

        all the others are then made by or'ing these together. (Since
        terminals don't support them, the actual halfline chars
        le, re, te and be will display as full lines.)
    */

define lconstant read_special_spec(leadchar);
    lvars i, c1, c2 = false, seq, parsevec, leadchar;

    lconstant

        ;;; graphics chars
        Gparsevec = {
            `-`     16:83       ;;; full horizontal line
            `|`     16:8C       ;;; full vertical line
            `+`     16:8F       ;;; cross
            `o`     16:90       ;;; degree sign
            `#`     16:91       ;;; diamond
            `.`     16:92       ;;; centred dot
            'be'    16:84       ;;; bottom end
            'bl'    16:85       ;;; bottom left corner
            'br'    16:86       ;;; bottom right corner
            'bt'    16:87       ;;; bottom tee
            'le'    16:81       ;;; left end
            'lt'    16:8D       ;;; left tee
            're'    16:82       ;;; right end
            'rt'    16:8E       ;;; right tee
            'te'    16:88       ;;; top end
            'tl'    16:89       ;;; top left corner
            'tr'    16:8A       ;;; top right corner
            'tt'    16:8B       ;;; top tee
        },

        ;;; special spaces
        Sparsevec = {
            `h`     16:9A       ;;; Ved hair space
            `f`     16:9C       ;;; Format-control space
            `s`     16:9D       ;;; Ved nobreak space
            `t`     16:9E       ;;; trailing space
            `p`     16:9F       ;;; prompt-marker space (vvedpromptchar)
            `n`     16:A0       ;;; ISO Latin nobreak space
        },

        ;;; special newline
        Nparsevec = {
            `t`     16:9B       ;;; trailing newline
        };

    if leadchar == `G` then Gparsevec
    elseif leadchar == `S` then Sparsevec
    else Nparsevec
    endif -> parsevec;

    Lex_get_all() ->> c1;
    if lexN == 1 then
        fast_for i from 1 by 2 to datalength(parsevec) do
            fast_subscrv(i, parsevec) -> seq;
            if seq /== c1 then
                nextif(isinteger(seq) or fast_subscrs(1,seq) /== c1);
                unless c2 then
                    Lex_get_all() ->> c2;
                    lexN fi_+ 1 -> lexN;
                    quitunless(lexN == 2)
                endunless;
                nextif(fast_subscrs(2,seq) /== c2)
            endif;
            ERASECHARS;
            1 -> lexN;
            return(fast_subscrv(i fi_+ 1, parsevec))
        endfor
    endif;

    lex_error(consstring(lexN), 1,
                'UNKNOWN CHARACTERS FOLLOWING \\G OR \\S IN STRING, ETC',
                bsseq_idstring)
enddefine;


lconstant procedure read_string;

define lconstant get_attrs(closer, curr_attr) -> curr_attr;
    lvars c, bit, curr_attr, closer, op = false, embstring = false;
    lconstant
        attr_ms = 'INVALID ATTRIBUTE CHAR AFTER "\\[" IN STRING, ETC',
        attr_idstring = 'incharitem-attr:syntax';

    repeat
        Lex_get_all() -> c;
        if lexN /== 1 then
            lex_error(consstring(c,lexN), 1, attr_ms, attr_idstring)
        endif;
        quitif(c == closer);
        if lextype == IT_STR then
            ;;; embedded string on character
            if closer == `}` then
                lex_error(0, 'EMBEDDED STRING NOT ALLOWED IN "\\{...}" IN STRING',
                                attr_idstring)
            endif;
            read_string(IT_STR) -> embstring
        elseif c == `+` then
            nonop fi_|| -> op
        elseif c == `-` then
            nonop fi_&&~~ -> op
        else
            if c == `b` then
                VEDCMODE_BOLD
            elseif c == `u` then
                VEDCMODE_UNDERLINE
            elseif c == `a` or c == `i` then
                VEDCMODE_ALTFONT
            elseif c == `f` then
                VEDCMODE_BLINK
            elseif c == `A` then
                VEDCMODE_ACTIVE
            elseif c /== termin and `0` fi_<= c and c fi_<= `7` then
                ;;; colour number
                curr_attr fi_&&~~ VEDCMODE_COLOURNUM -> curr_attr;
                if op == nonop fi_&&~~ then
                    0
                else
                    (c fi_- `0`) fi_<< VEDCMODE_COLOUR_SHIFT
                endif
            else
                if c == termin then
                    lex_error(0, 'MISSING CLOSING "]" AFTER "\\[" IN STRING, ETC',
                                        attr_idstring)
                else
                    lex_error(consstring(c,1), 1, attr_ms, attr_idstring)
                endif
            endif -> bit;
            unless op then 0 -> curr_attr, nonop fi_|| -> op endunless;
            fast_apply(curr_attr, bit, op) -> curr_attr
        endif
    endrepeat;
    unless op or embstring then 0 -> curr_attr endunless;
    if embstring then
        conspair(curr_attr, embstring) -> curr_attr
    endif
enddefine;

define lconstant Do_slash(nextchar_attr, curr_attrs);
    lvars char, nextchar_attr, curr_attrs, attrs;
    dlocal reading_string = false;
    ERASECHAR1;
    Lex_get_all() -> char;
    if lexN /== 1 then
        char
    elseif char == `n` then `\n`
    elseif char == `b` then `\b`
    elseif char == `t` then `\t`
    elseif char == `s` then `\s`
    elseif char == `r` then `\r`
    elseif char == `e` then `\^[`
    elseif char == `^` then
        ;;; control char
        Lex_get_all() -> char;
        if lexN /== 1 then
            Lex_put(char, true);
            1 -> lexN;
            `^`
        elseif `@` fi_<= char and char fi_<= `_` then
            char fi_- `@`   ;;; - 64
        elseif `a` fi_<= char and char fi_<= `z` then
            char fi_- ```   ;;; - 96
        elseif char == `?` then
            `\^?`
        else
            Lex_put(char, true);
            `^`
        endif

    elseif char == `(` then
        ;;; numeric char value
        Lex_get_all();
        if lextype == IT_DIG and isinteger(It_dig() ->> char)
        and 0 fi_<= char and char fi_<= 16:FFFF
        and Lex_get_all() == `)` and lexN == 1
        then
            char
        else
            ERASECHARS;
            lex_error(0, 'INCORRECT \\( ) LITERAL IN STRING, ETC',
                                    bsseq_idstring)
        endif

    elseif char == `G` or char == `S` or char == `N` then
        read_special_spec(char)

    elseif curr_attrs           ;;; inside string or char constant
    and (char == `[` or char == `{`) then
        if nextchar_attr then
            lex_error(0, 'UNEXPECTED \\[ OR \\{ AFTER \\[...] IN STRING, ETC',
                                bsseq_idstring)
        elseif char == `[` then
            ;;; attributes affecting next char only
            get_attrs(`]`, curr_attrs) -> attrs;
            return(false, attrs, curr_attrs)
        else
            ;;; attributes affecting rest of string
            get_attrs(`}`, curr_attrs) -> attrs;
            return(false, false, attrs)
        endif
    else
        char
    endif;

    if curr_attrs then
        (true, nextchar_attr, curr_attrs)   ;;; true means char read
    endif
enddefine;


;;; --- UNDERSCORE -------------------------------------------------------

lconstant procedure (Do_bc1, Do_alpha, Do_sign);

define lconstant Do_underscore(charcount);
    lvars charcount, type;
    charcount fi_+ lexN -> charcount;   ;;; count the underscore
    Lex_get_base();
    lextype -> type;
    if (type == IT_LET or type == IT_DIG or type == IT_EXT or type == IT_FRC)
    and (Lex_get_comb() or lextype == IT_EXT) then
        chain(charcount, Do_alpha)
    elseif lextype == IT_BC1 and Lex_get_comb() and Do_bc1() then
        chain(charcount, Item_consword)
    elseif lextype == IT_UND and Lex_get_comb() then
        chain(charcount, Do_underscore)
    elseif lextype == IT_ABC and Lex_get_comb() then
        Do_slash(false, false);
        chain(charcount, Do_alpha)
    elseif (lextype->>type) == IT_SGN or type == IT_BC2 then
        Lex_get_comb() -> ;
        chain(charcount, Do_sign)
    else
        Lex_put(false);
        chain(charcount, Item_consword)
    endif
enddefine;

define lconstant It_und();
    chain(0, Do_underscore)
enddefine;


;;; --- ALPHANUMERIC -----------------------------------------------------

define lconstant Do_alpha(charcount);
    lvars charcount, type;
    repeat
        charcount fi_+ lexN -> charcount;   ;;; count the total chars
        Lex_get_base();
        lextype -> type;
        if (type == IT_LET or type == IT_DIG or type == IT_EXT
            or type == IT_FRC)
        and (Lex_get_comb() or lextype == IT_EXT) then
            ;;; continue
        elseif lextype == IT_UND and Lex_get_comb() then
            chain(charcount, Do_underscore)
        elseif lextype == IT_ABC and Lex_get_comb() then
            Do_slash(false, false)
        else
            quitloop
        endif
    endrepeat;
    Lex_put(false);
    chain(charcount, Item_consword)
enddefine;

define lconstant It_let();
    chain(0, Do_alpha)
enddefine;

    /*  Alphabeticiser
    */
define lconstant It_abc();
    Do_slash(false, false);
    chain(0, Do_alpha)
enddefine;


;;; --- ALPHANUMERIC EXTENDER -----------------------------------------------

define lconstant Do_ext(charcount);
    lvars charcount, type;
    repeat
        charcount fi_+ lexN -> charcount;   ;;; count the total chars
        Lex_get_base();
        lextype -> type;
        if (type == IT_EXT or type == IT_DIG or type == IT_FRC)
        and (Lex_get_comb() or lextype == IT_EXT) then
            ;;; continue
        elseif lextype == IT_UND and Lex_get_comb() then
            chain(charcount, Do_underscore)
        else
            quitloop
        endif
    endrepeat;
    Lex_put(false);
    chain(charcount, Item_consword)
enddefine;

define lconstant It_ext();
    chain(0, Do_ext)
enddefine;


;;; --- SIGN ----------------------------------------------------------

define lconstant Do_sign(charcount);
    lvars charcount, type;
    repeat
        charcount fi_+ lexN -> charcount;   ;;; count the total chars
        Lex_get_base();
        if lextype == IT_BC1 and Lex_get_comb() and Do_bc1() then
            chain(charcount, Item_consword)
        elseif lextype == IT_UND and Lex_get_comb() then
            chain(charcount, Do_underscore)
        elseif (lextype->>type) == IT_SGN or type == IT_BC2 then
            Lex_get_comb() ->
        else
            quitloop
        endif
    endrepeat;
    Lex_put(false);
    chain(charcount, Item_consword)
enddefine;

define lconstant It_sgn();
    if dup() == `-` and lexN == 1 then
        ERASECHAR1;
        Lex_get_base();
        if (lextype == IT_DIG or lextype == IT_FRC) and Lex_get_comb() then
            chain(true, Lex_read_number)
        endif;
        Lex_put(true);
        LEX_RESTORE(`-`, IT_SGN)
    endif;
    chain(0, Do_sign)
enddefine;


;;; --- BRACKETED COMMENT / SIGN --------------------------------------

define lconstant Do_bc1();
    lvars type, bc2_last = false;
    Lex_get_base();
    if lextype == IT_BC2 and Lex_get_comb() then
        ;;; start of comment
        -> (,);
        repeat
            Lex_get_base();
            lextype -> type;
            if type == IT_BC1 then
                if Lex_get_comb() then
                    if bc2_last then
                        ;;; found end of comment
                        ERASECHAR1;
                        return(true)
                    else
                        nextif(Do_bc1())
                    endif
                endif;
                ERASECHARS
            elseif type == IT_BC2 then
                Lex_get_comb() -> bc2_last;
                ERASECHARS;
                nextloop
            elseif type == IT_TMN then
                ERASECHAR1;
                lex_error(0, 'UNTERMINATED "/*" COMMENT',
                                        'incharitem-utcomm:syntax')
            else
                ERASECHAR1
            endif;
            false -> bc2_last
        endrepeat
    else
        ;;; not start of comment
        Lex_put(true);
        LEX_RESTORE((), IT_SGN);
        false
    endif
enddefine;

define lconstant It_bc1();
    if Do_bc1() then
        Lex_get_all();
        fast_chain(CURR_LEX_P)
    else
        chain(It_sgn)
    endif
enddefine;

define lconstant It_bc2();
    chain(It_sgn)
enddefine;


;;; --- END-OF-LINE COMMENT / SEMICOLON ----------------------------------

define lconstant It_scn();

    define lconstant skip_to_eol();
        lvars c;
        until (Lex_get_base() ->> c) == `\n` or c == termin do enduntil;
        c;
        Lex_get_comb() -> ;
        fast_chain(CURR_LEX_P)
    enddefine;

    if () == `;` then
        Lex_get_base();
        if dup() == `;` and Lex_get_comb() == 0 then
            ERASECHAR1;
            Lex_get_base();
            if dup() == `;` and Lex_get_comb() == 0 then
                ERASECHAR1;
                chain(skip_to_eol)
            endif;
            Lex_put(true);
            LEX_RESTORE(`;`, IT_SEP)
        endif;
        Lex_put(false);
        ";"
    else
        chain(skip_to_eol)
    endif
enddefine;


;;; --- CHARACTER CONSTANT/STRING OPENER ------------------------------------

    ;;; closer_type is the closing type
define lconstant read_strcon(closer_type);
    lvars   n, charcount = 0, c, curr_attr = 0, nextchar_attr = false,
            has_attr = false, closer_type, char_read = false, tmp;
    dlocal reading_string = true;

    until (Lex_get_all(), lextype == closer_type) do
        true -> char_read;

        if lexN == 1 then
            () ->> c;
            if c == `\\` then
                ;;; last result is true if it reads a character
                nextunless(Do_slash(nextchar_attr, curr_attr)
                                            -> (nextchar_attr, curr_attr))
            elseunless c /== termin and (c /== `\n` or pop_longstrings) then
                -> ;
                consstring(charcount) -> charcount;
                lex_error(charcount, 1, 'uts: UNTERMINATED STRING OR CHARACTER CONSTANT',
                                    'incharitem-uts:syntax')
            endif
        endif;

        ;;; leave character(s) on stack
        fast_for n from lexN by -1 to 1 do
            subscr_stack(n) -> c;
            unless nextchar_attr then
                c fi_|| curr_attr
            elseif iscompound(nextchar_attr) then
                ;;; pair for embedded string
                0 -> has_attr;
                fast_front(nextchar_attr) -> tmp;
                c fi_|| tmp -> fast_front(nextchar_attr);
                nextchar_attr;
                tmp -> nextchar_attr
            else
                c fi_|| nextchar_attr
            endunless -> subscr_stack(n)
        endfor;

        if closer_type == IT_CON then
            unless lexN == 1 then
                lex_error(0, 'COMBINING CHARS INVALID IN CHARACTER CONSTANT',
                                    'incharitem-charcon:syntax')
            endunless;
            Lex_get_all();
            if lextype == IT_CON then -> else Lex_put(false) endif;
            return
        endif;

        if has_attr /== 0 and dup() fi_&&~~ 16:FFFF /== 0 then
            true -> has_attr
        endif;
        charcount fi_+ lexN -> charcount;
        false -> nextchar_attr
    enduntil;

    () -> c;
    returnunless(closer_type == IT_CON) (charcount, has_attr);

    ;;; character constant without a character
    unless char_read then
        ;;; only closer read
        c;
        Lex_get_all();
        if lextype == IT_CON then -> else Lex_put(false) endif
    elseif nextchar_attr then
        nextchar_attr
    else
        curr_attr
    endunless
enddefine;

define lconstant read_string(/*closer_type*/);
    lvars (charcount, has_attr) = read_strcon(), id;
    unless has_attr then
        consstring(charcount)
    elseif has_attr == true then
        ;;; need a dstring for attributes
        if testdef dstring_key then
            weakref[dstring_key] consdstring(charcount)
        else
            mishap(0, 'CAN\'T ITEMISE QUOTED STRING WITH ATTRIBUTES (dstrings not loaded)')
        endif
    else
        ;;; need a vedstring for embedded data
        if testdef consvedstring then
            weakref consvedstring(charcount)
        else
            mishap(0, 'CAN\'T ITEMISE QUOTED STRING WITH EMBEDDED DATA (vedstrings not loaded)')
        endif
    endunless
enddefine;

define lconstant It_con();
    -> ;
    chain(IT_CON, read_strcon)
enddefine;

define lconstant It_str();
    -> ;
    chain(IT_STR, read_string)
enddefine;

;;; --- MISCELLANEOUS ------------------------------------------------------

define lconstant It_cmb();
    lex_error((), 1, 'ISOLATED COMBINING CHARACTER IN ITEMISER INPUT',
                                    'incharitem-combchar:syntax')
enddefine;

    /*  Should never be called
    */
define lconstant It_ign();
    mishap((), 1, 'SYSTEM ERROR IN ITEMISER (ignoreable character)')
enddefine;

define lconstant It_ntc();
    lex_error((), 1, 'ILLEGAL CHARACTER CODE IN ITEMISER INPUT',
                                    'incharitem-notchar:syntax')
enddefine;


;;; --- TABLES ------------------------------------------------------------

    ;;; table of character types -> procedures
lvars lexprocedures = initv(ITM_LAST+1);
It_tmn -> subscrv(IT_TMN+1, lexprocedures);
It_let -> subscrv(IT_LET+1, lexprocedures);
It_dig -> subscrv(IT_DIG+1, lexprocedures);
It_sgn -> subscrv(IT_SGN+1, lexprocedures);
It_und -> subscrv(IT_UND+1, lexprocedures);
It_sep -> subscrv(IT_SEP+1, lexprocedures);
It_sp  -> subscrv(IT_SP+1,  lexprocedures);
It_str -> subscrv(IT_STR+1, lexprocedures);
It_con -> subscrv(IT_CON+1, lexprocedures);
It_scn -> subscrv(IT_SCN+1, lexprocedures);
It_bc1 -> subscrv(IT_BC1+1, lexprocedures);
It_bc2 -> subscrv(IT_BC2+1, lexprocedures);
It_abc -> subscrv(IT_ABC+1, lexprocedures);
It_frc -> subscrv(IT_FRC+1, lexprocedures);
It_ext -> subscrv(IT_EXT+1, lexprocedures);
It_cmb -> subscrv(IT_CMB+1, lexprocedures);
It_cmb -> subscrv(IT_ENC+1, lexprocedures);
It_ign -> subscrv(IT_IGN+1, lexprocedures);
It_ntc -> subscrv(IT_NTC+1, lexprocedures);


;;; --- INCHARITEM -----------------------------------------------------------

    /*  Return the next item from the input stream
        (n.b. props are for Popc identification)
    */
define Incharitem(lexstream, lextable_7, lextable_8, lextable_16)
                                            with_props '(Sys$-Incharitem)';
    dlocal lexstream, lextable_7, lextable_8, lextable_16, lexN, lextype;
    unless lextable_7 then iso_lextable_7 -> lextable_7 endunless;
    unless lextable_8 then iso_lextable_8 -> lextable_8 endunless;
    unless lextable_16 then default_lextable_16 -> lextable_16 endunless;
    Lex_get_all();
    fast_apply(CURR_LEX_P)
enddefine;

    /*  Produce an item repeater -- set it up initially with char table false
        so that iso_lextable_7 will be used. item_chartype can replace
        this with a modified copy of iso_lextable_7.
    */
define incharitem(charrep);
    lvars charrep;
    Check_procedure(charrep);
    Incharitem(% consref(charrep), false, false, false %)
enddefine;

define isincharitem(p) -> p;
    lvars p, want_charrep = false;
    if isboolean(p) then ((), p) -> (p, want_charrep) endif;
    if testdef readitem and p == weakref readitem
    or testdef itemread and p == weakref itemread then
        weakref proglist -> p;
        while ispair(p) do fast_back(p) -> p endwhile
    endif;
    unless isprocedure(p) and pdpart(p) == Incharitem then
        false -> p
    elseif want_charrep then
        fast_cont(fast_frozval(FROZ_STREAM,p)) -> p;
        while ispair(p) do fast_back(p) -> p endwhile
    endunless
enddefine;


;;; --- CHARACTER TYPES ------------------------------------------------------

define lconstant Checkr_item_pdr(pdr) -> itemrep;
    lvars pdr, itemrep;
    unless isincharitem(pdr) ->> itemrep then
        mishap(pdr, 1, 'ITEM REPEATER PROCEDURE NEEDED',
                        'incharitem-nextchar:type-incharitem_rep')
    endunless
enddefine;

define lconstant Checkr_chartype_args(char) -> (char, table, pdr);
    lvars char, pdr = false, table = false;
    if isprocedure(char) then
        Checkr_item_pdr(char) -> (char, pdr)
    endif;
    Checkr_dchar(char) fi_&& 16:FFFF -> char;
    if pdr then
        fast_frozval(if char fi_<= 16:7F then FROZ_7
                     elseif char fi_<= 16:FF then FROZ_8
                     else FROZ_16
                     endif, pdr) -> table
    endif;
    unless table then
        if char fi_<= 16:7F then
            iso_lextable_7
        elseif char fi_<= 16:FF then
            iso_lextable_8
        else
            default_lextable_16
        endif -> table
    endunless
enddefine;

define item_chartype() with_nargs 2;
    lvars (_char, table, ) = Checkr_chartype_args();
    if _char fi_<= 16:7F then
        fast_subscrs(_char fi_+ 1, table)
    elseif _char fi_<= 16:FF then
        fast_subscrs(_char fi_- 16:7F, table)
    else
        if dup(fast_apply(_char, table)) fi_< 0 then
            ;;; equivalent to another char
            chain(negate(), item_chartype)
        endif
    endif
enddefine;
;;;
define updaterof item_chartype() with_nargs 3;
    lvars (_newtype, _char, table, pdr) = Checkr_chartype_args();
    Check_integer_range(_newtype, 0, datalength(lexprocedures)-1);
    if fast_lmember(_newtype, [^IT_TMN ^IT_DIG ^IT_FRC ^IT_CMB ^IT_ENC]) then
        mishap(_newtype, 1, 'CHARTYPE NOT ASSIGNABLE TO ARBITRARY CHARACTERS')
    endif;

    if _char fi_<= 16:7F then
        if pdr then
            if table == iso_lextable_7 then
                writeable copy(table) -> table
            endif;
            table -> fast_frozval(FROZ_7, pdr)
        endif;
        _newtype -> fast_subscrs(_char fi_+ 1, table)
    elseif _char fi_<= 16:FF then
        if pdr then
            if table == iso_lextable_8 then
                writeable copy(table) -> table
            endif;
            table -> fast_frozval(FROZ_8, pdr)
        endif;
        _newtype -> fast_subscrs(_char fi_- 16:7F, table)
    else
        ;;; 16-bit
        if table == unichar_lextype then
            ;;; turn it into a property
            unless testdef newproperty then
                mishap(0, 'CAN\'T ASSIGN 16-BIT CHAR TYPE (newproperty not loaded)')
            endunless;
            weakref newproperty([], 16, false, "perm") ->> table
                                                    -> default_lextable_16;
            ;;; so this gets called on the character only
            unichar_lextype -> fast_frozval(1,table)!PT_ACTIVE
        endif;
        if pdr then
            if table == default_lextable_16 then
                copy(table) -> table
            endif;
            table -> fast_frozval(FROZ_16, pdr)
        endif;
        _newtype -> fast_apply(_char, table)
    endif
enddefine;

define item_newtype() -> newtype;
    lvars newtype;

    define lconstant It_new(type);
        lvars type, charcount = 0;
        repeat
            charcount fi_+ lexN -> charcount;   ;;; count the total chars
            Lex_get_base();
            quitunless(lextype == type and Lex_get_comb())
        endrepeat;
        Lex_put(false);
        chain(charcount, Item_consword)
    enddefine;

    destvector(lexprocedures) -> newtype;
    consvector(It_new(%newtype%), newtype fi_+ 1) -> lexprocedures
enddefine;


;;; --- MANIPULATING THE NEXT CHARACTER --------------------------------------

define nextchar(item_p);
    lvars stream, ref, item_p;
    fast_frozval(FROZ_STREAM, Checkr_item_pdr(item_p)) -> ref;
    fast_cont(ref) -> stream;
    if ispair(stream) then
        fast_destpair(stream) -> fast_cont(ref)
    else
        stream()
    endif
enddefine;
;;;
define updaterof nextchar(char, item_p);
    lvars ref, char, stream, item_p;
    fast_frozval(FROZ_STREAM, Checkr_item_pdr(item_p)) -> ref;
    if isstring(char) then
        fast_cont(ref) -> stream;
        false, deststring(char) -> ;
        while (->> char) do conspair(char, stream) -> stream endwhile;
        stream
    else
        conspair(if char == termin then char else Checkr_dchar(char) endif,
                                            fast_cont(ref))
    endif -> fast_cont(ref)
enddefine;


;;; --- STRNUMBER -----------------------------------------------------------

lconstant
    lexstream_ref   = writeable consref(0),
    stringin_pair   = writeable conspair(0, 0),
    stringin_clos   = Stringin(%stringin_pair%),
    ;

    /*  Given a string, produce the corresponding number
        e.g. strnumber('1034') = 1034 strnumber('-99') = -99
    */
define strnumber(string) -> num;
    lvars   string, num = false, _minus = false;
    dlocal  lextable_7 = iso_lextable_7,
            lextable_8 = iso_lextable_8,
            lextable_16 = default_lextable_16,
            lexstream = lexstream_ref,
            popnewline = false,
            lexN, lextype
        ;

    define dlocal lex_error(n, errms, idstring);
        lvars n, errms, idstring;
        erasenum(n);
        exitfrom(false, strnumber)
    enddefine;

    if isword(string) then string!W_STRING -> string endif;
    Check_string(string);
    stringin_clos -> fast_cont(lexstream);
    string -> fast_front(stringin_pair);
    1 -> fast_back(stringin_pair);

    while (Lex_get_all(), lextype == IT_SP) do -> endwhile;
    if lextype == IT_CON then
        It_con()
    else
        if dup() == `-` and lexN == 1 then
            ERASECHAR1;
            true -> _minus;
            Lex_get_all()
        endif;
        unless lextype == IT_DIG or lextype == IT_FRC then
            ERASECHARS;
            return
        endunless;
        Lex_read_number(_minus)
    endif -> num;
    ;;; check rest is only spaces
    while (Lex_get_all(), lextype == IT_SP) do ERASECHAR1 endwhile;
    if lextype /== IT_TMN then false -> num endif;
    ERASECHARS
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 24 1999
        Added \Sh backslash sequence (Ved hair space)
--- John Gibson, Jun 26 1997
        Made reading_string true by default to disable the translation
        of fullwidth chars.
--- John Gibson, May 16 1997
        Made read_strcon set reading_string true to stop LEX_GET
        translating fullwidth chars (also stops IT_IGN chars being ignored
        in strings).
--- John Gibson, Mar 29 1997
        Rewritten for Unicode.
--- John Gibson, Feb 14 1997
        String16 changes
--- John Gibson, Apr  3 1996
        Added some mishap id-strings
--- John Gibson, Mar 15 1996
        Changed isincharitem so it can take an optional true 2nd arg to
        mean return the underlying character repeater.
--- John Gibson, Nov  6 1995
        Allowed embedded string in character attributes
--- John Gibson, Aug 30 1995
        Added `A` for active bit in character attributes (VEDCMODE_ACTIVE)
--- John Gibson, Jan 19 1995
        Added 'trailing newline' character `\Nt` = 16:9B
--- John Gibson, Feb  7 1994
        Added lex_error and fixed strnumber to redefine it so as to return
        false for errors
--- John Gibson, Apr 23 1993
        Changed item chartypes of all Ved special spaces to space
--- John Gibson, Feb 16 1993
        Added Ved nobreak space \Ss
--- John Gibson, Feb 25 1992
        Rewritten so that char tables are strings containing chartypes
        (rather than vectors containing chartype procedures). Also added
        8-bit ISO Latin 1 support.
--- John Gibson, Feb  6 1992
        Allowed \Gx in char constants and strings to represent standard
        graphics chars
--- John Gibson, Jan 30 1992
        Changed string and character constants to allow specification
        of character attributes with \[...] and \{...}
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Jul  4 1990
        -lexstream- now contains the ref arg -Incharitem- rather than its
        cont (ensuring that the ref is always kept updated).
--- John Gibson, Nov  8 1989
        Allowed -Do_slash- to take \^a to \^z for Ctrl-A to Ctrl-Z
--- John Gibson, Jan 16 1989
        Added string pdprops to -Incharitem- for popc identfiication
 */
