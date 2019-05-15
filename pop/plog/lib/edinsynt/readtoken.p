/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/edinsynt/readtoken.p
 > Purpose:         Prolog: Edinburgh syntax tokeniser.
 > Author:          Simon Nichols, Mar 14 1990 (see revisions)
 > Related Files:   C.all/plog/lib/edinsynt/tokens.p
 >                  C.all/plog/lib/edinsynt/readterm.p
 */

section $-prolog;

;;; The following "character-type" procedures both classify characters and
;;; define actions for processing tokens which start with a character of each
;;; type.

constant procedure (
    Ctrl, Esc, Format, Sym, Digit, Letter, Dot, Slash, Scolon, Squote,
    Aquote, Lpar, Rpar, comma, Lbra, Rbra, Lbrace, Rbrace, Termin
);

;;; prolog_lextable:
;;;     maps each character to an appropriate classification procedure.

constant
    prolog_lextable,
;


lvars
    prolog_lexchar,     ;;; the current character
    prolog_lextype,     ;;; its type (as a procedure)
    prolog_lexstream,   ;;; the current input stream
    prolog_tokenbuffer, ;;; one element token buffer
;


/* Getting characters from the input stream and putting them back */

;;; prolog_lexget:
;;;     get the next character from the input stream.

define lconstant prolog_lexget();
    lvars char, lexstream = prolog_lexstream;
    if isprocedure(lexstream) then
        lexstream();
    elseif ispair(lexstream) then
        fast_destpair(lexstream) -> prolog_lexstream;
    else
        termin;
    endif -> char;
    if char == termin then
        Termin -> prolog_lextype;
        termin ->> prolog_lexstream -> prolog_lexchar;
    elseif char fi_> 127 then
        char -> prolog_lexchar;
        Ctrl -> prolog_lextype;
    else
        char -> prolog_lexchar;
        fast_subscrv(char fi_+ 1, prolog_lextable) -> prolog_lextype;
        if char == `\n` then 0 -> poplastchar endif;
    endif;
enddefine;


;;; prolog_lexput:
;;;     puts the current character back on the input stream.

define lconstant prolog_lexput();
    conspair(prolog_lexchar, prolog_lexstream) -> prolog_lexstream;
enddefine;


;;; prolog_lexput2:
;;;     puts the current character and the character -c- back on the input
;;;     stream.

define lconstant prolog_lexput2(c);
    lvars c;
    conspair(c, conspair(prolog_lexchar, prolog_lexstream))
        -> prolog_lexstream;
enddefine;


;;; prolog_lexput3:
;;;     puts the current character and the characters -c1- and -c2- back on
;;;     the input stream.

define lconstant prolog_lexput3(c1, c2);
    lvars c1, c2;
    conspair(c1, conspair(c2, conspair(prolog_lexchar, prolog_lexstream)))
        -> prolog_lexstream;
enddefine;


/* Error printing */

define lexical_error(message, culprit);
    lvars message, culprit;
    printf(message, ';;; Lexical error: %p');
    if culprit then
        pr(': ');
        prolog_write(culprit);
    endif;
    nl(1);
enddefine;


/* The main tokeniser procedures */

;;; readtoken:
;;;     returns the next token from the input stream.
;;;     Its updater puts a token back.

define lconstant readtoken();
    dlocal prolog_lexchar, prolog_lextype;
    if prolog_tokenbuffer then
        prolog_tokenbuffer, false -> prolog_tokenbuffer;
    else
        prolog_lexget();
        prolog_lextype();
    endif;
enddefine;
;;;
define updaterof readtoken() with_nargs 1;
    -> prolog_tokenbuffer;
enddefine;


;;; Read_cmdname:
;;;     reads a command name, which must be an atom (word).

define lconstant Read_cmdname() -> token;
    lvars token = readtoken();
    unless isword(token) then
        token -> readtoken();
        false -> token;
    endunless;
enddefine;
;;;
define updaterof Read_cmdname =
    updater(readtoken)(%%);
enddefine;


;;; Read_cmdarg:
;;;     reads a command argument, i.e. characters up to the end of line.

define lconstant Read_cmdarg() -> string;
    lvars string;
    cons_with consstring {%
        repeat
            prolog_lexget();
            if prolog_lexchar == termin or prolog_lexchar == `\n` then
                quitloop;
            elseif prolog_lextype /== Format then
                prolog_lexchar;
            endif;
        endrepeat;
    %} -> string;
    prolog_lexput();
enddefine;


;;; readtokens:
;;;     read a sequence of tokens up to a clause terminator.
;;;     This is the interface to the tokeniser

define readtokens() -> tokens;
    lvars token, tokens;
    dlocal
        prolog_lexstream    = cucharin,
        prolog_tokenbuffer  = false,
        prolog_read_cmdname = Read_cmdname,
        prolog_read_cmdarg  = Read_cmdarg,
        prolog_term_terminator,
    ;
    [] -> readenv;
    returnif(prolog_read_command() ->> tokens);
    if (readtoken() ->> token) == termin then
        [end_of_file] -> tokens;
    elseif token == prolog_term_terminator then
        [] -> tokens;
    else
        ;;; read tokens up to a clause terminator
        lvars eof = false;
        [%  token;
            until (readtoken() ->> token) == prolog_term_terminator do
                if token == termin then
                    true -> eof;
                    quitloop;
                endif;
                token;
            enduntil
        %] -> tokens;
        if eof then
            lexical_error('incomplete term before end of file', false);
            [end_of_file] -> tokens;
        endif;
    endif;
enddefine;


/* The major lexical classes -- numbers, atoms, strings */

lvars after_atom;

;;; read_digits:
;;;     reads a sequence of digits, returning the non-negative integer they
;;;     denote. It expects -prolog_lexchar- to be primed with the first digit.

define lconstant read_digits() -> n;
    lvars n = 0;
    repeat
        n * 10 + (prolog_lexchar fi_- `0`) -> n;
        prolog_lexget();
    quitunless (prolog_lextype == Digit);
    endrepeat;
enddefine;


;;; read_based_number:
;;;     read a number in a given base

define lconstant read_based_number(base) -> n;
    lvars d, n, base;
    if base == 0 then
        ;;; DEC-10 character notation: 0'c
        prolog_lexget();
        prolog_lexchar -> n;
    else
        lvars got_one = false;
            ;;; set <true> if at least one valid digit follows the quote
        0 -> n;
        repeat
            prolog_lexget();
            if prolog_lextype == Digit then
                prolog_lexchar fi_- `0`
            else
                prolog_lexchar fi_- `A` fi_+ 10
            endif -> d;
        quitunless (0 fi_<= d and d fi_< base);
            n * base + d -> n;
            true -> got_one;
        endrepeat;
        if got_one then
            prolog_lexput();
        else
            ;;; put back the sole, invalid digit and the quote
            prolog_lexput2(`'`);
            base -> n;
        endif;
    endif;
enddefine;


;;; read_number:
;;;     reads an integer or real number. The characters forming the number
;;;     are pushed on the stack for use in error messages.

define lconstant read_number();
    lvars num = 0, e = 0, num_type = 1, esgn = 1;
    false -> after_atom;
    read_digits() -> num;
    if prolog_lexchar == `.` then
        ;;; real number?
        prolog_lexget();
        unless prolog_lextype == Digit then
            prolog_lexput2(`.`);
            return(num * 10 ** e);
        endunless;
        1.0 -> num_type;
        ;;; read fractional part of number
        while prolog_lextype == Digit do
            num * 10 + (prolog_lexchar fi_- `0`) -> num;
            e fi_- 1 -> e;
            prolog_lexget();
        endwhile;
    elseif prolog_lexchar == `'` then
        ;;; read a number in base -num-
        return(read_based_number(num));
    endif;

    if prolog_lexchar == `e` then
        ;;; exponent?
        prolog_lexget();
        if prolog_lexchar == `-` then
            prolog_lexget();
            unless prolog_lextype == Digit then
                prolog_lexput3(`e`, `-`);
                return(number_coerce(num * 10 ** e, num_type));
            endunless;
            -1 -> esgn;
        else
            unless prolog_lextype == Digit then
                prolog_lexput2(`e`);
                return(number_coerce(num * 10 ** e, num_type));
            endunless;
        endif;
        ;;; read exponent
        esgn * read_digits() + e -> e;
    endif;

    prolog_lexput();
    number_coerce(num * 10 ** e, num_type);
enddefine;


;;; read_esc_seq:
;;;     reads and interprets a string escape sequence. It returns either
;;;     the character represented by the sequence, or false if the sequence
;;;     consists of formatting characters delimited by backslashes.

define lconstant read_esc_seq();
    lvars charcode, num_digits;

    prolog_lexget();
    if prolog_lexchar == `n` then `\n`
    elseif prolog_lexchar == `r` then `\r`
    elseif prolog_lexchar == `t` then `\t`
    elseif prolog_lexchar == `b` then `\b`
    elseif prolog_lexchar == `s` then `\s`
    elseif prolog_lexchar == `^` then
        ;;; control character.
        prolog_lexget();
        if prolog_lexchar == `?` then
            127;
        elseif prolog_lexchar >= `@` and prolog_lexchar <= `_` then
            prolog_lexchar fi_- `@`;
        else
            prolog_lexput();
            `^`;
        endif;
    elseif prolog_lexchar == `(` then
        ;;; ASCII code
        0 -> charcode;
        prolog_lexget();
        while prolog_lextype == Digit do
            charcode fi_* 10 fi_+ (prolog_lexchar fi_- `0`) -> charcode;
            prolog_lexget();
        endwhile;
        if prolog_lexchar == `)` and charcode fi_<= 127 then
            charcode;
        else
            lexical_error('illegal \\( ) escape sequence', false);
            unless prolog_lexchar == `)` then prolog_lexput() endunless;
            `\s`;
        endif;
    else
        prolog_lexchar;
    endif;
enddefine;


;;; read_quoted_literal:
;;;     reads a sequence of characters up to a closing quote.

define lconstant read_quoted_literal(quote);
    lvars char, quote;
    repeat
        prolog_lexget();
        if prolog_lexchar == quote then
            prolog_lexget();
            if prolog_lexchar == quote then
                quote;
            else
                prolog_lexput();
                quote -> prolog_lexchar;
                quitloop();
            endif;
        elseif prolog_lexchar == `\n` or prolog_lexchar == termin then
            quitloop;
        elseif prolog_lexchar == `\\` and read_esc_seq() ->> char then
            char;
        else
            prolog_lexchar;
        endif;
    endrepeat;
enddefine;


;;; read_string:
;;;     reads a character string.

define lconstant read_string() -> s;
    lvars s;
    [% read_quoted_literal(`"`) %] -> s;
    unless prolog_lexchar == `"` then
        lexical_error('missing closing string quote after',
            consstring(destlist(s)));
    endunless;
    false -> after_atom;
enddefine;


;;; read_quoted_atom:
;;;     reads a quoted atom.

define lconstant read_quoted_atom() -> a;
    lvars a;
    cons_with consword {% read_quoted_literal(`'`) %} -> a;
    unless prolog_lexchar == `'` then
        lexical_error('missing closing atom quote after', a);
    endunless;
    true -> after_atom;
enddefine;


;;; read_alpha:
;;;     reads a sequence of letters (which includes underscore) or digits.
;;;     This could either be an atom or a variable.

define lconstant read_alpha();
    lvars first_char = prolog_lexchar, token;
    cons_with consword {%
        repeat
            prolog_lexchar;
            prolog_lexget();
        quitunless (prolog_lextype == Letter or prolog_lextype == Digit);
        endrepeat;
    %} -> token;
    prolog_lexput();
    if isuppercode(first_char) or first_char == `_` then
        false -> after_atom;
        getvar(token);
    else
        true -> after_atom;
        token;
    endif;
enddefine;


;;; read_symbol:
;;;     reads a symbolic atom.

define lconstant read_symbol(char);
    lvars char;
    true -> after_atom;
    cons_with consword {%
        if char then char endif;
        while prolog_lextype == Sym
        or    prolog_lextype == Dot
        or    prolog_lextype == Slash
        do
            prolog_lexchar;
            prolog_lexget();
        endwhile;
    %};
    prolog_lexput();
enddefine;


/* Comments */

;;; inlc:
;;;     handles in-line comments, include nested ones.

define lconstant inlc();
    lvars comment_nesting_level = 1;
    until comment_nesting_level == 0 do
        prolog_lexget();
        if prolog_lexchar == `*` then
            prolog_lexget();
            if prolog_lexchar == `/` then
                comment_nesting_level fi_- 1 -> comment_nesting_level;
            else
                prolog_lexput();
            endif;
        elseif prolog_lexchar == `/` then
            prolog_lexget();
            if prolog_lexchar == `*` then
                comment_nesting_level fi_+ 1 -> comment_nesting_level;
            else
                prolog_lexput();
            endif;
        elseif prolog_lexchar == termin then
            lexical_error('unterminated "/*" comment', false);
            comment_nesting_level fi_- 1 -> comment_nesting_level;
        endif;
    enduntil;
enddefine;


;;; eolc:
;;;     handles end of line comments.

define lconstant eolc();
    repeat
        prolog_lexget();
    quitif(prolog_lexchar == `\n` or prolog_lexchar == termin);
    endrepeat;
    if prolog_lexchar == `\n` then prolog_lexget() endif;
    chain(prolog_lextype);
enddefine;


/* The "character-type" procedures */

;;; Ctrl:
;;;     return the control character as an atom.

define Ctrl();
    true -> after_atom;
    consword(prolog_lexchar, 1);
enddefine;

;;; Esc:
;;;     print mishap message if not in VED.

define Esc();
    handlepopescape();
    prolog_lexget(); chain(prolog_lextype);
enddefine;

;;; Format:
;;;     skip all formatting characters. Check whether the following character
;;;     is a left parenthesis, returning SPACE_LPAR if it is.

define Format();
    while (prolog_lexget(), prolog_lextype == Format) do
        /* nothing */;
    endwhile;
    if prolog_lexchar == `(` then
        false -> after_atom;
        SPACE_LPAR;
    else
        chain(prolog_lextype);
    endif;
enddefine;

define bang();
    true -> after_atom;
    "!";
enddefine;

;;; Lpar:
;;;     always returns " (" rather than "(", except after an atom.

define Lpar();
    if after_atom then LPAR else SPACE_LPAR endif;
enddefine;

;;; Dot:
;;;     checks whether the dot is an atom or (if it is followed by white
;;;     space) a clause terminator.

define Dot();
    prolog_lexget();
    if prolog_lexchar == termin then
        lexical_error('end of file just after full stop', false);
        false -> after_atom;
        DOT_SPACE;  ;;; end of clause
    elseif prolog_lextype == Format then
        false -> after_atom;
        DOT_SPACE;  ;;; end of clause
    else
        chain(`.`, read_symbol);
    endif;
enddefine;

;;; Slash:
;;;     checks for an opening comment bracket, otherwise calls -read_symbol-.

define Slash();
    prolog_lexget();
    if prolog_lexchar == `*` then
        inlc(); ;;; in-line comment
        prolog_lexget();
        chain(prolog_lextype);
    else
        chain(`/`, read_symbol);
    endif;
enddefine;

;;; Scolon:
;;;     looks for the separator ";" or the comment introducer ";;;".

define Scolon();
    if (prolog_lexget(), prolog_lextype == Scolon) then
        if (prolog_lexget(), prolog_lextype == Scolon) then
            ;;; end-of-line comment
            chain(eolc);
        else
            prolog_lexput2(`;`);
        endif;
    else
        prolog_lexput();
    endif;
    false -> after_atom;
    ";";
enddefine;

constant procedure (
    Digit   = read_number,
    Letter  = read_alpha,
    Sym     = read_symbol(% false %),
    Squote  = read_string,
    Aquote  = read_quoted_atom,
);

;;; The remaining procedures return constant separator values

define lconstant separator(/* s */) with_nargs 1;
    false -> after_atom;
    /* s; */
enddefine;

constant procedure (
    comma   = separator(% COMMA %),
    Rpar    = separator(% RPAR %),
    Lbra    = separator(% LBRA %),
    Rbra    = separator(% RBRA %),
    Lbrace  = separator(% LBRACE %),
    Rbrace  = separator(% RBRACE %),
    bar     = separator(% BAR %),
    Termin  = separator(% termin %),
);


constant prolog_lextable =
{%
;;;         00      01      02      03      04      05      06      07
;;;         CTRL-@  CTRL-a  CTRL-b  CTRL-c  CTRL-d  CTRL-e  CTRL-f  CTRL-g
            Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,

;;;         010     011     012     013     014     015     016     017
;;;         CTRL-h  CTRL-i  CTRL-j  CTRL-k  CTRL-l  CTRL-m  CTRL-n  CTRL-o
            Ctrl,   Format, Format, Ctrl,   Format, Ctrl,   Ctrl,   Ctrl,

;;;         020     021     022     023     024     025     026     027
;;;         CTRL-p  CTRL-q  CTRL-r  CTRL-s  CTRL-t  CTRL-u  CTRL-v  CTRL-w
            Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,   Ctrl,

;;;         030     031     032     033     034     035     036     037
;;;         CTRL-x  CTRL-y  CTRL-z  CTRL-[  CTRL-\  CTRL-]  CTRL-^  CTRL-_
            Ctrl,   Ctrl,   Ctrl,   Esc,    Ctrl,   Ctrl,   Ctrl,   Ctrl,

;;;         040     041     042     043     044     045     046     047
;;;         \s      !       "       #       $       %       &       '
            Format, bang,   Squote, Sym,    Sym,    eolc,   Sym,    Aquote,

;;;         050     051     052     053     054     055     056     057
;;;         (       )       *       +       ,       -       .       /
            Lpar,   Rpar,   Sym,    Sym,    comma,  Sym,    Dot,    Slash,

;;;         060     061     062     063     064     065     066     067
;;;         0       1       2       3       4       5       6       7
            Digit,  Digit,  Digit,  Digit,  Digit,  Digit,  Digit,  Digit,

;;;         070     071     072     073     074     075     076     077
;;;         8       9       :       ;       <       =       >       ?
            Digit,  Digit,  Sym,    Scolon, Sym,    Sym,    Sym,    Sym,

;;;         0100    0101    0102    0103    0104    0105    0106    0107
;;;         @       A       B       C       D       E       F       G
            Sym,    Letter, Letter, Letter, Letter, Letter, Letter, Letter,

;;;         0110    0111    0112    0113    0114    0115    0116    0117
;;;         H       I       J       K       L       M       N       O
            Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter,

;;;         0120    0121    0122    0123    0124    0125    0126    0127
;;;         P       Q       R       S       T       U       V       W
            Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter,

;;;         0130    0131    0132    0133    0134    0135    0136    0137
;;;         X       Y       Z       [       \       ]       ^       _
            Letter, Letter, Letter, Lbra,   Sym,    Rbra,   Sym,    Letter,

;;;         0140    0141    0142    0143    0144    0145    0146    0147
;;;         `       a       b       c       d       e       f       g
            Sym,    Letter, Letter, Letter, Letter, Letter, Letter, Letter,

;;;         0150    0151    0152    0153    0154    0155    0156    0157
;;;         h       i       j       k       l       m       n       o
            Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter,

;;;         0160    0161    0162    0163    0164    0165    0166    0167
;;;         p       q       r       s       t       u       v       w
            Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter,

;;;         0170    0171    0172    0173    0174    0175    0176    0177
;;;         x       y       z       {       |       }       ~       CTRL-?
            Letter, Letter, Letter, Lbrace, bar,    Rbrace, Sym,    Ctrl
%};

endsection;     /* $-prolog */

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Aug 20 1993
        Changed the name of each character-type procedure to have an
        initial upper-case letter. This was initially prompted by the
        confusion of names caused by the introduction of a format procedure
        to do Prolog formatted printing. (Note that the character-type
        procedures cannot be lconstant as this causes an access violation
        when this file is compiled.)
--- Simon Nichols, May 27 1992
        Changed -Read_cmdname- to call -readtoken-. Apart from simplifying
        its definition, this fixes a bug whereby commands which followed a
        comment in a file being (re)consulted were not recognized as such.
        Changed -read_based_number- such that if it doesn't read any digits
        appropriate to the base, it puts back on the input the invalid
        digit and the quote and returns the base as the result. This allows
        the correct treatment of (e.g.) 1'.'2 where '.' is infix.
--- Simon Nichols, May 22 1992
        Changed -readtokens- to check for an incomplete term followed
        immediately by end of file.
--- Robert John Duncan, Jan 24 1991
        Changed -Read_cmdname- to recognise ESC
--- Simon Nichols, Jul 18 1990
        Changes to support new implementation of commands:
        - added -Read_cmdname- and -Read_cmdarg-;
        - changed -readtokens- to dlocal -prolog_read_cmdname- and
        -prolog_read_cmdarg- and call -prolog_read_command-.
--- Simon Nichols, Jul  6 1990
        Removed the global variable -saved_stacklength- (and the assignment
        to it in -readtokens-) as it is no longer used.
--- Simon Nichols, Jun 26 1990
        Changed -dot- to return DOT_SPACE as clause terminator.
        Changed the error handling to take sensible remedial action and
        continue after a lexical error.
 */
