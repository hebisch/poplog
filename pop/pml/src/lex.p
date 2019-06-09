/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/lex.p
 > Purpose:         PML: Lexical analysis
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

lconstant
    lextable = writeable initv(128),
                    ;;; maps initial characters to classification procedures
;

lvars
    lexchar,        ;;; the current character
    lextype,        ;;; its type (a classification procedure)
    lexstream,      ;;; the current input stream
;

vars
    ml_linenum,     ;;; like -poplinenum-, but relates to the last *item*
                    ;;; read rather than the last character
;

lconstant procedure (       ;;; forward
    ctrl,
    eof,
    letter,
    lex_number,
    lex_symbol,
    lex_longid,
);

;;; lex_error:
;;;     trivial, but needed as a redefinition hook for Ved

define vars lex_error(msg, culprits);
    lvars msg, culprits;
    ml_error(msg, culprits, popfilename, ml_linenum);
enddefine;

;;; lexget:
;;;     reads the next character from -lexstream- into -lexchar- and sets
;;;     -lextype- to its character type

define lconstant lexget();
    lvars c;
    if ispair(lexstream) then
        Destpair(lexstream) -> lexstream -> c;
    else
        fast_apply(lexstream) -> c;
    endif;
    if c == termin then
        eof -> lextype;
        closed_instream -> lexstream;
    elseif c fi_> 127 then
        ctrl -> lextype;
    else
        Subscrv(c fi_+ 1, lextable) -> lextype;
        if c == `\n` then 0 -> poplastchar endif;
    endif;
    c -> lexchar;
enddefine;


;;; lexput:
;;;     puts the current character back on the input stream.
;;;     Its updater puts a second given character back too.

define lconstant lexput();
    unless lexchar == termin then
        conspair(lexchar, lexstream) -> lexstream;
    endunless;
enddefine;

define updaterof lconstant lexput(/* c */) with_nargs 1;
    lexput();
    conspair(/* c, */ lexstream) -> lexstream;
enddefine;


;;; itemise:
;;;     returns the next item from the input stream.
;;;     Its updater puts an item back.

define lconstant itemise(buffer, instream);
    lvars   buffer, instream, tmp;
    dlocal  lexchar, lextype, lexstream;
    if Cont(buffer) ->> tmp then
        false -> Cont(buffer);
        tmp;
    else
        fast_back(instream) -> lexstream;
        lexget();
        poplinenum -> ml_linenum;
        lextype();
        lexstream -> fast_back(instream);
    endif;
enddefine;

define updaterof lconstant itemise(/* item, buffer, stream */) with_nargs 3;
    /* stream */ ->, /* item */ -> Cont(/* buffer */);
enddefine;


;;; new_itemiser:
;;;     returns a new copy of the itemiser, based on the given instream.
;;;     The first frozen argument is a one-element buffer.

define new_itemiser(instream);
    lvars instream;
    itemise(% consref(false), instream %);
enddefine;


;;; ml_nextchar:
;;;     reads/puts back the next character from an itemiser's input stream

define ml_nextchar(itemiser);
    lvars itemiser;
    input(frozval(2, itemiser));
enddefine;

define updaterof ml_nextchar(itemiser) with_nargs 2;
    lvars itemiser;
    -> input(frozval(2, itemiser));
enddefine;


/*
 *  Character classification
 */

;;; Simple separators:
lconstant procedure (
    eof     = identfn(% termin %),
    comma   = identfn(% RESERVED ',' %),
    lbra    = identfn(% RESERVED '[' %),
    lbrace  = identfn(% RESERVED '{' %),
    rbra    = identfn(% RESERVED ']' %),
    rbrace  = identfn(% RESERVED '}' %),
    rpar    = identfn(% RESERVED ')' %),
    uscore  = identfn(% RESERVED '_' %),
);

;;; ctrl:
;;;     non-printing character -- error

define lconstant ctrl();
    lex_error('illegal non-printing character in input\n\t%p\n',
        [%consscon("string", consstring(lexchar, 1))%]);
enddefine;

;;; esc:
;;;     special handling for ESC character (for PWM)

define lconstant esc();
    handlepopescape();
    lexget();
    poplinenum -> ml_linenum;
    fast_chain(lextype);
enddefine;

;;; format:
;;;     skips all formatting characters.

define lconstant format();
    while (lexget(), lextype == format) do /* nothing */ endwhile;
    poplinenum -> ml_linenum;
    fast_chain(lextype);
enddefine;

;;; dot:
;;;     reads a sequence of three dots, i.e. a record wildcard.

define lconstant dot();
    unless (lexget(), lexchar == `.`) and (lexget(), lexchar == `.`) then
        lex_error('illegal use of "."\n', []);
    endunless;
    RESERVED '...';
enddefine;

;;; scolon:
;;;     looks for the separator ";" or the comment introducer ";;;" (when
;;;     enabled).

define lconstant scolon();
    if ml_allow_eolc then
        if (lexget(), lexchar == `;`) then
            if (lexget(), lexchar == `;`) then
                ;;; comment out the rest of line
                until (lexget(), lexchar == `\n` or lexchar == termin) do
                    /* nothing */
                enduntil;
                if lexchar == `\n` then lexget() endif;
                poplinenum -> ml_linenum;
                fast_chain(lextype);
            else
                `;` -> lexput();
            endif;
        else
            lexput();
        endif;
    endif;
    RESERVED ';';
enddefine;

;;; lpar:
;;;     looks for the separator "(" or a bracketed comment starting "(*"

define lconstant lpar();
    if (lexget(), lexchar == `*`) then
        lblock;
            lvars comments = [^ml_linenum];
            until comments == [] do
                if (lexget(), lexchar == `*`) then
                    if (lexget(), lexchar == `)`) then
                        Back(comments) -> comments;
                    else
                        lexput();
                    endif;
                elseif lexchar == `(` then
                    if (lexget(), lexchar == `*`) then
                        conspair(ml_linenum, comments) -> comments;
                    else
                        lexput();
                    endif;
                elseif lexchar == termin then
                    ;;; report the line number of the unmatched comment
                    Front(comments) -> ml_linenum;
                    lex_error('unmatched opening comment bracket\n', []);
                endif;
            enduntil;
        endlblock;
        lexget();
        poplinenum -> ml_linenum;
        fast_chain(lextype);
    endif;
    lexput();
    RESERVED '(';
enddefine;

;;; digit:
;;;     starts a positive number

define lconstant digit();
    chain(false, lex_number);
enddefine;

;;; lex_number:
;;;     reads an integer or real number.

define lconstant lex_number(sgn);
    lvars sgn, s, sl = stacklength(), type = "int";
    if sgn then sgn endif;
    lexchar, lexget();
    while lextype == digit do
        lexchar, lexget();
    endwhile;
    if lexchar == `.` then
        "real" -> type;
        `.`, lexget();
        unless lextype == digit then
            consstring(lexchar, stacklength() - sl) -> s;
            lex_error('incorrect real number format\n\t%p\n', [^s]);
        endunless;
        while lextype == digit do
            lexchar, lexget();
        endwhile;
    endif;
    if lexchar == `E` then
        "real" -> type;
        `E`, lexget();
        if lexchar == `~` then
            `~`, lexget();
        endif;
        unless lextype == digit then
            consstring(lexchar, stacklength() - sl) -> s;
            lex_error('incorrect real number format\n\t%p\n', [^s]);
        endunless;
        while lextype == digit do
            lexchar, lexget();
        endwhile;
    endif;
    lexput();
    consstring(stacklength() - sl) -> s;
    consscon(type, s);
enddefine;

;;; lex_escape:
;;;     reads and interprets a string escape sequence (after a "\\").
;;;     Returns the character represented by the sequence, or nothing if
;;;     it's a format sequence.

define lconstant lex_escape();
    lexget();
    if lexchar == `n` then
        `\n`;
    elseif lexchar == `t` then
        `\t`;
    elseif lexchar == `"` then
        `"`;
    elseif lexchar == `\\` then
        `\\`;
    elseif lexchar == `^` then
        ;;; control character
        lexget();
        if lexchar == `?` then
            `\^?`;
        elseif lexchar >= `@` and lexchar <= `_` then
            lexchar fi_- `@`;
        else
            lex_error('illegal control character in string\n\t%p\n',
                [%consscon("string", consstring(`\\`, `^`, lexchar, 3))%]);
        endif;
    elseif lextype == digit then
        ;;; 3-digit decimal ASCII code
        lblock;
            lvars charcode = lexchar fi_- `0`;
            if (lexget(), lextype == digit) then
                10 fi_* charcode fi_+ lexchar fi_- `0` -> charcode;
                if (lexget(), lextype == digit) then
                    10 fi_* charcode fi_+ lexchar fi_- `0` -> charcode;
                endif;
            endif;
            unless lextype == digit and charcode fi_<= 255 then
                lex_error(
                    'illegal numeric character code in string\n\t"\\%p"\n',
                    [^charcode]);
            endunless;
            charcode;
        endlblock;
    elseif lextype == format then
        ;;; skip formatting characters.
        while lextype == format do lexget() endwhile;
        ;;; should be followed by a backslash.
        unless lexchar == `\\` then
            lex_error('unterminated format sequence in string\n', []);
        endunless;
    else
        lex_error('illegal use of "\\" in string\n\t%p\n',
            [%consscon("string", consstring(`\\`, lexchar, 2))%]);
    endif;
enddefine;

;;; dquote:
;;;     double quote starts a character string.

define lconstant dquote() -> s;
    lvars s;
    lexget();
    consscon("string", mlstring(#|
        until lexchar == `"` or lexchar == `\n` or lexchar == termin do
            if lexchar == `\\` then
                lex_escape();
            else
                lexchar;
            endif;
            lexget();
        enduntil;
    |#)) -> s;
    unless lexchar == `"` then
        lex_error('unterminated string\n\t%p\n', [^s]);
    endunless;
enddefine;

;;; squote:
;;;     single quote starts a type variable

define lconstant squote();
    lvars id, equality = false, imperative = false;
    constvid(consword(#|
        lexchar, lexget();
        if lexchar == `'` then
            true -> equality;
            lexchar, lexget();
        endif;
        if lexchar == `_` then
            true -> imperative;
            lexchar, lexget();
        endif;
        while lextype == letter
        or lextype == digit
        or lextype == uscore
        or lextype == squote
        do
            lexchar, lexget();
        endwhile;
        lexput()
    |#), equality, imperative);
enddefine;

;;; letter:
;;;     starts an alphanumeric identifier or reserved word

define lconstant letter() -> item;
    lvars item;
    tryreserved(consword(#|
        lexchar, lexget();
        while lextype == letter
        or lextype == digit
        or lextype == uscore
        or lextype == squote
        do
            lexchar, lexget();
        endwhile
    |#)) -> item;
    if lexchar == `.` and isshortid(item) then
        lex_longid(item) -> item;
    else
        lexput();
    endif;
enddefine;

;;; symbol:
;;;     reads a symbolic identifier or reserved word

define lconstant symbol();
    chain(false, lex_symbol);
enddefine;

;;; tilde:
;;;     may start a negative number or a symbol

define lconstant tilde();
    lexchar, lexget();
    if lextype == digit then
        chain(lex_number);
    else
        chain(lex_symbol);
    endif;
enddefine;

;;; star:
;;;     checks for an unmatched comment closer '*)' or else reads a
;;;     symbol

define lconstant star();
    lexget();
    if lexchar == `)` then
        lex_error('unmatched closing comment bracket\n', []);
    endif;
    chain(`*`, lex_symbol);
enddefine;

;;; lex_symbol:
;;;     reads a symbolic identifier, optionally starting with character -c-

define lconstant lex_symbol(c) -> item;
    lvars c, item;
    tryreserved(consword(#|
        if c then c endif;
        while lextype == symbol
        or lextype == tilde
        or lextype == star
        do
            lexchar; lexget();
        endwhile;
    |#)) -> item;
    if lexchar == `.` and isshortid(item) then
        lex_longid(item) -> item;
    else
        lexput();
    endif;
enddefine;

;;; lex_longid:
;;;     read the rest of a long identifier

define lconstant lex_longid(first);
    lvars first, rest;
    while (lexget(), lextype == esc) do handlepopescape() endwhile;
    if lextype == letter
    or lextype == symbol
    then
        lextype();
    elseif lextype == tilde
    or lextype == star
    then
        lex_symbol(lexchar, lexget());
    elseif lextype == format then
        lex_error('incomplete long identifier\n\t%p.\n', [^first]);
    elseif lextype == ctrl then
        ctrl();     ;;; error
    else
        lex_error('illegal character in long identifier\n\t%p.%c\n',
            [^first ^lexchar]);
    endif -> rest;
    if isreserved(rest) then
        unless rest == RESERVED '=' then
            lex_error('illegal item in long identifier\n\t%p.%p\n',
                [^first ^rest]);
        endunless;
        "=" -> rest;
    endif;
    conslongid(first, rest);
enddefine;

;;; lex_command_args:
;;;     read the arguments to a command: arbitrary sequences of characters
;;;     separated by white space, terminated by newline or semicolon. String
;;;     quotes can be used to escape special characters.

define lex_command_args(itemiser);
    lvars   itemiser;
    dlocal  lexchar, lextype, lexstream;
    dlocal  % lextable(`\n`+1) % = eof;
    ;;; Assume -itemiser- is a closure of -itemise- (above)
    instream_proc(frozval(2, itemiser)) -> lexstream;
    [%
        lexget();
        repeat
            while lextype == format do lexget() endwhile;
            if lextype == eof then
                quitloop;
            elseif lextype == scolon then
                if ml_allow_eolc then
                    if (lexget(), lextype == scolon)
                    and (lexget(), lextype == scolon)
                    then
                        until (lexget(), lextype == eof) do enduntil;
                    else
                        lexput();
                    endif;
                endif;
                quitloop;
            elseif lextype == ctrl then
                ctrl();     ;;; error
            elseif lextype == dquote then
                scon_value(dquote());
                lexget();
            elseif lextype == esc then
                handlepopescape();
                lexget();
            else
                consstring(#|
                    until lextype == format
                    or lextype == eof
                    or lextype == scolon
                    or lextype == ctrl
                    or lextype == dquote
                    or lextype == esc
                    do
                        lexchar, lexget();
                    enduntil;
                |#);
            endif;
        endrepeat;
    %];
    lexstream -> fast_back(fast_frozval(2, itemiser));
enddefine;

;;; Fill the lex table with appropriate classification procedures

;;; 00      01      02      03      04      05      06      07
;;; CTRL-@  CTRL-a  CTRL-b  CTRL-c  CTRL-d  CTRL-e  CTRL-f  CTRL-g
    ctrl,   ctrl,   ctrl,   ctrl,   ctrl,   ctrl,   ctrl,   ctrl,

;;; 010     011     012     013     014     015     016     017
;;; CTRL-h  CTRL-i  CTRL-j  CTRL-k  CTRL-l  CTRL-m  CTRL-n  CTRL-o
    ctrl,   format, format, ctrl,   format, format, ctrl,   ctrl,

;;; 020     021     022     023     024     025     026     027
;;; CTRL-p  CTRL-q  CTRL-r  CTRL-s  CTRL-t  CTRL-u  CTRL-v  CTRL-w
    ctrl,   ctrl,   ctrl,   ctrl,   ctrl,   ctrl,   ctrl,   ctrl,

;;; 030     031     032     033     034     035     036     037
;;; CTRL-x  CTRL-y  CTRL-z  CTRL-[  CTRL-\  CTRL-]  CTRL-^  CTRL-_
    ctrl,   ctrl,   ctrl,   esc,    ctrl,   ctrl,   ctrl,   ctrl,

;;; 040     041     042     043     044     045     046     047
;;; \s      !       "       #       $       %       &       '
    format, symbol, dquote, symbol, symbol, symbol, symbol, squote,

;;; 050     051     052     053     054     055     056     057
;;; (       )       *       +       ,       -       .       /
    lpar,   rpar,   star,   symbol, comma,  symbol, dot,    symbol,

;;; 060     061     062     063     064     065     066     067
;;; 0       1       2       3       4       5       6       7
    digit,  digit,  digit,  digit,  digit,  digit,  digit,  digit,

;;; 070     071     072     073     074     075     076     077
;;; 8       9       :       ;       <       =       >       ?
    digit,  digit,  symbol, scolon, symbol, symbol, symbol, symbol,

;;; 0100    0101    0102    0103    0104    0105    0106    0107
;;; @       A       B       C       D       E       F       G
    symbol, letter, letter, letter, letter, letter, letter, letter,

;;; 0110    0111    0112    0113    0114    0115    0116    0117
;;; H       I       J       K       L       M       N       O
    letter, letter, letter, letter, letter, letter, letter, letter,

;;; 0120    0121    0122    0123    0124    0125    0126    0127
;;; P       Q       R       S       T       U       V       W
    letter, letter, letter, letter, letter, letter, letter, letter,

;;; 0130    0131    0132    0133    0134    0135    0136    0137
;;; X       Y       Z       [       \       ]       ^       _
    letter, letter, letter, lbra,   symbol, rbra,   symbol, uscore,

;;; 0140    0141    0142    0143    0144    0145    0146    0147
;;; `       a       b       c       d       e       f       g
    symbol, letter, letter, letter, letter, letter, letter, letter,

;;; 0150    0151    0152    0153    0154    0155    0156    0157
;;; h       i       j       k       l       m       n       o
    letter, letter, letter, letter, letter, letter, letter, letter,

;;; 0160    0161    0162    0163    0164    0165    0166    0167
;;; p       q       r       s       t       u       v       w
    letter, letter, letter, letter, letter, letter, letter, letter,

;;; 0170    0171    0172    0173    0174    0175    0176    0177
;;; x       y       z       {       |       }       ~       CTRL-?
    letter, letter, letter, lbrace, symbol, rbrace, tilde,  ctrl,

fill(lextable) -> ;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Nov 11 1994
        Added some missing newlines to error messages
--- Robert John Duncan, Oct 24 1994
        Changes to error reporting
--- Robert John Duncan, Feb 22 1991
        Returns new lexical items.
--- Robert John Duncan, Feb 11 1991
        New style error messages
--- Simon Nichols, Nov 23 1990
        Added new character classification procedure -underscore- and
        changed -letter- to to prevent an underscore being accepted as the
        first character of an identifier.
 */
