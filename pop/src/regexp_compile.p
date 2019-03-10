/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/regexp_compile.p
 > Purpose:         Regular expression matcher - compiler
 > Author:          Jonathan Meyer, July 10 1992
 > Documentation:   REF *REGEXP
 > Related Files:   SRC *VED_REGEXP_SEARCH.P *REGEXP_SEARCH.P
*/

;;; ----- REGULAR EXPRESSION COMPILER -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'regexp.ph'


constant
    procedure (
        initintvec,
        isalphacode
    ),
;

;;; -----------------------------------------------------------------------

section $-Sys =>
            regexp_compile
        ;


/*
This code is very strongly related to the code in /usr/include/regexp.h
on System V.3.2 and SunOS, with additions to cope with ved facilities
and long regular expressions.
*/

lconstant
    macro (
        ;;; Bits for flags argument to regexp_compile
        F_IGNORE_CASE       = 2:1e0,    ;;; case sensitivity
        F_NOT_EMBEDDED      = 2:1e1,    ;;; non-embedded " and ` searches
        F_DETECT_BREAK      = 2:1e2,    ;;; allow long regular expressions
        F_USE_VEDCHARTYPE   = 2:1e3,    ;;; vedatitemstart/end for word bounds
        F_USE_VEDINDENTSTEP = 2:1e4,    ;;; expect tabs to be padded VED style
        F_ESC_MODE          = 2:1e5,    ;;; literal/unliteral escape mode
      ),
  ;

;;; see regexp_search.p for definitions of these two...
constant procedure (regexp_search, regexp_invalid);

;;; DECLARE -goto- LABELS AS LVARS TO STOP THE COMPILER FROM AUTOLOADING THEM
;;; lvars dollar, dot, defchar, nlim;

;;; this compiles a single regular expression
define regexp_compile(inputstr) -> regexp_p -> err;
    lvars
        ;;; inputs
        inputstr,               ;;; the regular expression
        eofc,                   ;;; delimeter character for string
        flags,                  ;;; case sensitivity, etc.
        escc,                   ;;; escape character
        allowunescaped,         ;;; start in 'unliteral' mode?

        ;;; outputs
        regexp_p    = regexp_invalid, ;;; the resulting regexp procedure
        err         = false,    ;;; error message

        ;;; input string and output string management variables
        inputp = 1,             ;;; index in inputstr
        inputlen,               ;;; length of inputstr
        outputstr   = inits(32),;;; compiled regular expression
        outputp     = 1,        ;;; index in outputstr
        outputlen   = 32,       ;;; length of outputstr (it can grow)
        lastp       = false,    ;;; last position in outputstr

        ;;; state variables for () expressions
        bracket     = initintvec(NBRA),
        bracketp    = 1,        ;;; index to bracketp
        nbrackets   = 0,        ;;; total number of () expressions
        closed      = 0,        ;;; test for balance of ) brackets

        ;;; working variables
        detectbreak,            ;;; true if ^ after $ is a delimeter
        casemakesdifference,    ;;; true if inputstr has an alpha char
        startoutp   = outputp,  ;;; where this expression starts
        circf       = false,    ;;; true if string starts with @^
        dollar      = false,    ;;; true if string contains a @$
        linedata    = [],       ;;; per-line data
        nlines      = 1,        ;;; number of lines
        iflag,                  ;;; used for non-ascii chars in [] brackets
        commacount,             ;;; used in handling of {} expressions
        neg,                    ;;; -true- for negatives in [] expressions
        notembedded,            ;;; true if it is an implicit @< @> search
        minlength = 0,          ;;; minimum length for matching string
        firstchar = undef,      ;;; first character of expression
        procedure filter_p = identfn, ;;; filter for case sensitivity
        c, lc, i, cclcnt, nextc,
      ;

    lconstant Error_map = {
            ;;; ERROR     MEANING
            /*1*/        'Regular expression too long'
            /*2*/        '@( @) imbalance'
            /*3*/        'Illegal or missing delimiter'
            /*4*/        '@[ @] imbalance'
            /*5*/        'Too many @( '
            /*6*/        'Bad number'
            /*7*/        'Range endpoint too large'
            /*8*/        'More than 2 numbers  given  in @{ @}'
            /*9*/        '} expected after @'
            /*10*/       'First number exceeds second in @{ @}'
            /*11*/       '`@ digit\' out of range'
            /*12*/       'Badly placed @z'
            /*13*/       'More than one * or @{ @} on same item'
        };

    ;;; grows the output buffer
    define lconstant Try_grow;
        lvars growbuf, growlen;
        inits(outputlen + 32 ->> growlen) -> growbuf;
        outputstr -> substring(1, outputlen, growbuf);
        growbuf -> outputstr; growlen -> outputlen;
        true;
    enddefine;

    define lconstant Get_char() -> c; lvars c;
        ;;; get the next character in the input, advance to next location.
        ;;; sets nextc to a one-character lookahead.
        inputp > inputlen and termin or subscrs(inputp, inputstr) -> c;
        inputp + 1 -> inputp;
        inputp > inputlen and termin or subscrs(inputp, inputstr) -> nextc;
        filter_p(c) -> c; filter_p(nextc) -> nextc;
    enddefine;

    define lconstant Output_byte(c); lvars c;
        ;;;write -c- to the current location, advance to next location
        c -> subscrs(outputp, outputstr);
        outputp +1 -> outputp
    enddefine;

    ;;; PROCESS ARGUMENTS
    undef -> escc; 0 -> flags; false -> eofc;
    ;;; HANDLE OPTIONAL ARGUMENTS
    unless inputstr.isstring or inputstr.isword then
        inputstr -> (inputstr, flags);
    endunless;
    unless inputstr.isstring or inputstr.isword then
        (inputstr, flags) -> (inputstr, flags, eofc);
    endunless;
    unless inputstr.isstring or inputstr.isword then
        (inputstr, flags, eofc) -> (inputstr, flags, eofc, escc);
    endunless;

    if inputstr.isword then
        fast_word_string(inputstr) -> inputstr
    elseunless inputstr.isstring then
        mishap(inputstr, 1, 'WORD or STRING NEEDED');
    endif;
    datalength(inputstr) -> inputlen;
    if escc == undef then `@` -> escc endif;

    ;;; PROCESS FLAGS
    uppertolower(inputstr) /== lowertoupper(inputstr) -> casemakesdifference;
    if flags &&/=_0 F_IGNORE_CASE then
        if casemakesdifference then
            ;;; ignore case.
            uppertolower -> filter_p;
        else
            ;;; case makes no difference anyway.
            flags &&~~ F_IGNORE_CASE -> flags;
        endif;
    endif;
    flags &&/=_0 F_NOT_EMBEDDED -> notembedded;
    flags &&/=_0 F_DETECT_BREAK -> detectbreak;
    flags &&/=_0 F_ESC_MODE -> allowunescaped;

    ;;; test that the VED procedures are available.

    ;;; test for special first characters.
    Get_char() -> c;
    uppertolower(nextc) -> lc;
    while c == escc and lc /== termin and strmember(lc, 'luic') do
        if lc == `u` then
            true -> allowunescaped;
        elseif lc == `l` then
            false -> allowunescaped;
        elseif lc == `i` and casemakesdifference then
            uppertolower -> filter_p;
            flags || F_IGNORE_CASE -> flags;
        elseif lc == `c` and casemakesdifference then
            identfn -> filter_p;
            flags &&~~ F_IGNORE_CASE -> flags;
        endif;
        Get_char() -> /*luic*/;
        Get_char() -> c;
        uppertolower(nextc) -> lc;
    endwhile;
    if allowunescaped and c == `^` then
        true -> circf;
    elseif c == escc and (nextc == `a` or nextc == `A`
    or (not(allowunescaped) and nextc == `^`)) then
        ;;; @a or @A maps onto ^
        true -> circf;
        inputp + 1 -> inputp;
    else
        inputp - 1 -> inputp;
    endif;

    ;;; deal with non-embedded searches
    if notembedded then Output_byte(CBRC); endif;

    ;;; COMPILE REGULAR EXPRESSION
    repeat;
        if outputlen - outputp < 0 then
            returnunless(Try_grow())(Error_map(1) -> err);
        endif;
        Get_char() -> c;
        unless (allowunescaped and c == `*`) or (c == escc and
        (nextc == `{` or (not(allowunescaped) and nextc == `*`))) then
            outputp -> lastp;
        endunless;

        if c == eofc or c == termin or c == `\n` or detectbreak == 1 then
            ;;; Exit condition.
            ;;; we have reached the end of the regular expression.

            if bracketp /== 1 then
                return(Error_map(2) -> err); ;;; ERROR
            endif;

            ;;; deal with implicit non-embedded searches.
            if notembedded then
                if outputlen - outputp < 1 then
                    returnunless(Try_grow())(Error_map(1) -> err);
                endif;
                Output_byte(CLET);
            endif;

            ;;; mark the end of the regular expression.
            Output_byte(CCEOF);

            if firstchar == undef then false -> firstchar endif;
            {^startoutp ^minlength} :: linedata -> linedata;

            if detectbreak == 1 then
                ;;; we hit a line break - start a new line
                0 -> minlength;
                outputp -> startoutp;
                false -> lastp;
                true -> detectbreak;
                nlines + 1 -> nlines;
                nextloop;
            endif;

            ;;; return a closure on regexp_search
            consclosure(
                regexp_search,
                {% c == eofc and inputp - 1, nbrackets, dollar or circf %},
                if nbrackets == 0 then
                    ;;; no point allocating vectors for brackets.
                    false, false, false,
                else
                    ;;; should be writeable to cope with locking.
                    writeable initintvec(nbrackets),
                    writeable initintvec(nbrackets),
                    writeable initv(nbrackets),
                endif,
                nlines,
                rev(linedata),
                outputstr,
                circf,
                firstchar,
                flags &&/=_0 F_IGNORE_CASE,
                flags &&/=_0 F_USE_VEDINDENTSTEP,
                flags &&/=_0 F_USE_VEDCHARTYPE,
                CLOS_LENGTH
              ) -> regexp_p;
            substring(1, inputp - 2, inputstr) -> pdprops(regexp_p);
            ;;; pdnargs is  nlines + strp + strsize + back or nlines + 3
            nlines + 3 -> pdnargs(regexp_p);

            ;;; protect the closure
            _:M_PD_CLOSURE _biset _:M_PD_CLOS_PROTECT -> regexp_p!PD_FLAGS;

            return(); ;;; SUCCESS

        elseif allowunescaped and c == `.` then ;;; single character match, also @?
        dot:
            if allowunescaped == undef then false -> allowunescaped endif;
            Output_byte(CDOT);
            ;;; increase minlength by 1
            minlength + 1 -> minlength;
            if firstchar == undef then false -> firstchar endif;
            nextloop;

        elseif allowunescaped and c == `*` then ;;; multi character match
            if allowunescaped == undef then false -> allowunescaped endif;
            if lastp == false or (subscrs(lastp, outputstr) ->> i) == CBRA or
                    i == CKET or i == CBRC or i == CLET  then
                goto defchar;
            endif;
            if i &&/=_0 (STAR || RNGE) then
                return(Error_map(13) -> err); ;;; ERROR
            endif;
            if i /== CBACK then
                ;;; because the last character is repeated zero or more times,
                ;;; we must discount it from minlength
                minlength - 1 -> minlength
            endif;
            i || STAR -> subscrs(lastp, outputstr);
            if firstchar == undef or lastp == 1 then false -> firstchar endif;
            nextloop;

        elseif allowunescaped and c == `$` then ;;; end of line, also @z
        dollar:
            if allowunescaped == undef then false -> allowunescaped endif;
            if detectbreak then
                ;;; check for $@a or $^
                if nextc == escc then
                    Get_char() -> ;
                    if nextc == `a` or nextc == `A`
                    or (not(allowunescaped) and nextc == `^`) then
                        1 -> detectbreak;
                    else
                        ;;; undo the last Get_char
                        inputp - 1 -> inputp;
                    endif;
                elseif allowunescaped and nextc == `^` then
                    1 -> detectbreak;
                endif;
            endif;
            unless nextc == eofc or nextc == `\n` or nextc == termin
            or detectbreak == 1 then
                goto defchar; ;;; return(Error_map(12) -> err); ;;; ERROR ?
            endunless;
            true -> dollar;
            Output_byte(CDOL);
            nextloop;

        elseif allowunescaped and c == `[` then ;;; character class
            if allowunescaped == undef then false -> allowunescaped endif;
            if outputlen - outputp < 16 then
                returnunless(Try_grow())(Error_map(1) -> err);
            endif;
            ;;; increase minlength by 1
            minlength - 1 -> minlength;
            if firstchar == undef then false -> firstchar endif;
            Output_byte(CCL);
            for i from 0 to 15 do
                0 -> subscrs(outputp + i, outputstr);
            endfor;
            false -> neg;
            if (Get_char() ->> c) == `^` then
                true -> neg;
                Get_char() -> c;
            endif;
            0 -> lc;
            true -> iflag;

            ;;; this is used to place a character in a character range.
            define lconstant Place(c); lvars c;
                subscrs(outputp + (c >> 3), outputstr) || (1<<(c && 7))
                    -> subscrs(outputp + (c >> 3), outputstr)
            enddefine;

            repeat;
                if c == termin or (c && 8:377 ->> c) == `\n` then
                    return(Error_map(4) -> err);    ;;; ERROR
                endif;
                if (c &&/=_0 8:200) and iflag then
                    false -> iflag;
                    if outputlen - outputp < 31 then
                        returnunless(Try_grow())(Error_map(1) -> err);
                    endif;
                    CXCL -> subscrs(outputp - 1, outputstr);
                    for i from 16 to 31 do
                        0 -> subscrs(outputp + i, outputstr);
                    endfor;
                endif;
                if c == `-` and lc /== 0 then
                    Get_char() -> c;
                    if allowunescaped and c == `]` or (not(allowunescaped) and
                    c == escc and nextc == `]`) then
                        Place(`-`);
                        quitloop;
                    elseif c == termin then
                        return(Error_map(4) -> err); ;;; ERROR
                    endif;
                    if (c &&/=_0 8:200) and iflag then
                        false -> iflag;
                        if outputlen - outputp < 31 then
                            returnunless(Try_grow())(Error_map(1) -> err);
                        endif;
                        CXCL -> subscrs(outputp - 1, outputstr);
                        for i from 16 to 31 do
                            0 -> subscrs(outputp + i, outputstr);
                        endfor;
                    endif;
                    while(lc < c ) do
                        Place(lc);
                        lc +1 -> lc;
                    endwhile;
                endif;
                c -> lc;
                Place(c);
                Get_char() -> c;
                quitif(allowunescaped and c == `]` or (not(allowunescaped) and
                    c == escc and nextc == `]`));
            endrepeat;
            unless allowunescaped then Get_char() -> endunless; ;;; remove ]
            if neg then
                if not(iflag) then
                    for cclcnt from 0 to 31 do
                        subscrs(outputp + cclcnt, outputstr) ||/& 8:377
                            -> subscrs(outputp + cclcnt, outputstr);
                    endfor;
                    subscrs(outputp, outputstr) && 8:376
                        -> subscrs(outputp, outputstr);
                else
                    NCCL -> subscrs(outputp - 1, outputstr);
                    ;;; make nulls match so test fails.
                    subscrs(outputp, outputstr) || 8:1
                        -> subscrs(outputp, outputstr);
                endif;
            endif;
            (iflag and 16 or 32) + outputp -> outputp;
            nextloop;

        elseif c == escc then ;;; ie. `@'
            Get_char() -> c;
            uppertolower(c) -> lc;

            if c == termin or c == escc then
                ;;; @@ maps to @, as does @ at the end of the string
                if c /== escc then
                    inputp - 1 -> inputp;
                endif;
                escc -> c;
                goto defchar;

            elseif not(allowunescaped) and locchar(c, 1, '.*$[') then
                ;;; make . * $ and [ temporarily special
                undef -> allowunescaped;
                inputp - 1 -> inputp;
                nextloop;

            elseif lc == `z` then
                ;;; @z or @Z maps onto $
                goto dollar;

            elseif c == `?` then
                ;;; @? maps onto .
                goto dot;

            elseif lc == `u` then
                ;;; @u turns on unliteral mode - ie. turns off literal mode.
                true -> allowunescaped;

            elseif lc == `l` then
                ;;; @l turns on literal mode - ie. turns off unliteral mode.
                false -> allowunescaped;

            elseif lc == `c` then
                unless casemakesdifference and filter_p == uppertolower then
                    Output_byte(CCASE);
                    identfn -> filter_p;
                endunless;
                nextloop;

            elseif lc == `i` then
                unless casemakesdifference and filter_p == identfn then
                    Output_byte(NCCASE);
                    uppertolower -> filter_p;
                endunless;
                nextloop;

            elseif c == `<` then    ;;; @< constrains to start of word
                Output_byte(CBRC);
                nextloop;

            elseif c == `>` then
                ;;; @> constrains to end of word
                Output_byte(CLET);
                nextloop;

            elseif c == `(` then    ;;; @( marks start of sub-expression
                if nbrackets >= NBRA then
                    return(Error_map(5) -> err); ;;; ERROR
                endif;
                if outputlen - outputp < 1 then
                    returnunless(Try_grow())(Error_map(1) -> err);
                endif;
                nbrackets + 1 -> nbrackets;
                nbrackets -> bracket(bracketp);
                bracketp + 1 -> bracketp;
                Output_byte(CBRA);
                Output_byte(nbrackets);
                nextloop;

            elseif c == `)` then    ;;; @) marks end of sub-expression
                if bracketp <= 1 then
                    return(Error_map(2) -> err); ;;; ERROR
                endif;
                if outputlen - outputp < 1 then
                    returnunless(Try_grow())(Error_map(1) -> err);
                endif;
                Output_byte(CKET);
                Output_byte(bracket(bracketp -1 ->> bracketp));
                closed +1 -> closed;
                nextloop;

            elseif c >= `1` and c <= `9` then
                ;;; @n where n is a digit refers back to a sub-expression
                if (c - `0` ->> c) > closed then
                    return(Error_map(11) -> err); ;;; ERROR
                endif;
                if outputlen - outputp < 1 then
                    returnunless(Try_grow())(Error_map(1) -> err);
                endif;
                Output_byte(CBACK);
                Output_byte(c);
                nextloop;

            elseif c == `{` then
                ;;; @{ @} places constraint on number of matches
                if lastp == false then
                    goto defchar;
                endif;
                subscrs(lastp, outputstr) -> lc;
                if lc &&/=_0 (RNGE || STAR) then
                    return(Error_map(13) -> err); ;;; ERROR
                endif;
                lc || RNGE -> subscrs(lastp, outputstr);
                0 -> commacount;
            nlim:
                Get_char() -> c;
                0 -> i;
                repeat;
                    if c /== termin and `0` <= c and c <= `9` then
                        10 * i + c - `0` -> i;
                    else
                        ;;; not a number
                        return(Error_map(6) -> err); ;;; ERROR
                    endif;
                    Get_char() -> c;
                    quitif(c == escc or c == `,`);
                endrepeat;
                if i >= 255 then
                    return(Error_map(7) -> err); ;;; ERROR
                elseif commacount == 0 and i == 0 and lastp == 1 then
                    ;;; must clear firstchar if low is 0.
                    false -> firstchar;
                endif;
                Output_byte(i);
                if commacount == 0 and lc /== CBACK then
                    ;;; must modify minlength
                    minlength - 1 + i -> minlength
                endif;
                if c == `,` then
                    if commacount /== 0 then
                        return(Error_map(8) -> err); ;;; ERROR
                    endif;
                    commacount +1 -> commacount;
                    if (Get_char() ->> c) == escc then
                        if outputlen - outputp < 0 then
                            returnunless(Try_grow())(Error_map(1) -> err);
                        endif;
                        Output_byte(255);
                    else
                        inputp - 1 -> inputp;
                        goto nlim;
                        ;;; get 2`nd number.
                    endif;
                endif;
                if Get_char() /== `}` then
                    return(Error_map(9) -> err); ;;;ERROR
                endif;
                if commacount == 0 then ;;; one number.
                    if outputlen - outputp < 0 then
                        returnunless(Try_grow())(Error_map(1) -> err);
                    endif;
                    Output_byte(i);
                elseif (subscrs(outputp - 1, outputstr) && 8:377)
                        < (subscrs(outputp - 2, outputstr) && 8:377) then
                    return(Error_map(10) -> err); ;;; ERROR
                endif;
                nextloop;

            elseif c == `\n` then
                return(Error_map(3) -> err); ;;; ERROR

            elseif escc == `\\` and strmember(c, 'nbtsre') ->> lc then
                subscrs(lc, '\n\b\t\s\r\e') -> c;
                ;;; FALLTHROUGH TO DEFCHAR

            elseif escc == `\\` and c == `^` then
                if isalphacode(nextc) or nextc == `@` or nextc == `_` then
                    lowertoupper(Get_char()) - `@  -> c;
                elseif nextc == `?` then
                    erase(Get_char()); `\^?` -> c;
                endif;
                ;;; FALLTHROUGH TO DEFCHAR
            endif;
            goto defchar;

        ;;; DEFAULT ACTION
        else
        defchar:
            outputp -> lastp;
            if outputlen - outputp < 1 then
                returnunless(Try_grow())(Error_map(1) -> err);
            endif;
            Output_byte(CCHR);
            Output_byte(c);
            ;;; increase minlength by 1
            minlength + 1 -> minlength;
            if firstchar == undef then c -> firstchar endif;
        endif;
    endrepeat;
enddefine; /* regexp_compile */

endsection;     /* $-Sys */
