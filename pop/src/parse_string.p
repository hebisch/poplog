/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/parse_string.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---- PARSE AN ARGUMENT STRING INTO WHITESPACE-SEPARATED ITEMS ---------

#_INCLUDE 'declare.ph'


;;; ---------------------------------------------------------------------

section $-Sys => sys_parse_string;

define sys_parse_string(string);
    lvars   string, char, sepchar = false,
            procedure (p = identfn, subscr_p, cons_p),
            _len, _num, _count = 0;

    if isprocedure(string) then
        ;;; procedure to apply to each parsed string
        (), string -> (string, p)
    endif;
    if isinteger(string) then
        ;;; separator character
        (), string -> (string, sepchar)
    endif;

    if isword(string) then
        string!W_STRING -> string
    else
        Check_bytevec(string)
    endif;
    string!KEY!K_FAST_SUBSCR_V -> subscr_p;
    string!KEY!K_CONS_V -> cons_p;

    datalength(string) -> _len;
    returnif(_len == 0);

    ;;; leave the strings on the stack
    fast_for _num to _len do
        subscr_p(_num, string) fi_&& 16:FFFF -> char;
        if sepchar then
            if char == sepchar then false -> char endif
        elseif char == `\s` or char == `\t` or char == `\n` then
            false -> char
        endif;
        if char then
            _CHECKUSER;
            char;
            _count fi_+ 1 -> _count
        elseif _count /== 0 then
            p(cons_p(_count));
            0 -> _count
        elseif sepchar then
            p(nullstring)
        endif
    endfor;
    if _count /== 0 then
        p(cons_p(_count))
    elseif sepchar then
        p(nullstring)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 14 1997
        When given sepchar, made it call p on nullstring for empty fields
        (otherwise you can't parse e.g. a line of comma-separated fields
        correctly).
--- John Gibson, Apr 17 1996
        Allowed to take an extra arg to specify the separator char (also
        made comparisons ignore char attributes).
--- John Williams, Jul  6 1993
        Now produces dstrings if given a dstring as argument.
        (cf. BR adrianh.20)
--- John Gibson, Oct 20 1992
        Rewritten as sys_parse_string to deal with tabs, etc.
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
