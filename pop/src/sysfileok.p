/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sysfileok.p
 > Purpose:         Filename manipulation
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;;------------------- FILENAMES (UNIX) -----------------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (systranslate, isalphacode, isnumbercode,
        Sys$-Explode_substring)
    ;

;;; ---------------------------------------------------------------------

section $-Sys => pop_max_filename_len, sysfileok, dir_>< ;

#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0 or DEF SCO

lconstant macro MAXNAMLEN = 255;

#_ELSE

lconstant macro MAXNAMLEN = 14;

#_ENDIF

protected vars
    pop_max_filename_len    = MAXNAMLEN
    ;

lvars
    _parse_dir,
    _parse_name,
    _parse_extn,
    _parse_vers,
    ;

define lconstant Parse_filename(string);
    lvars string, _p, _fin, _d, _start;

    define :inline lconstant PARSE(T, T_V_FLD);
        string@T_V_FLD[_0] -> _start;
        _start@(T)[string!V_LENGTH] -> _fin;

        ;;; version
        while _fin >@(T) _start do
            if (_fin--!(T) -> _fin) /== _:`-` then
                _fin@(T)++ -> _fin, quitloop
            endif
        endwhile;
        _pint(##(T){_fin,_start} _add _1) -> _parse_vers;

        ;;; directory
        _start -> _p;
        while _p <@(T) _fin and (_p!(T)++ -> _p) /== _:`/` do endwhile;
        while _p >@(T) _start do
            if (_p--!(T) -> _p) == _:`!` then _p@(T)++ -> _p, quitloop endif
        endwhile;
        _pint(##(T){_p,_start} _add _1) -> _parse_dir;
        _p -> _d;

        ;;; file name
        _fin -> _p;
        while _p >@(T) _d and (_p--!(T) -> _p) /== _:`/` do endwhile;
        if _p <@(T) _fin and _p!(T) == _:`/` then _p@(T)++ -> _p endif;
        _pint(##(T){_p,_start} _add _1) -> _parse_name;

        ;;; extension
        _fin -> _d;
        while _d >@(T) _p do
            if (_d--!(T) -> _d) == _:`.` then _d -> _fin, quitloop endif
        endwhile;
        _pint(##(T){_fin,_start} _add _1) -> _parse_extn
    enddefine;

    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        PARSE(s, V_SHORTS)
    else
        PARSE(b, V_BYTES)
    endif
enddefine;

define sysfileok(string);
    lvars string, trans, _loc, _len, _char, _want_parse = false;
    _CLAWBACK_SAVE;

    define lconstant Isvalidcode(_c);
        lvars _c;
        _c == `_` or isalphacode(_c) or isnumbercode(_c)
    enddefine;

    if isboolean(string) then string -> _want_parse -> string endif;
    if isword(string) then
        string!W_STRING -> string
    else
        Check_string(string)
    endif;

    datalength(string) fi_+ 1 -> _len;
    returnif(_len == 1) (nullstring, if _want_parse then 1,1,1,1,1 endif);
    fast_subscrs(1, string) -> _char;
    if _char == `~` then
        ;;; if string begins with ~, translate following characters upto
        ;;; end of string or / as user login dir (by systranslate)
        unless locchar(`/`, 1, string) ->> _loc then
            _len -> _loc;
        endunless;
        if systranslate(substring(1, _loc fi_- 1, string)) ->> trans then
            cons_with consstring {%
                if trans /= nullstring then
                    deststring(trans) -> ,
                    if dup() == `/` and _loc /== _len then -> endif,
                endif,
                Explode_substring(_loc, _len fi_- _loc, string)
            %} -> string;
            datalength(string) fi_+ 1 -> _len;
        ;;; otherwise leave it alone
        endif
    endif;

    ;;; added pop_translate_envvars test A.S. 1 Nov 2003
    if locchar(`$`, 1, string) and pop_translate_envvars then
        ;;; the string may contain environment variables that need expanding
        _len fi_- 1 -> _len;
        cons_with consstring {%
            fast_for _loc to _len do
                fast_subscrs(_loc, string) -> _char;
                if _char == `$` then
                    ;;; possibly an env. var. Try for a conversion
                    fast_subscrs((_loc fi_+ 1 ->> _loc), string) -> _char;
                    if _char == `{` then
                        ;;; var. name is contained within curly braces
                        _loc fi_+ 1 -> _loc;
                        unless locchar(`}`, _loc, string) ->> _char then
                            mishap(string, 1, 'sysfileok: NO MATCHING } IN NAME')
                        endunless;
                        ;;; leave a copy of the var name on the stack
                        substring(_loc, _char fi_- _loc, string) ->> trans;
                        if systranslate(trans) ->> trans then
                            ;;; remove old var name
                            erase();
                            ;;; plant translation
                            deststring(trans) -> ,
                            _char -> _loc;
                        else
                            ;;; pull old name off of the stack...
                            -> trans;
                            ;;; ... then recreate string
                            `$`, `{`, deststring(trans) -> _char, `}`,
                            _loc fi_+ _char -> _loc;
                        endif;
                    else
                        ;;; search until EOS or (non-alpha and non-_) for name
                        cons_with consstring {%
                            while Isvalidcode(_char) do
                                _char,
                            quitif(_loc == _len);
                                _loc fi_+ 1 -> _loc;
                                fast_subscrs(_loc, string) -> _char;
                            endwhile;
                            if _loc == _len then
                                unless Isvalidcode(_char) then
                                    _loc fi_- 1 -> _loc;
                                endunless;
                            else
                                _loc fi_- 1 -> _loc;
                            endif;
                        %} ->> trans;
                        if systranslate(trans) ->> trans then
                            ;;; remove old name
                            erase();
                            ;;; plant translation
                            deststring(trans) -> ,
                        else
                            ;;; pull old name off of the stack...
                            -> trans;
                            ;;; ... then recreate string
                            `$`, deststring(trans) -> ,
                        endif;
                    endif;
                else
                    ;;; nothing special, so just dump it into new string.
                    _char;
                endif;
            endfor;
        %} -> string;
        datalength(string) fi_+ 1 -> _len;
    endif;

    ;;; parse the filename
    Parse_filename(string);

    ;;; now truncate file name to -pop_max_filename_len- chars,
    ;;; retaining extension if it would be truncated
    ;;; also make sure old-file trailing '-'s are retained
    ;;; skip pathname
    lvars maxnamlen = pop_max_filename_len, _trunc;
    _len fi_- _parse_name fi_- maxnamlen -> _trunc;
    if _trunc fi_> 0 then
        cons_with consstring {%
            Explode_substring(1, _parse_name fi_- 1, string);
            _len fi_- _parse_extn -> _len;      ;;; len of extn+version
            if _len fi_>= maxnamlen then
                mishap(string, 1, 'sysfileok: EXTENSION/VERSION PART TOO LONG')
            else
                Explode_substring(_parse_name, maxnamlen fi_- _len, string);
                Explode_substring(_parse_extn, _len, string);
                _parse_extn fi_- _trunc -> _parse_extn;
                _parse_vers fi_- _trunc -> _parse_vers
            endif
        %}
    else
        if _len == 1 then nullstring else string endif
    endif;
    Clawback();
    if _want_parse then
        dup(_parse_dir),    ;;; for disk and dir
        _parse_name, _parse_extn, _parse_vers
    endif
enddefine;


define 4 dir dir_>< name;
    lvars dir, name, _dirlen;
    _CLAWBACK_SAVE;
    if isword(dir) then
        dir!W_STRING
    elseunless isstring(dir) then
        dir sys_>< nullstring
    else
        dir
    endif -> dir;
    if isword(name) then
        name!W_STRING
    elseunless isstring(name) then
        name sys_>< nullstring
    else
        name
    endif -> name;
    datalength(dir) -> _dirlen;
    unless _dirlen == 0 or fast_subscrs(_dirlen,dir) == `/` then
        if datalength(name) == 0 or fast_subscrs(1,name) /== `/` then
            dir sys_>< '/' -> dir
        endif
    endunless;
    Clawback(dir sys_>< name)
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 1 Nov 2003
    Added pop_translate_envvars
    syfileok will translate environment variables if it is true (default)
--- John Gibson, Mar  4 1997
        String16 changes
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added case for SCO.
--- Robert John Duncan, Jun  7 1993
        Increased MAXNAMLEN for SVR4
--- Robert John Duncan, Jul 22 1992
        Added test to make file names be of reasonable length in SunOS 5.0
--- John Gibson, Aug 22 1989
        Now only requires test for BERKELEY on long names, since
        Bobcat has this set.
--- John Gibson, Jul 31 1989
        Changed -sysfileok- to return string plus 5 subscripts of fields
        within string when given extra 2nd of true.
            Also made dir_>< add a / when second name is empty and dir
        doesn't end with one.
--- Ian Rogers, Oct 19 1988
        Fixed bug in sysfileok, now accepts that digits can be part of
        logical variable names
--- Rob Duncan, Oct  6 1988
        Changed DEFV BOBCAT to DEF BOBCAT
--- John Williams, Sep 28 1988
        Bobcat now allows 255 character filenames
--- Ian Rogers,  Sep 17 1988
        Given -sysfileok- the ability to expand environment variables no
    matter where they appear in the string. Also accepts curly brackets as
    variable name delimiters.
--- John Gibson, May  9 1988
        Added -pop_max_filename_len-
--- John Gibson, Feb 22 1988
        Check_string into section Sys
--- John Gibson, Dec  6 1987
        Tidied up.
 */
