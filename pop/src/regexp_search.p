/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/regexp_search.p
 > Purpose:         Regular expression matcher - search routine and utilities
 > Author:          Jonathan Meyer, July 10 1992 (see revisions)
 > Documentation:   REF *REGEXP
 > Related Files:   SRC *VED_REGEXP_SEARCH.P *REGEXP_COMPILE.P
*/

;;; ---- REGULAR EXPRESSION SEARCHING ---------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'regexp.ph'

weak vars
    vedindentstep
  ;

weak constant
    procedure (
        vedatitemstart,
        vedatitemend,
        vedusedsize,
      ),
  ;

constant
    procedure (
        subscrintvec,
        sys_grbg_destpair,
        isalphacode,
        isnumbercode,
        conslist,
    ),
  ;


;;; -----------------------------------------------------------------------

section $-Sys =>
            isregexp,
            regexp_break_count, regexp_subexp_count, regexp_subexp,
            regexp_delimeter, regexp_anchored,
        ;

/*
This code is very strongly related to the code in /usr/include/regexp.h
on System V.3.2 and SunOS, with additions to cope with ved facilities
and long regular expressions.
*/



;;; VED TABS: Jump_over_tab(STR, COLUMN, GOING_FORWARD) -> COLUMN
;;;   Takes an index into the string str. Adjusts the index to
;;;   skip over any tab pad characters. If GOING_FORWARD, jumps to nearest
;;;   non-pad location to right of COLUMN, otherwise jumps to the nearest
;;;   non-pad loction to left of COLUMN.
define lconstant Jump_over_tab(str, strp, forward) -> strp;
    lvars str, strp, forward, indent = weakref vedindentstep;

    ;;; These came from SRC vdtabs.p

    define lconstant Tab_size_at(col);
        lvars col;
        (indent fi_- ((col fi_- 1) fi_rem indent))
    enddefine;

    define lconstant Col_is_inside_tab(col, string);
        lvars string, col;
        not(col fi_<= 1 or col fi_> datalength(string)
            or fast_subscrs(col, string) /== `\t`
            or fast_subscrs(col fi_- 1, string) /== `\t`
            or (col fi_- 1) fi_rem indent == 0
        )
    enddefine;

    ;;; find the start of a tab in string backwards from col
    define lconstant Find_tab_start(col, string);
        lvars string, col, lim;
        col fi_+ Tab_size_at(col) fi_- indent -> lim;
        while col fi_> lim  do
            col fi_- 1 -> col;
            returnif(fast_subscrs(col, string) /== `\t`) (col fi_+ 1)
        endwhile;
        lim;
    enddefine;

    if Col_is_inside_tab(strp, str) then
        ;;; string has hard tabs in it and strp is in the middle of a tab.
        if forward then
            strp fi_+ Tab_size_at(strp) -> strp;
        else
            Find_tab_start(strp, str) -> strp;
        endif;
    endif;
enddefine; /* Jump_over_tab */

;;; DECLARE -goto- LABELS AS LVARS TO STOP THE COMPILER FROM AUTOLOADING THEM
;;; lvars do_ccl, do_ccl_star, do_ccl_range, star;

;;; Macros for iterating over the compiled regular expression.
define :inline lconstant R(n); ;;; ep[n]
    (fast_subscrs(regexp fi_+ (n), regexstr))
enddefine;
define lconstant macro *_R; ;;; *ep
    [(fast_subscrs(regexp, regexstr))].dl
enddefine;
define lconstant macro R_+= n; ;;; ep += n
    lvars n;
    "regexp", "fi_+", n, "->", "regexp"
enddefine;
define lconstant macro R_++; ;;; *r++
    [(*_R, R_+= 1)].dl
enddefine;
;;; Macros for iterating over the string that is being searched.
define lconstant macro CHECKTAB;
    [(if jumptabs then Jump_over_tab(str, strp, true) -> strp endif)].dl
enddefine;
define lconstant macro CHECKTAB_BACK;
    [(if jumptabs then Jump_over_tab(str, strp, false) -> strp endif)].dl
enddefine;
define lconstant macro *_S; ;;; *lp
    [(strp fi_<= strsize and fast_apply(fast_subscrs(strp, str),filter_p)
      or termin)].dl
enddefine;
define lconstant macro S_+= n; ;;; lp += n
    lvars n;
    [(strp fi_+ (].dl, n, [) -> strp, CHECKTAB)].dl
enddefine;
define lconstant macro S_-= n; ;;; lp -= n
    lvars n;
    [(strp fi_- (].dl, n, [) -> strp, CHECKTAB_BACK)].dl
enddefine;
define lconstant macro S_++; ;;; *lp++
    [ ( *_S, S_+= 1 ) ].dl
enddefine;
define :inline lconstant ISTHERE(c);
    (R(c fi_>> 3) &&/=_0 (1 fi_<< (c fi_&& 7)))
enddefine;
define :inline lconstant WHITESPACE(c);
    ;;; this is the definition used by the `C` regexp.h
    not(isalphacode(c) or c == `_` or isnumbercode(c))
enddefine;

;;; used to make locchar and skipchar case-insensitive if necessary.
define lconstant Case(proc, c, n, str, ignorecase);
    lvars proc, c, n, str, ignorecase, l1, l2;
    if ignorecase and isalphacode(c) then
        if proc == skipchar then
            ;;; must implement this one ourselves
            datalength(str) -> l2;
            unless n fi_>= 1 then mishap(n,1, 'INTEGER >= 1 NEEDED') endunless;
            while n fi_<= l2 and uppertolower(fast_subscrs(n, str)) == c do
                n fi_+ 1 -> n;
            endwhile;
            (n fi_<= l2 and n);
        else
            ;;; apply the search to both upper and lower case characters
            fast_apply(c, n, str, proc) -> l1;
            fast_apply(lowertoupper(c), n, str, proc) -> l2;
            if l1 and l2 then
                ;;; select the nearest/farthest match depending on proc
                proc == locchar and fi_min(l1, l2) or fi_max(l1, l2)
            else
                ;;; select either character
                l1 or l2
            endif;
        endif;
    else
        ;;; can use the routine as it stands
        fast_chain(c, n, str, proc);
    endif;
enddefine;

define regexp_search(strp, str, strsize, backward,
                                props, braslist, braelist, bravlist,
                                nlines, linedata, regexstr, circf, firstchar,
                                ignorecase, usevedindentstep, usevedchartype)
                    -> (loc1, loc2);
    lvars
        c,
        ;;; user specified arguments:
        strp, str, strsize,     ;;; string to match against
        backward,               ;;; true if we are searching right-to-left

        ;;; these are part of the regexp_search closure made by regexp_compile
        props                   ;;; unused - see regexp_delimeter, etc.
        braslist, braelist,     ;;; start and end of bracketed expressions
        bravlist,               ;;; vector of strings which brackets matched
        nlines,                 ;;; number of lines in regexp
        linedata,               ;;; data which is local to each line
        regexstr,               ;;; compiled regular expression
        circf,                  ;;; true if constrained to start of line
        firstchar,              ;;; first character in expression
        ignorecase,             ;;; true if we ignore case
        usevedindentstep,       ;;; true if search should adjust for VED tabs
        usevedchartype,         ;;; true if word boundaries use vedchartype

        ;;; results:
        loc1, loc2,             ;;; return locations

        ;;; working variables:
        jumptabs,               ;;; true if usevedindentstep and string has tab
        filter_p        = ignorecase and uppertolower or identfn,
      ;

    ;;; Succeeds if substring(start, n, str2) = substring(strp, n, str).
    define lconstant Strncmp(start, n, str2, strp);
        lvars str2, start, strp, n, bsize, jump, endpos;
        ;;; Handles VED tabs if necessary.
        if str2 == str then
            jumptabs -> jump;
            strsize -> bsize;
        else
            jumptabs or (usevedindentstep and locchar(`\t`, 1, str2)) -> jump;
            if usevedindentstep then
                weakref vedusedsize(str2)
            else
                datalength(str)
            endif -> bsize;
        endif;
        start fi_+ n -> endpos;
        if not(jump) and strp fi_+ n fi_-1 fi_> strsize then
            return(false);
        endif;
        until start fi_>= endpos do
            returnunless(fast_apply(fast_subscrs(start,str2), filter_p)
                == fast_apply(fast_subscrs(strp,str), filter_p))(false);
            start fi_+1 -> start; strp fi_+1 -> strp;
            if jump then
                ;;; adjust to cope with any tab padding characters in the str.
                Jump_over_tab(str2, start, true) -> start;
                Jump_over_tab(str, strp, true) -> strp;
                if strp fi_> strsize then
                    return(false);
                endif;
            endif;
        enduntil;
        strp;
    enddefine; /* Strncmp */

    ;;; this is the main procedure for iterating over the regular expression.
    define lconstant Advance(strp, regexp);
        lvars
            strp, regexp,           ;;; index pointers into string/regexp.
            savestrp,               ;;; used to store start of * expressions.
            neg,                    ;;; true for negative expressions.
            bbeg, bnchars, bstr,    ;;; used for @n back references.
            low, size,              ;;; holds range for character classes.
            c, tmp,
          ;
        define lconstant Getrange(offs);
            lvars offs;
            R(offs) -> low;
            R(offs fi_+ 1) -> size;
            size == 255 and 20000 or size fi_- low -> size;
        enddefine;

        fast_repeat;
            false -> neg;
            R_++ -> c;

            if c == CCHR then
                if R_++ == S_++ then
                    nextloop;
                endif;
                return(false);

            elseif c == CDOT then
                if S_++ /== termin then
                    nextloop;
                endif;
                return(false);

            elseif c == CDOL then
                if *_S == termin then
                    nextloop;
                endif;
                return(false);

            elseif c == CCEOF then
                strp -> loc2;
                return(true);

            elseif c == CXCL then
                S_++ -> c;
                if c /== termin and ISTHERE(c) then
                    R_+= 32;
                    nextloop;
                endif;
                return(false);

            elseif c == NCCL then
                true -> neg;
                goto do_ccl;

            elseif c == CCL then
            do_ccl:
                S_++ -> c;
                if c /== termin and ((c &&=_0 8:200 and ISTHERE(c)) /== neg)
                then
                    R_+= 16;
                    nextloop;
                endif;
                return(false);

            elseif c == CBRA then
                R_++ -> c;
                strp -> subscrintvec(c, braslist);
                str -> bravlist(c);
                nextloop;

            elseif c == CKET then
                R_++ -> c;
                strp -> braelist(c);
                nextloop;

            elseif c == #_< CCHR || RNGE >_# then
                R_++ -> c;
                Getrange(0);
                fast_repeat low times
                    if S_++ /== c then
                        return(false);
                    endif;
                endfast_repeat;
                strp -> savestrp;
                until (size fi_-1 ->> size) == -1 do
                    if S_++ /== c then
                        quitloop;
                    endif;
                enduntil;
                if size == -1 then
                    S_+= 1;
                endif;
                R_+= 2;
                goto star;

            elseif c == #_< CDOT || RNGE >_#  then
                Getrange(0);
                fast_repeat low times
                    if S_++ == termin then
                        return(false);
                    endif;
                endfast_repeat;
                strp -> savestrp;
                until (size fi_-1 ->> size) == -1 do
                    if S_++ == termin then
                        quitloop;
                    endif;
                enduntil;
                if size == -1 then
                    S_+= 1;
                endif;
                R_+= 2;
                goto star;

            elseif c == #_< CXCL || RNGE >_# then
                Getrange(32);
                fast_repeat low times
                    S_++ -> c;
                    if c == termin or not(ISTHERE(c)) then
                        return(false);
                    endif;
                endfast_repeat;
                strp -> savestrp;
                until (size fi_-1 ->> size) == -1 do
                    S_++ -> c;
                    if c == termin or not(ISTHERE(c)) then
                        quitloop;
                    endif;
                enduntil;
                if size == -1 then
                    S_+= 1;
                endif;
                R_+= 34;        ;;; 32 + 2
                goto star;

            elseif c == #_< NCCL || RNGE >_# then
                true -> neg;
                goto do_ccl_rnge;

            elseif c == #_< CCL || RNGE >_# then
            do_ccl_rnge:
                Getrange(16);
                fast_repeat low times
                    S_++ -> c;
                    if c == termin or (((c &&/=_0 8:200) or not(ISTHERE(c)))
                                                /== neg) then
                        return(false);
                    endif;
                endfast_repeat;
                strp -> savestrp;
                until (size fi_-1 ->> size) == -1 do
                    S_++ -> c;
                    if c == termin or (((c &&/=_0 8:200) or not(ISTHERE(c)))
                                                /== neg) then
                        quitloop;
                    endif;
                enduntil;
                if size fi_< 0 then
                    S_+= 1;
                endif;
                R_+= 18;        ;;; 16 + 2
                goto star;

            elseif c == #_< CBACK || RNGE >_# then
                ;;; This is greatly complicated by VED tabs. A 5 character
                ;;; piece of text in one part of a string may be any number of
                ;;; characters in another part of the string.
                R_++ -> c;
                subscrintvec(c, braslist) -> bbeg;
                subscrintvec(c, braelist) fi_- bbeg -> bnchars;
                subscrv(c, bravlist) -> bstr;
                Getrange(0);
                R_+= 2;
                fast_repeat low times
                    unless Strncmp(bbeg, bnchars, bstr, strp) ->> strp then
                        return(false);
                    endunless;
                endfast_repeat;
                strp -> savestrp;
                ;;; build a list of possible matches.
                savestrp :: [] -> tmp;
                until (size fi_-1 ->> size) == -1 do
                    if Strncmp(bbeg, bnchars, bstr, strp) ->> strp then
                        strp :: tmp -> tmp;
                    else
                        quitloop;
                    endif;
                enduntil;
                until tmp == [] do
                    sys_grbg_destpair(tmp) -> tmp -> strp;
                    if Advance(strp, regexp) then
                        sys_grbg_list(tmp);
                        return(true);
                    endif;
                enduntil;
                return(false);

            elseif c == CBACK then
                R_++ -> c;
                subscrintvec(c, braslist) -> bbeg;
                subscrintvec(c, braelist) fi_- bbeg -> bnchars;
                subscrv(c, bravlist) -> bstr;

                nextif(Strncmp(bbeg, bnchars, bstr, strp) ->> strp);
                return(false);

            elseif c == #_< CBACK || STAR >_# then
                ;;; see also comment on CBACK || RNGE above.
                R_++ -> c;
                subscrintvec(c, braslist) -> bbeg;
                subscrintvec(c, braelist) fi_- bbeg -> bnchars;
                subscrv(c, bravlist) -> bstr;
                strp -> savestrp;

                ;;; build a list of possible matches.
                savestrp :: [] -> tmp;
                while Strncmp(bbeg, bnchars, bstr, strp) ->> strp do
                    strp :: tmp -> tmp;
                endwhile;
                until tmp == [] do
                    sys_grbg_destpair(tmp) -> tmp -> strp;
                    if Advance(strp, regexp) then
                        sys_grbg_list(tmp);
                        return(true);
                    endif;
                enduntil;
                return(false);

            elseif c == #_< CDOT || STAR >_# then
                strp -> savestrp;
                strsize fi_+ 2 -> strp; ;;; one after the end of the string
                goto star;

            elseif c == #_< CCHR || STAR >_# then
                strp -> savestrp;
                *_R -> c;
                unless Case(skipchar, c, strp, str, ignorecase) ->> strp then
                    strsize fi_+ 1 -> strp;
                endunless;
                S_+= 1; ;;; one char after the final match
                R_+= 1;
                goto star;

            elseif c == #_< CXCL || STAR >_# then
                strp -> savestrp;
                fast_repeat;
                    S_++ -> c;
                    quitunless(c /== termin and ISTHERE(c));
                endfast_repeat;
                R_+= 32;
                goto star;

            elseif c == #_< NCCL || STAR >_# then
                true -> neg;
                goto do_ccl_star;

            elseif c == #_< CCL || STAR >_# then
            do_ccl_star:
                strp -> savestrp;
                fast_repeat
                    S_++ -> c;
                    quitif(c == termin or
                        ((c &&=_0 8:200) and ISTHERE(c)) == neg);
                endfast_repeat;
                R_+= 16;
                ;;; FALLTHROUGH to star

            star:
                S_-= 1;
                if strp == savestrp then
                    nextloop;
                endif;

                *_R -> c;
                if c == CCHR or c == CBACK then
                    ;;; fast search for the first character.
                    if c == CCHR then
                        R(1)
                    else
                        R(1) -> c;
                        subscrs(subscrintvec(c, braslist),
                                subscrv(c, bravlist))
                    endif -> c;
                    while (Case(locchar_back, c, strp, str, ignorecase)
                                            ->> strp)
                                and (CHECKTAB_BACK, strp fi_>= savestrp) do
                        if Advance(strp, regexp) then
                            return(true);
                        endif;
                        strp fi_-1 -> strp;
                    endwhile;
                    return(false);
                endif;

                fast_repeat;
                    if Advance(strp, regexp) then
                        return(true);
                    endif;
                    quitunless(strp fi_> savestrp);
                    S_-= 1;
                endfast_repeat;
                return(false);

            elseif c == CBRC then
                ;;; constrain to start of item
                *_S -> c;
                returnif(c == termin)(false); ;;; items cannot start here
                if strp == 1 then  ;;; maybe this should be strp == loc1 ?
                    ;;; first character of string must be the start of an item
                    nextloop;
                elseif usevedchartype then
                    if weakref vedatitemstart(strp, str, strsize) then
                        nextloop;
                    endif;
                else
                    if not(WHITESPACE(c)) then
                        ;;; get the last character
                        S_-= 1; S_++ -> c;
                        if WHITESPACE(c) then
                            nextloop;
                        endif;
                    endif;
                endif;
                return(false);

            elseif c == CLET then
                ;;; constrain to end of item
                *_S -> c;
                if c == termin then
                    nextloop;
                endif;
                if usevedchartype then
                    if strp /== 1 then
                        S_-= 1;
                        if weakref vedatitemend(strp, str, strsize fi_+1) then
                            S_+= 1;
                            nextloop;
                        endif;
                    endif;
                else
                    *_S -> c;
                    if WHITESPACE(c) then
                        nextloop;
                    endif;
                endif;
                return(false);

            elseif c == CCASE then
                false -> ignorecase;
                identfn -> filter_p;
                nextloop;

            elseif c == NCCASE then
                true -> ignorecase;
                uppertolower -> filter_p;
                nextloop;

            else
                ;;; this should never happen but just in case ...
                mishap(c, 1, 'INVALID REGULAR EXPRESSION');
            endif;

        endfast_repeat;
    enddefine; /* Advance */

    ;;; Do_search - calls Advance to perform a search
    define lconstant Do_search;
        lvars regexp, minlength, anchor, startstrp, strlen;

        ;;; PROCESS ARGUMENTS - strp, str, strsize,
        if str.isword then
            fast_word_string(str) -> str;
        elseunless str.isstring then
            mishap(str,1,'STRING NEEDED')
        endif;
        if usevedindentstep then
            weakref vedusedsize(str)
        else
            datalength(str)
        endif -> strlen;
        unless strlen == 0 then
            unless strp.isinteger and strp fi_>= 1 and strp fi_<= strlen then
                mishap(strp, 1,
                    'INTEGER >= 1 AND <= ' sys_>< strlen sys_>< ' NEEDED');
            endunless;
        elseunless strp == 1 then
            mishap(strp, str, strlen, 3,
                   'SUBSCRIPT EXCEEDS STRING LENGTH');
        endunless;
        if strsize == false then
            strlen -> strsize;
        else
            unless strsize.isinteger and strsize fi_>= 0
            and strsize fi_<= strlen fi_- strp fi_+ 1 then
                mishap(strsize, 1,
                    'INTEGER >= 0 and <= ' sys_>< (strlen - strp + 1)
                    sys_>< ' NEEDED');
            endunless;
            strsize fi_+ strp fi_- 1 -> strsize;
        endif;
        if backward == 0 then
            ;;; can never match
            return(false ->> loc1 -> loc2);
        elseif backward and not(circf) then
            strp -> startstrp;
            if backward.isinteger then
                ;;; backward specifies where to start searching
                unless backward fi_>= strp fi_-1 then
                    mishap(backward, 1,
                        'INTEGER >= ' sys_>< (strp -1) sys_>< ' NEEDED');
                endunless;
                fi_min(backward, strsize fi_+1) -> strp;
            else
                strsize fi_+ 1 -> strp;
            endif;
        endif;
        ;;; set per-line data
        explode(fast_destpair(linedata) -> linedata) -> (regexp, minlength);

        ;;; quick check to see if we have enough characters.
        returnif(strsize fi_< minlength)(false ->> loc1 -> loc2);

        define lconstant Set_jumptabs;
            ;;; see if we're using vedindentstep and string has hard tab in it.
            if usevedindentstep and locchar(`\t`, 1, str) ->> jumptabs then
                if backward then
                    Jump_over_tab(str, strp, false) -> strp;
                    Jump_over_tab(str, startstrp, false) -> startstrp;
                else
                    Jump_over_tab(str, strp, true) -> strp;
                endif;
            endif;
        enddefine;

        ;;; PERFORM SEARCH
        ;;; anchored left search (same for left-to-right and right-to-left).
        if circf then
            Set_jumptabs();
            if strp == 1 and Advance(strp, regexp) then
                strp -> loc1;
                loc2 fi_- loc1 -> loc2;
                return;
            endif;

        ;;; fast search when we know the first character.
        elseif firstchar then
            firstchar -> c;
            if backward then
                ;;; fast right-to-left search.
                if (Case(locchar_back, c, strp, str, ignorecase) ->> strp) then
                    Set_jumptabs();
                    repeat;
                        if Advance(strp, regexp) then
                            strp -> loc1;
                            loc2 fi_- loc1 -> loc2;
                            return;
                        endif;
                        strp fi_- 1 -> strp;
                    quitunless(
                        (Case(locchar_back, c, strp, str, ignorecase) ->> strp)
                        and (CHECKTAB_BACK, strp fi_>= startstrp)
                      );
                    endrepeat;
                endif;
            else
                ;;; fast left-to-right search.
                if (Case(locchar, c, strp, str, ignorecase) ->> strp) then
                    Set_jumptabs();
                    repeat;
                        if Advance(strp, regexp) then
                            strp -> loc1;
                            loc2 fi_- loc1 -> loc2;
                            return;
                        endif;
                        strp fi_+ 1 -> strp;
                    quitunless(
                        (Case(locchar, c, strp, str, ignorecase) ->> strp)
                        and (CHECKTAB, strp fi_<= strsize)
                      );
                    endrepeat;
                endif;
            endif;

        ;;; right-to-left searching - regular algorithm.
        elseif backward then
            Set_jumptabs();
            while strp fi_>= startstrp do
                if Advance(strp, regexp) then
                    strp -> loc1;
                    loc2 fi_- loc1 -> loc2;
                    return;
                endif;
                S_-= 1;
            endwhile;

        ;;; left-to-right searching - regular algorithm.
        else
            Set_jumptabs();
            fast_repeat;
                if Advance(strp, regexp) then
                    strp -> loc1;
                    loc2 fi_- loc1 -> loc2;
                    return;
                endif;
                quitif(S_++ == termin);
            endfast_repeat;
        endif;

        return(false ->> loc1 -> loc2); ;;; FAIL
    enddefine; /* Do_search */

    ;;; multi-line matching
    define lconstant Do_long_search();
        lvars startmatch, savestrsize, lines;
        ;;; massage arguments:
        ;;; strp, str, strsize -> (STRP, STR1, STR2, ..., STRSIZE)
        strsize -> savestrsize;
        conslist(strp, str, nlines) -> lines; ;;; list of STR1 STR2 ...
        ;;; FIRST LINE
        ;;; set strp, str and strsize
        -> strp; ;;; off the stack.
        sys_grbg_destpair(lines) -> lines -> str;
        false -> strsize;
        ;;; do search
        Do_search();
        returnunless(loc1 ->> startmatch)(sys_grbg_list(lines));
        true -> circf; ;;; all other lines are constrained to start
        backward and true -> backward; ;;; ignore any integer backward value
        ;;; MIDDLE LINES
        nlines fi_-2 -> nlines;
        fast_repeat nlines times
            ;;; set strp, str and strsize
            1 -> strp;
            sys_grbg_destpair(lines) -> lines -> str;
            false -> strsize;
            ;;; do search
            Do_search();
            returnunless(loc1)(sys_grbg_list(lines));
        endfast_repeat;
        ;;; LAST LINE
        ;;; set strp, str and strsize
        1 -> strp;
        sys_grbg_destpair(lines) -> lines -> str;
        savestrsize -> strsize;
        ;;; do search
        Do_search();
        if loc2 then startmatch -> loc1 endif;
    enddefine; /* Do_long_search */

    usevedindentstep
        and testdef vedindentstep and testdef vedusedsize -> usevedindentstep;
    usevedchartype
        and testdef vedatitemstart and testdef vedatitemend -> usevedchartype;

    if nlines == 1 then
        Do_search()
    else
        Do_long_search()
    endif;
enddefine; /* regexp_search - phew. */

;;; procedure returned if a regular expression cannot be compiled
define regexp_invalid with_props '<invalid regular expression>';
    mishap(0, 'INVALID REGULAR EXPRESSION');
enddefine;


;;; Recogniser

define isregexp(r);
    lvars r;
    r.isclosure and r.pdpart == regexp_search
enddefine;

define Checkr_regexp(r) -> r;
    lvars r;
    unless r.isregexp then
        mishap(r, 1, 'REGULAR-EXPRESSION SEARCH PROCEDURE NEEDED');
    endunless;
enddefine;

;;; Accessors for the PROPS field of regexp_search

;;; the location of the delimeter in the string
define regexp_delimeter(r);
    lvars r;
    subscrv(PROPS_DELIM, frozval(CLOS_PROPS, Checkr_regexp(r)));
enddefine;

;;; the number of sub expressions
define regexp_subexp_count(r);
    lvars r;
    subscrv(PROPS_NBRA, frozval(CLOS_PROPS, Checkr_regexp(r)));
enddefine;

;;; true if the string contains an @a or @z
define regexp_anchored(r);
    lvars r;
    subscrv(PROPS_ANCHORED, frozval(CLOS_PROPS, Checkr_regexp(r)));
enddefine;

;;; returns string and location in string for @( @) matches
define regexp_subexp(n, r);     /* -> (n, len, string) */
    lvars n, r, maxn, loc1;
    regexp_subexp_count(r) -> maxn;
    unless n.isinteger and n >= 1 and n <= maxn then
        mishap(n, 1, 'INVALID SUB-EXPRESSION INDEX');
    endunless;
    subscrintvec(n, frozval(CLOS_BRASLIST, r)) -> loc1;
    ;;; stack the results
    loc1,
    subscrintvec(n, frozval(CLOS_BRAELIST, r)) - loc1,
    subscrv(n, frozval(CLOS_BRAVLIST, r))
enddefine;

define regexp_break_count(r);
    lvars r;
    frozval(CLOS_NBREAKS, Checkr_regexp(r)) - 1;
enddefine;

endsection;     /* $-Sys */
