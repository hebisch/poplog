/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_grep.p
 > Purpose:         Run grep and read output into a VED file
 > Author:          Aaron Sloman, Oct 14 1990 (see revisions)
 > Documentation:   HELP * VED_GREP
 > Related Files:
 */

/*
  <ENTER> grep [<flags>] <search-string> file1, file2 ...
  <ENTER> grep [<flags>] <search-string> <file-pattern>
  <ENTER> grep [<flags>] <file-pattern>
  <ENTER> grep [<flags>]
*/

section;

uses-by_name vedgenshell;

/*USER-MODIFIABLE DEFAULTS*/

global vars
        grep_search_command = 'grep',
        grep_leave_colons;

global vars show_output_on_status;  ;;; used by vedgenshell


/* UTILITIES */

define lconstant explode_substring(i1, i2, string);
    ;;; Put the characters between i1 and i2 on the stack.
    ;;; If i2 is false, put all characters to end of string on stack
    lvars x, i1, i2, string, len = datalength(string);


    check_string(string);
    checkinteger(i1, 1, len);

    if i2 then
        checkinteger(i2, 1, len);
    else
        len -> i2;
    endif;

    fast_for x from i1 to i2 do
        fast_subscrs(x, string)
    endfor;
enddefine;

define lconstant trim_flags(args) -> args;
    ;;; If there are any flags (indicated by `-`) put the characters
    ;;; of the leading substring on the stack.
    ;;; Return a string with remaining args

    lvars args,
        flags = false,
        n = 1,
        nextspace = false;

    ;;; Put all 'flag' arguments on stack as characters.
    while subscrs(n, args) == `-` do
        true -> flags;  ;;; indicate flag found
        ;;; found `-` so search for space after flag
        locchar(`\s`, n, args) -> nextspace;
    quitunless(nextspace);
        ;;; Find start of next arg, i.e. after space
        skipchar(`\s`, nextspace, args) -> n;
    quitunless(n);
    endwhile;

    if nextspace then
        ;;; found a space after the last flag arg
        ;;; dump all characters up to that point on the stack.
        explode_substring(1, nextspace, args);
        `\s`;
        if n then
            allbutfirst(n - 1, args) -> args;
        else
            ;;; only following spaces. (Should not be possible)
            nullstring -> args;
        endif
    elseif flags then
        ;;; no following space, so it is all flags
        explode(args), `\s`;
        nullstring-> args;
    else
        ;;; no flags found, args unchanged
    endif;

enddefine;

define lconstant quotify(arg, index);
    ;;; Arg is a string with at least one space, at location index.
    ;;; Put the first space-bounded text between quotes (').
    ;;; Put all remaining characters on the stack
    lvars arg, index;

        `'`,    ;;; opening string quote
        explode_substring(1, index - 1, arg);
        `'`;        ;;; closing string quote

        explode_substring(index, false, arg);
enddefine;

define lconstant next_string_in_line() -> string;
    ;;; get space/tab delimited string to right of cursor,
    ;;; excluding trailing . or ,.
    ;;; Uses the mechanism employed by vedexpandchar, in SRC * vdprocess.p
    lvars string;
    pdpart(vedexpandchars(`f`))('\s\t\n();,[]{}|><~&*?`\'"', '.,') -> string;
enddefine;

define global vars procedure ved_grep;
    lvars index, oldfile = vedcurrentfile;

    dlocal show_output_on_status = false, vedargument;

    ;;; Save vedsearch state
    dlocal ved_search_state;

    unless isboolean(grep_leave_colons) then
        false -> grep_leave_colons
    endunless;

    ;;; now create second argument for vedgenshell

    consstring(#|
        explode(grep_search_command), `\s`,
        if vedargument = nullstring then
            ;;; use the whole of the current VED line
            explode(vedthisline())
        else
            ;;; get any grep" flags" from vedargument
            trim_flags(vedargument) -> vedargument;

            if vedargument = nullstring then
                ;;; use the whole of the current VED line
                explode(vedthisline())
            elseif (strmember(`\s`, vedargument) ->> index) then
                ;;; There is a search string. Put it between "'" quotes
                ;;; to be safe, unless already there.
                if fast_subscrs(1,vedargument) == `'` then
                    explode(vedargument)
                else
                    ;;; insert quotes around initial arg
                    quotify(vedargument, index)
                endif
            else
                ;;; DODGY !!!
                ;;; no searchstring argument. So get it from current line.
                ;;; remainder of vedargument is the file spec.
                explode(next_string_in_line()), `\s`, explode(vedargument)
            endif

        endif
        |#) -> vedargument;

    valof("vedgenshell") (systranslate('SHELL') or '/bin/sh', vedargument);

    ;;; Now prepare file so that filenames are delimited by spaces

    unless oldfile == vedcurrentfile or grep_leave_colons then
        ;;; get rid of first colon in each line
        dlocal vedstatic = true;
        lblock lvars line, loc;
            for line from 1 to vvedbuffersize do
                if locchar(`:`, 1, subscrv(line,vedbuffer)) ->> loc then
                    vedjumpto(line, loc);
                    veddotdelete();
                endif
            endfor
        endlblock;

        vedjumpto(1,1)

    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 12 2005
        Imported some improvements from Bham version, including
        wrapping search arg in quotes.
--- Aaron Sloman, Jan 30 1999
    Got rid of test for ngrep
--- Aaron Sloman, 29 Dec 1997
    Changed so as not to insert quotes if already there.
--- John Williams, Jan  7 1997
        Now uses $SHELL if set (/bin/sh otherwise), as per BR joew.12.
--- Aaron Sloman, Oct 15 1995
        Fixed bug to do with indexing into strings.
--- Aaron Sloman, Oct 14 1995
        Changed default command back to egrep, since it offers more
        flexibility
        Renamed analyse_argument as trim_flags, and introduced
        explode_substring.
--- Aaron Sloman, Sep 3 1995
        Changed to use 'grep' if 'ngrep' is not available, since egrep
        can be slower
--- John Gibson, Aug  2 1995
        Made initialisation of grep_search_command be done at runtime
--- Jonathan Meyer, Sep 29 1993
        Changed vvedsr*ch vars to ved_search_state
--- John Williams, Aug  4 1992
        Made -ved_grep- global (cf BR isl-fr.4461)
--- Aaron Sloman, Aug 18 1992
    changed to use ngrep for local birmingham version
--- Aaron Sloman, Oct 21 1990
    Restricted test for 'ggrep' to sussex
--- Aaron Sloman, Oct 16 1990
    Completely replaced and generalised previous version. Provided
    HELP file
 */
