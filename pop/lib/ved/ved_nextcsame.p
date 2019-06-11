/* --- Copyright University of Birmingham 1996. All rights reserved. ------
 > File:            C.all/lib/ved/ved_nextcsame.p
 > Purpose:         Find next location where this file and other file match
 > Author:          Aaron Sloman, May  9 1993 (see revisions)
 > Documentation:   BELOW, HELP * CDIFF, HELP * VED_NEXTSAME
 > Related Files:   LIB * VED_CDIFF
 */

/* HELP VED_NEXTCSAME

ved_nextcsame                                                [procedure]
ved_nextcsame N
    The command ENTER nextcsame is provided for use in conjunction with
    ved_cdiff. It searches down at most ved_nextcsame_lines (default 6)
    lines in each file, looking for lines that contain at least
    ved_min_char_match (default 15) matching non-space characters,
    where non-space characters are defined by the string ved_cdiff_ignore
    (for which see LIB VED_CDIFF).

    If given an integer argument N, N is assigned to ved_nextcsame_lines
    e.g. to require ved_nextcsame to look further afield for a match.
*/


uses ved_cdiff;     ;;; for ved_cdiff_ignore
section;

compile_mode:pop11 +strict;

global vars
    ved_nextcsame_lines = 6,
    ved_min_char_match = 15;        ;;; At least 15 characters in a line

define lconstant procedure nextc(col, string, len) -> (col,char);
    ;;; given an initial column number, a line of text (string), and the
    ;;; length of the string, return the first column number (>= col) at which
    ;;; there's a character not in ved_cdiff_ignore (i.e. a non-space
    ;;; character), and also return the character.
    ;;; If no non-space character is found return the value false for col.
    ;;; If it started on a space then return column for last char in space

    lvars col, char, string, len, inspace = false;

    fast_for col from col to len do
        fast_subscrs(col, string) -> char;
        if strmember(char, ved_cdiff_ignore) then
            true -> inspace;
        else
            ;;; found a non-space character
            if inspace then ;;; was in a space, record it
                `\s` -> char;
                col fi_- 1 -> col
            endif;
            return();
        endif
    endfor;
    false -> col;
enddefine;

define lconstant procedure nextcol(col, string, len ) -> col ;
    ;;; Like nextc, but return only the column, ignoring spaces
    lvars col, string, len, char;
    repeat
        nextc(col, string, len) -> (col , char);
    quitunless(char == `\s`);
        col fi_+ 1 -> col
    endrepeat
enddefine;


define lconstant procedure sametail(col1,string1,len1,col2,string2,len2) -> matched;
    ;;; Given two triples (column number, string, length of string)
    ;;; check whether there are at least ved_min_char_match non-space
    ;;; characters (i.e. not in ved_cdiff_ignore) to the right of the
    ;;; starting columns in both strings. If so, return true.
    ;;; Otherwise return false
    lvars col1,string1,len1,col2,string2,len2,matched,char1,char2;

    lvars count = 0;

    repeat
        nextc(col1,string1,len1) -> (col1,char1);

        unless col1 then
            ;;; No more chars in string1.
            false -> matched;
            return()
        endunless;

        nextc(col2,string2,len2) -> (col2,char2);

        unless col2 then
            ;;; no more chars in string2.
            false -> matched;
            return()
        endunless;

    returnif( char1 /== char2 )(false -> matched); ;;; mismatch found

        unless char1 == `\s` then
            count fi_+ 1 -> count;  ;;; increment count of non-space matches
        endunless;
        ;;; See if ved_min_char_match characters have matched
    returnif(count fi_>= ved_min_char_match)(true -> matched);

        ;;; keep trying
        col1 fi_+ 1 -> col1;
        col2 fi_+ 1 -> col2;
    endrepeat
enddefine;


define lconstant nextcsame(col1,string1,col2,string2) -> (col1,col2);
    ;;; Starting from those locations try to find positions in the two
    ;;; strings after which there are matching sets of ved_min_char_match
    ;;; characters in the two strings, ignoring "space" characters.
    ;;; If suitable locations are found, return them.
    ;;; If there's no success or if either string is empty,
    ;;; col1 and col2 come out both false.

    lvars col1,string1,col2,string2, startcol2,
        len1 = datalength(string1),
        len2 = datalength(string2);

    returnif(len1 == 0 or len2 == 0)(false ->> col1 ->col2);
    nextcol(col1, string1, len1) -> col1;
    nextcol(col2, string2, len2) -> startcol2;

    repeat
        startcol2 -> col2;

        repeat

        returnif(sametail(col1,string1,len1,col2,string2,len2));

            nextcol(col2 fi_+ 1, string2, len2) -> col2 ;

        quitunless(col2 and col2 fi_+ ved_min_char_match fi_<= len2 fi_+ 1)

        endrepeat;

        nextcol(col1 fi_+ 1, string1, len1) -> col1;
        unless col1 and col1 fi_+ ved_min_char_match <= len1 fi_+ 1 then
            return(false ->> col1 ->col2)
        endunless;
    endrepeat;
enddefine;



define vars ved_nextcsame;
    ;;; Search for a pair of lines that have a common substring of length
    ;;; at least ved_min_char_match, or a "tail" of length
    ;;; Use steadily increasing depth of search up to ved_nextcsame_lines
    ;;; Use numerical argument to change ved_nextcsame_lines

    dlocal vedwarpcontext = false;  ;;; suppress exposures

    lvars
        buffer1, buffer2,           ;;; The two top VED buffers
        linestart1, linestart2,     ;;; Current lines initially
        startcol1, startcol2,       ;;; Current columns initially
        line1, line2,               ;;; Line being tested in each file
        col1, col2,                 ;;; Ditto columns
        lim1, lim2,                 ;;; The line search limits
        string1,string2,            ;;; The two text lines
        lim,                        ;;; Current limit of search
        ;

    ;;; See whether number argument specified
    strnumber(vedargument) -> lim;
    if lim then lim -> ved_nextcsame_lines endif;

    ;;; Set up data-structures and variables for the two files
    vedbuffer -> buffer1;
    if vedcolumn >= vvedlinesize then vednextline() endif;
    vedline -> linestart1;
    vedcolumn ->> startcol1 -> col1;
    vedtrimline();
    vvedbuffersize -> lim1;

    vedswapfiles();
    if vedcolumn >= vvedlinesize then vednextline() endif;
    vedbuffer -> buffer2;
    vedline -> linestart2;
    vedcolumn ->> startcol2 -> col2;
    vedtrimline();
    vvedbuffersize -> lim2;

    vedswapfiles();

    if linestart1 > lim1 then vederror('AT END') endif;
    if linestart2 > lim2 then vederror('AT END OF OTHER FILE') endif;
    min(lim1, linestart1 + ved_nextcsame_lines ) -> lim1;
    min(lim2, linestart2 + ved_nextcsame_lines ) -> lim2;

    ;;; Try pairs of lines in increasing depth from start points
    ;;; (Zig-zagging along diagonals)
    fast_for lim from 0 to ved_nextcsame_lines*2 do
        linestart2 fi_+ lim -> line2;
        linestart1 -> line1;
        startcol1 -> col1;
        repeat

            unless line1 fi_>= lim1 or line2 fi_>= lim2 then
;;;; veddebug([lines ^line1 ^line2 cols ^col1 ^col2]><'');

                fast_subscrv(line1, buffer1) -> string1;
                fast_subscrv(line2, buffer2) -> string2;

;;;; veddebug([string1 ^string1]>< '');
;;;; veddebug([string2 ^string2]>< '');

                nextcsame(col1,string1,col2,string2) -> (col1,col2);

                if col1 and col2
                then
                    ;;; Found locations. Set cursor location in both files
                    vedjumpto(line1,col1);
                    vedswapfiles();
                    vedjumpto(line2,col2);
                    dlocal vedwiggletimes = 4;
                    vedwiggle(line2,col2);
                    vedswapfiles();

                    ;;; now set up cdiff command
                    dlocal ved_on_status = true;
                    lconstant cdiff_command = 'cdiff';
                    vedcharup();
                    if vedthisline() = cdiff_command then
                        vedlinedelete();
                    endif;
                    false -> ved_on_status;
                    vedputcommand('cdiff');
                    vedputmessage('PRESS "REDO" TO GO ON SEARCHING');
                    return();
                endif
            endunless;

            line1 fi_+ 1 -> line1;
            if line1 == linestart1 then startcol1 else 1 endif-> col1;
            line2 fi_- 1 -> line2;
            if line2 == linestart2 then startcol2 else 1 endif-> col2;
        quitif(line2 fi_< linestart2  or line1 fi_> lim1)
        endrepeat
    endfast_for;

    vedputmessage('NO COMMON SUBSTRING FOUND WITHIN '
                    sys_>< ved_nextcsame_lines sys_>< ' LINES');
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 22 1996
        Installed at Sussex.
 */
