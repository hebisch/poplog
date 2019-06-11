/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_g.p
 > Purpose:        Moving between index and text in help files
 > Author:         Aaron Sloman, Apr  6 1986 (see revisions)
 > Documentation:  HELP * ENTER_G
 > Related Files:  LIB * VED_INDEXIFY  LIB * VED_HEADING
 */
compile_mode :pop11 +strict;

section $-ved => ved_g;

lconstant header_types = newproperty([],10,false,"tmparg");


    /* Also used in ved_newindex */
define is_new_indexline(string);
    lvars n, c, numstart, textstart, dot, used, string, l, check_sp = false;

    if (skipchar(`\s`,1,string) ->> numstart)
    and (fast_lmember(numstart, [2 [7] 3 [7] 7 [13] 13 [17]]) ->> l)
    and (vedusedsize(string) ->> used) >= (l.tl.hd.hd ->> textstart)
    then
        fast_subscrs(numstart,string) -> c;
        if isnumbercode(c) then
            numstart == 7 and `.` -> dot;
            numstart -> n;
            while n < textstart-1 do
                n+1 -> n;
                if (fast_subscrs(n,string) ->> c) == dot then
                    `0` -> c, false -> dot
                endif;
                unless isnumbercode(c) then
                    true -> check_sp;
                    quitloop
                endunless
            endwhile
        elseif numstart == 13 and c == `.`
        and issubstring_lim('...\s',numstart,numstart,false,string)
        then
            16 -> n, true -> check_sp
        endif
    endif;
    if check_sp and skipchar(`\s`,n,string) == textstart then
        ;;; it is
        substring(numstart,n-numstart,string) <> '\s\s'
                        <> substring(textstart,used-textstart+1,string)
    else
        false
    endif
enddefine;

    /* Also used in ved_newindex */
define find_new_indexline() -> string;
    lvars line, start = vedline, string = false;
    if vedcolumn > vvedlinesize then start+1 -> start endif;
    for line from start to datalength(vedbuffer) do
        if is_new_indexline(subscrv(line,vedbuffer)) ->> string then
            vedjumpto(line, 1);
            return
        endif
    endfor
enddefine;

define vars ved_g();
    lvars   col, loc, line, string, header_type, last_header, typevec,
            underline, new_style, hyphen = `-`, atindex=undef, text, num;
    dlocal  ved_g_string, vedpositionstack;

    unless header_types(ved_current_file) ->> typevec then
        ;;; set up mappings from strings to header types of the file
        {% newmapping([],2,false,true), newmapping([],2,false,true), false %}
                        ->> typevec -> header_types(ved_current_file)
    endunless;
    explode(typevec) -> (header_type, last_header, new_style);


    define lconstant check_search(string, err, message);
        lvars string, err, message, line;
        ;;; search and complain if necessary
        if vedteststartsearch(string) ->> line then
            vedjumpto(line,1);
            return(unless err then true endunless)
        endif;
        false -> last_header(ved_g_string);
        if err then
            vederror(err sys_>< message)
        else
            false
        endif
    enddefine;

    define lconstant findindex();
        lvars line, start;
        unless new_style then
            returnif(check_search('\s' sys_>< ved_g_string, false, nullstring));
        endunless;
        ;;; try for new-style indexline
        if find_new_indexline() ->> string then
            true ->> new_style -> subscrv(3,typevec)
        else
            vederror('NO INDEX')
        endif
    enddefine;

    unless vedargument = nullstring then
        vedargument sys_>< space -> ved_g_string;
    endunless;

    if vedargument = nullstring then
        header_type(ved_g_string)
    else
        "+"
    endif -> underline;
    unless underline then
        ;;; first time in this file. Work out type
        vedpositionpush();
        vedtopfile();
        findindex();
        if new_style then
            "-"
        else
            ;;; Found first index entry. Check whether one or two spaces after
            ;;; ved_g_string (Altered Jul.1.87 AS. Still not general enough)
            if vedthisline()(datalength(ved_g_string)+2) == `\s` then
                "-"
            else
                "+"
            endif
        endif ->> underline -> header_type(ved_g_string);
        vedpositionpop();
    endunless;
    if not(new_style) and underline = "-" then
        ;;; headers don't start with '-- '. e.g. they may be underlined
        if ved_g_string = '--\s' then '--\s\s'
        else ved_g_string sys_>< '\s'
        endif -> ved_g_string
    endif;


    vedpositionpush();
    vedthisline() -> line;

    if new_style then
        if is_new_indexline(line) ->> string then
            true -> atindex;
            copy(line) -> last_header(ved_g_string);
        endif
    elseif issubstring_lim(ved_g_string,1,2,false,line) ->> loc then
        ;;; found ved_g_string in position 1 or 2 on current line
        if loc /== 1 and line(1) == `\s` then true
        elseif loc == 1 then false
        else undef ;;; should not happen
        endif -> atindex;
    endif;

    unless isboolean(atindex) then
        if last_header(ved_g_string) ->> string then
            ;;; find last table entry and go to next one
            false -> atindex;
        endif
    elseunless new_style then
        ;;; At index entry or section header.
        ;;; Find end of title, i.e. before trailing hyphens (optional)
        skipchar_back(hyphen,vvedlinesize,line) -> col;
        ;;; ignore spaces
        skipchar_back(`\s`,col,line) + 1  -> col;
        if atindex and underline == "-" then
            ;;; it is a file with underline headers
            ;;; use entry after ' --  ' as search string
            6 -> loc;
        endif;
        ;;; get the section title to search for
        substring(loc, col fi_- loc, line) ->string;
        if atindex then string -> last_header(ved_g_string) endif;
        vedtextright();     ;;; ensure that this line is not found again
    endunless;

    ;;; Preparation over. Now go to required place
    if atindex = undef then
        ;;; first time on this file - find first table entry
        vedtopfile();
        findindex()
    elseif atindex then
        ;;; on index entry - search for section header
        check_search(string, 'No section headed: ', string);
    else
        ;;; On section header or in text.
        ;;; Go to index entry, then move to next one.
        vedtopfile();
        unless new_style then
            if underline = "-" then
                ;;; need to doctor string
                ved_g_string sys_>< string -> string
            endif;
            '\s' sys_>< string -> string
        endunless;
        check_search(string, 'NO entry for: ', string);
        ;;; now go to next index entry
        vedtextright();
        findindex();
    endif;

    unless vedcursorset() then
        ;;; for terminals that scroll too slowly
        max(0,vedline - (vedwindowlength div 2)) -> vedlineoffset;
        vedrefresh();
    endunless;
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 30 1995
        Fixed test in is_new_indexline to be >= textstart not >
--- John Gibson, May 21 1993
        ... header lines now have 2 spaces between the ... and the text
--- John Gibson, Feb  6 1993
        Added support for new-style numbered section headers
--- Aaron Sloman, Jun 22 1991
        Generalised to allow argument to determine ved_g_string
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
--- Aaron Sloman, Oct 26 1988
    Altered to allow VED search patterns in ved_g_string, by not using
    the VED search mechanism. Instead use vedteststartsearch.
--- Bill Keller, Jun 21 1988
    Corrected VEDINDEXIFY to VED_INDEXIFY in Related Files section.
*/
