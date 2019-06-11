/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_newheading.p
 > Purpose:         Insert new-style section header in HELP or REF file
 > Author:          John Williams, May 21 1993 (see revisions)
 > Documentation:   REF * VEDCOMMS
 > Related Files:   LIB * VED_NEWINDEX
 */

compile_mode :pop11 +strict;
section;


uses ved_chat;


/*
 * Returns -true- if -string- consists only of the character -char-
 */
define lconstant Is_made_of(string, char) /* -> BOOL */;
    lvars string, char, x;
    returnif(string.datalength==0)(false);
    for x in_string string do;
        returnunless(x == char)(false);
    endfor;
    true;
enddefine;


/*
 * Returns -true- if the current line contains a heading, -false- otherwise
 */
define lconstant Is_heading();
    lvars line = vedthisline(),
        next_line = vedline < vvedbuffersize and vedbuffer(vedline+1) or ''
    ;
    isstartstring('-- ', line) or line.datalength > 0
            and (Is_made_of(next_line, `-`) or Is_made_of(next_line, `\G-`))

enddefine;


/*
 * Returns the type of heading on the current line:
 *    i) The word "subheading" for subheadings
 *   ii) The pair conspair(MAJORNUM, MINORNUM) for minor headings
 *  iii) The integer MAJORNUM for major headings
 * A MAJORNUM of 0 is returned if the type of the header cannot be found.
 */
define lconstant Heading_type() -> kind;
    lvars kind, line = vedthisline();
    vedscreenleft();
    if isstartstring('... ', line) then
        "subheading" -> kind
    elseif (vednextitem() ->> kind).isinteger or kind.isdecimal then
        unless kind.isinteger do;
            until vedcurrentchar() == `.` do
                vedcharright();
            enduntil;
            vedcharright();
            conspair(intof(kind), vednextitem()) -> kind;
        endunless;
    else
        0 -> kind;
    endif;
enddefine;


/*
 * Returns the last major/minor heading -num- (0 if no previous headings)
 * -subheadings- is -true- if there are also interveaning sub-headings
 */
define lconstant Last_heading_num() -> (num, subheadings);
    lvars num = 0, subheadings = false, line, kind;
    dlocal vedcolumn, vedline, vvedlinesize;
    until vedline == 1 do;
        if isstartstring('\G-\G-\G-', vedthisline() ->> line) then
            vedcharup();
            Heading_type() -> kind;
            if kind == "subheading" then
                true -> subheadings;
            elseunless kind == 0 do
                kind -> num;
                quitloop;
            endif;
        else
            vedcharup();
        endif;
    enduntil;
enddefine;


/*
 * Remove underlining, overlining & type indicators from a heading.
 */
define lconstant Strip_heading();

    /* Delete hyphens from old heading (if present) */
    if isstartstring('-- ', vedthisline()) then
        vedscreenleft();
        vedwordrightdelete();
        vedtextright(); vedcharleft();
        while vedcurrentchar() == `-` and vedcolumn > 1 do
            vedcharleft()
        endwhile;
        vedcharright(); vedcleartail()
    endif;

    /* Remove major/minor/subheading indicators (if present) */
    vedscreenleft();
    while strmember(vedcurrentchar(), '. 0123456789') and vvedlinesize > 0 do
        veddotdelete();
    endwhile;

    /* Delete any under/overlining from old heading (if present) */
    if vedline > 1 then
        vedcharup();
        if Is_made_of(vedthisline(), `\G-`) then
            vedlinedelete();
        else
            vedchardown();
        endif;
    endif;
    vedchardown();
    lvars line = vedthisline();
    if Is_made_of(line, `\G-`) or Is_made_of(line, `-`) then
        vedlinedelete();
    endif;
    vedcharup();

enddefine;


/*
 * Convert the current line into a heading. -style- is "ref" or "help"
 */
define lconstant Make_heading(style);
    lvars style,
        kind = Heading_type(),
        last_num, subs;
    dlocal vedbreak = false, vedleftmargin = 0, vedstatic = false;

    Last_heading_num() -> (last_num, subs);
    Strip_heading();

    /* If current line has no type, use the last one */
    if kind == 0 then
        if subs then "subheading" else last_num endif -> kind;
    endif;

    /* Insert section number, also note whether it's a sub-heading */
    vedscreenleft();
    if kind == "subheading" then
        vedinsertstring('...');
    elseif kind.ispair then
        if isinteger(last_num) then
            conspair(last_num, 1) -> kind;
        else
            last_num -> kind;
            if (kind.back+1 ->> kind.back) > 99 then
                vederror('MORE THAN 99 MINOR HEADINGS');
            endif;
        endif;
        vedinsertstring(kind.front sys_>< nullstring);
        vedcharinsert(`.`);
        vedinsertstring((kind.back) sys_>< nullstring);
    else
        if isinteger(last_num) then
            last_num+1 -> kind;
        else
            last_num.front+1 -> kind;
        endif;
        if kind > 99 then
            vederror('MORE THAN 99 MAJOR HEADINGS');
        else
            vedinsertstring(kind sys_>< nullstring);
        endif;
    endif;
    vedinsertstring('  ');

    /* Ensure major headings are bold */
    lvars line_chr;
    if isinteger(kind) then
        veddo('chat l +b');
        vedprevline();      ;;; chat l leaves cursor at beginning of next line
        `\{b}\G-` -> line_chr;
    else
        `\G-` -> line_chr;
    endif;

    /* Calculate length of under/overlining */
    lvars len =
        if style == "ref" or not(kind.isinteger) then
            vvedlinesize
        else
            71
        endif;

    /* First line of G- */
    if kind.isinteger do
        vedlineabove();
        repeat len times vedcharinsert(line_chr) endrepeat;
        vednextline();
    endif;

    /* Second line of G- */
    vedlinebelow();
    repeat len times vedcharinsert(line_chr) endrepeat;
    vednextline();

enddefine;


/*
 * <ENTER> newheading [help|ref] [r]
 */
define vars ved_newheading();
    dlocal vedwriteable = false, vedleftmargin=0, vedlinemax=72;
    lvars args = sysparse_string(vedargument),
        style = "help";

    if (vedfileprops == "ref" or issubstring('/ref/', vedpathname)
        or member('ref', args)) and not(member('help', args))
        or vvedbuffersize > 0 and isstartstring('REF', vedbuffer(1))
    then
        "ref" -> style
    endif;

    if member('r', args) then
        vedtopfile();
        until vedatend() do;
            if Is_heading() then
                Make_heading(style);
            else
                vedchardown();
            endif;
        enduntil;
    else
        Make_heading(style);
    endif;

enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 17 1993
        Checks for "REF" at the start of the file as an extra type check
        for the heading type
--- Adrian Howard, Jun 15 1993
        Now works if margins changed from normal
--- Adrian Howard, Jun  6 1993
        Now works when old "-- " style headings have no trailing hyphens
--- Adrian Howard, Jun  2 1993
        Fixed some buggettes, added renumber function, and allowed minor
        section numbers above nine.
 */
