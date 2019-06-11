/*  --- Copyright University of Sussex 2005.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_indexify.p
 > Purpose:        Create a table of contents for a help or teach file
 > Author:         Aaron Sloman, Apr 11 1986 (see revisions)
 > Documentation:  HELP * ENTER_G, HELP * VEDCOMMS /indexify
 > Related Files:  LIB * VED_G ,  LIB * VED_HEADING, LIB * VEDTESTSTARTSEARCH
 */
compile_mode :pop11 +strict;

/*
Insert a table of contents suitable for ved_g into current file.
Assumes sections have standard POPLOG headers.

If existing table exists it is updated. If it doesn't exist, it is assumed
that the table of contents should go after the current line.

If it can't find a line starting with '-- ' OR if it is called with an
argument <string> e.g.

    <ENTER> indexify -

it assumes that headers are followed by a line of hyphens
e.g.

Section 1
---------

Otherwise it assumes headers look like this:

-- Section 1 ------------------------------------------------------
<text 1>
-- Section 2 ------------------------------------------------------
<text 2>

In either case it produces a table of contents e.g.

         CONTENTS - (Use <ENTER> g to access sections)

 -- Section 1
 -- Section 2

except that if underlined headings are used, then entries have two
spaces after '--', to help  VED_G know what to search for.
*/

section;

uses ved_g;

vars ved_g_header;
unless isstring(ved_g_header) then
'         CONTENTS - (Use <ENTER> g to access required sections)'
    -> ved_g_header;
endunless;

;;; A propertly mapping versions of ved_g_string onto index headers
lconstant headers = newmapping([],16,false,false);

define vars ved_indexify;
    lvars num, line, col, list,
         oldchanged=vedchanged, thisline=vedline,
         underline, indexstring;
    dlocal
        vvedmarkprops, vedpositionstack, ved_g_string, ved_g_header,
        vedautowrite=false;

    define lconstant trysearch(string) -> line;
        lvars string, line;
        if vedteststartsearch(string) ->> line then
            vedjumpto(line,1)
        endif
    enddefine;


start:
    if vedargument = nullstring then
        false,
        ' ' sys_>< ved_g_string
    elseif vedargument = '-' then
        vedputmessage('LOOKING FOR UNDERLINED HEADERS');
        '--' -> ved_g_string;
        ' --  ',
        ' --  '
    else
        vedargument /* sys_>< space */ -> ved_g_string;
        false,
        ' ' sys_>< ved_g_string
    endif  -> indexstring ->underline;

    unless trysearch(ved_g_string)then
        if underline or vedargument = '-' then
            vederror('NO SECTION HEADERS')
        elseif vedargument = nullstring then
            '-' -> vedargument; goto start
        else
            vederror('NO SECTION HEADERS')
        endif
    endunless;

    thisline -> vedline;
    vedpositionpush();
    vedmarkpush(); false -> vvedmarkprops; vedmarkpush();
    ;;; remove existing index entries if they exist
    vedtopfile();
    if trysearch(indexstring) then
        ;;; Found index. Delete it, but leave bits that have
        ;;; had other text inserted.
        vedmarklo(); vedline -> thisline;
        repeat
            vednextline();
        quitunless(
            vvedlinesize == 0 or isstartstring(indexstring, vedthisline()));
            vedmarkhi();
        endrepeat;
        ved_d();
        if trysearch(indexstring) then
            vedputmessage('PORTIONS OF INDEX CONTAINING OTHER TEXT LEFT..');
        endif;
        vedjumpto(thisline,1);
        if vvedlinesize /== 0 then vedlineabove(); vedcharup(); endif;
    elseif trysearch(ved_g_header) then
        vedlinebelow();
    else
        ;;; Assume header should go below current line
        thisline -> vedline; vedsetlinesize();
        vedlinebelow(); copy(ved_g_header) -> vedthisline();
        vedrefreshrange(vedline,vedline,false);
        vedlinebelow();
    endif;
    vedpositionpush();
    ;;; collect header lines
    vedtopfile();
    [% fast_for num from 1 to vvedbuffersize do
             fast_subscrv(num, vedbuffer) -> line;
             if issubstring_lim(ved_g_string,1,1,false,line) then
                 ;;; section title found
                 if underline then
                    ;;; only a row of hyphens
                    unless skipchar(`-`,1,line) then
                     ;;; use previous line
                     fast_subscrv(num fi_- 1, vedbuffer);
                    endunless;
                 else
                     ;;; Find end of title, i.e. before trailing spaces or hyphens
                     skipchar_back(` `,datalength(line),line) -> col;
                     skipchar_back(`-`,col,line) -> col;
                     ;;; ignore spaces
                     skipchar_back(` `,col,line)   -> col;
                     ;;; get the lead-in plus section title
                     subdstring(1, col, line)
                 endif
             endif
         endfast_for %] -> list;

    ;;; Suppress printing of whole new index
    dlocal vedediting;
    false -> vedediting;
    vedpositionpop();vedpositionpush();
    underline or vedspacestring -> indexstring;
    fast_for line in list do
        vedlinebelow();
        indexstring sys_>< line -> vedthisline();
;;;     vedrefreshrange(vedline,vedline,false)
    endfast_for;
    vednextline();
    unless vvedlinesize == 0 then vedlineabove() endunless;
    vedmarkpop(); vedpositionpop();
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
    chain(vedalignscreen)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 12 2005
        Added 'uses ved_g'
--- Adrian Howard, Nov  8 1991 : Removed the other occurances of "-" so the
        library would work...
--- John Gibson, Nov  7 1991
        Removed nonsensical code for changing CONTENTS header if
        vedargument /= nullstring. Fixed bug where vedargument was being
        tested == "-" instead of = '-'.
--- Aaron Sloman, Jun 22 1991
    Changed to allow an argument string
--- Aaron Sloman, Jun 25 1990 reduced unnecessary screen printing
--- Aaron Sloman, Jan 14 1989 removed vednullstring
--- Aaron Sloman, Dec 24 1988
    Altered not to delete bits of index with interspersed additional text.
--- Aaron Sloman, Oct 26 1988
    Altered to use vedteststartsearch so that ved_g_string can contain
    VED search pattern elements.
--- Aaron Sloman, Nov 13 1986 altered to cope with underlined headers.
See comment at top
*/
