/*  --- Copyright University of Sussex 1997.  All rights reserved. ---------
 >  File:           C.all/lib/ved/page.p
 >  Purpose:        Paginate files in VED
 >  Author:         David Roberts, Nov 1982 (see revisions)
 >  Documentation:  HELP * PAGE
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

section $-library => ved_pagesize ved_header ved_footer ved_page ved_autopage
    ved_unpage rno_bp rno_tp rno_error checkbottom popformfeed page
    topgap headgap footgap aligntop;

section page => ved_pagesize ved_header ved_footer ved_page ved_autopage
    ved_unpage rno_bp rno_tp rno_error checkbottom popformfeed page
    topgap headgap footgap aligntop;

/* to make 'USES' work properly */
global vars page; if isundef(page) then undef -> page endif;

/*
USER-DEFINABLE PROCEDURE
CHECKBOTTOM is called just before footer and form feed are inserted.
It can be defined by the user to make the page boundary change.
*/

vars procedure checkbottom;
identfn -> checkbottom;

/*
CONTANTS, VARIABLES AND DEFAULT VALUES
 */

vars popformfeed;  /* contains form feed character: users may assign some
                      other string */
unless isstring(popformfeed) then
    '\^L' -> popformfeed
endunless;

vars
maxpagesize ;;; maximum number of printable lines on page that the printer can
            ;;;     handle (check your printer!)
pagesize    ;;; number of printable lines on page
            ;;;     (set by ved_pagesize(); defaults to maxpagesize)
topgap      ;;; number of blank lines to be left at top of page
headgap     ;;;    "    "   "     "    "  "   "  below headline
            ;;;                                         (if there is one)
footgap     ;;;    "    "   "     "    "  "   "  above footline
            ;;;                                         (if there is one)
headtext    ;;; text of headline if there is one
foottext    ;;;   "   " footline  "   "    "  "
pagenum     ;;; number of current page
nextpage;

66          -> maxpagesize;
maxpagesize -> pagesize;
false       -> topgap;      ;;;
false       -> headgap;     ;;; these are assigned their values in obey_page()
false       -> footgap;     ;;;
0           -> pagenum;
false       -> nextpage;

vars
breakline      ;;; number of line after last textline on a page
_oldchanged    ;;; saves state of vedchanged
at_page_top    ;;; boolean: true if vedline is first line of a page
aligntop ;;; boolean: true if first non-blank line is to be made first
               ;;;          line on page
top_blanks_removed ;;; number of blank lines deleted in order to align page top
never_ask      ;;; boolean: true if autopaging
dont_ask;      ;;; boolean: true if processing a rno command

true -> aligntop;
0 -> top_blanks_removed;

vars procedure insert_blank_lines rno_error make_page obey_page ved_page;

/*
VED_PAGESIZE
 */

define global ved_pagesize();
    vars num;
    if (strnumber(vedargument) ->> num)
            and num /= 0 and abs(num) <= maxpagesize then
        if num < 0 then
            (maxpagesize + num) div 2
        else
            0
        endif -> topgap;
        topgap + abs(num)
    else
        1 -> num;
        maxpagesize
    endif -> pagesize;
    unless num < 0 then
        vedputmessage('PAGESIZE IS ' sys_>< pagesize sys_>< ' LINES')
    else
        vedputmessage('PAGESIZE IS ' sys_>< - num sys_>< ' LINES CENTRED')
    endunless
enddefine;

/*
HEADERS AND FOOTERS:
user uses VED_HEADER and VED_FOOTER to assign to headtext and foottext;
DECOMPOSE breaks up VEDARGUMENT into left, middle, and right portions
 */

false ->> headtext -> foottext;

define decompose(string) -> rubrictext;
    vars index;
    {%'', '', ''%} -> rubrictext;
    1 -> index;
    appdata(string,
        procedure(char);
            if char == `<` then
                2 -> index
            elseif char == `>` then
                3 -> index
            else
                rubrictext(index) sys_>< consstring(char,1) -> rubrictext(index)
            endif
        endprocedure)
enddefine;

define global ved_header();
    unless vedargument = '' then
        decompose(vedargument)
    else
        false
    endunless -> headtext
enddefine;

define global ved_footer();
    unless vedargument = '' then
        decompose(vedargument)
    else
        false
    endunless -> foottext
enddefine;

/*
CONVERTCHARS converts 'special' characters # and % in headtext and foottext
to pagenumber and filename respectively.
 */

define convertchars(string);
    vars _l;
    stacklength() -> _l;
    appdata(string,
        procedure(char);
            if char == `#` then
                dest_characters(pagenum)
            elseif char == `%` then
                appdata(vedcurrent,
                    procedure(char);
                        if islowercode(char) then lowertoupper(char)
                        else char
                        endif
                    endprocedure)
            else
                char
            endif
        endprocedure);
    consstring(stacklength() - _l)
enddefine;

define vedcentre(vedargument);
    unless vedargument = '' then
        ved_centre()
    endunless
enddefine;

define vedright(vedargument);
    unless vedargument = '' then
        ved_right();
    endunless
enddefine;

/*
CENTRE_ON_SCREEN moves current line to middle line of current window.  It
tries to avoid unnecessary scrolling when small numbers of lines are moved
by MOVE_LINES.
 */

define centre_on_screen();
    vars lines_off_centre;
    vedcheck();
    vedline - vedlineoffset - (vedwindowlength div 2 ) -> lines_off_centre;
    if abs(lines_off_centre) > vedwindowlength div 5 then
        unless vedlineoffset == 0 then
            repeat - lines_off_centre times
                vedscrolldown()
            endrepeat
        endunless;
        repeat lines_off_centre times
            vedscrollup()
        endrepeat
    endif
enddefine;

/*
RNO COMMANDS
OBEY_RNO processes RUNOFF-type commands in the file: the procedures
for '.bp' and '.tp' are implemented here. Further commands may be implemented
in the system library or user's library: when OBEY_RNO finds a command in a
file it thinks is a RNO command (eg '.xyz') it looks for a procedure
'rno_xyz()' and calls it if it finds it; if the procedure is not found
RNO_ERROR is called, which asks the user if it should continue paging, treating
the putative RNO line as an ordinary line of text. (RNO_BP, RNO_TP, and
RNO_ERROR are global and may be redefined by the user.)
 */

define isrnoline();
    vedlinestart('.')
enddefine;

define global rno_bp(num);
    vars brk;
    unless num = vednullstring then
        unless (strnumber(num) ->> num) then
            rno_error()
        endunless;
        num - 1 -> nextpage
    endunless;
    unless at_page_top do
        vedline -> brk;
        if foottext then
            insert_blank_lines(breakline - vedline)
        endif;
        brk -> breakline;
        true -> dont_ask
    endunless
enddefine;

define global rno_tp(num);
    unless (strnumber(num) ->> num) then
        rno_error()
    endunless;
    unless at_page_top or (breakline - vedline) >= num or
           (vedline + num) > vedusedsize(vedbuffer) then
        rno_bp(vednullstring)
    endunless
enddefine;

define obey_rno();
    vars command argument name proc reponse rno_stack rno_comm;

    /* save stack size and rno command for rno_error */
    stacklength() -> rno_stack;
    vedthisline() -> rno_comm;

    unless vedcolumn = vvedlinesize then
        vedcharright()
    else
        rno_error()
    endunless;
    vedmoveitem() -> command;
    /* stick rno_ on front of command and look for a procedure of that name */
    consword('rno_' sys_>< command) -> name;
    if identprops(name) == 0 and isprocedure(valof(name) -> proc, proc) then
        proc
    elseif syslibcompile(name sys_>< '') then
        valof(name)
    else
        rno_error()
    endif -> proc;

    if vedcolumn <= vvedlinesize then
        substring(vedcolumn,vvedlinesize-vedcolumn+1,rno_comm)
    else
        vednullstring
    endif -> argument;

    vedlinedelete();
    vedscreenleft();

    /* call proc with rest of line as argument if required */
    proc(if pdnargs(proc) == 1 then argument endif)

enddefine;

/* exported error routine for errors in rno commands */
define global rno_error();
    vars response n p;
    /* check that call is in legal context */
    1 -> n;
    repeat;
        caller(n) -> p;
        unless p then
            mishap('RNO_ERROR CALLED OUT OF CONTEXT',[])
        endunless;
        if p == obey_rno then quitloop endif;
        n + 1 -> n
    endrepeat;
    /* level off stack */
    repeat stacklength() - rno_stack times erase(); endrepeat;
    centre_on_screen();
    vedwiggle(vedline, vedcolumn);
    vedscreenbell();
    vedputmessage('RNO COMMAND ERROR: ' sys_>< rno_comm sys_>< ' - CONTINUE? Y/N');
    vedinascii() -> response;
    unless response == `y` or response == `Y` then
        exitfrom(obey_page)
    else
        if at_page_top then
            false -> at_page_top
        else
            vednextline()
        endif;
        exitfrom(obey_rno)
    endunless
enddefine;

/*
FIND_PREVIOUS_PAGE_TOP looks for previous form feed and moves cursor to line
following it if one is found; otherwise moves cursor to top of file
 */

define find_previous_page_top();
    vars _string;
    until   length(vedthisline() ->> _string) > 0
            and issubstring(popformfeed, 1, _string)
            or vedline == 1 do
        vedcharup();
    enduntil;
    unless vedline == 1 do
        vednextline()
    endunless
enddefine;

/*
INSERT procedures: all insert material ABOVE current line, ie current line
is always displaced downwards.  After a call of insert_rubric or
insert_blank_lines the (notional) cursor lies on the line following the
insert, ie on the same piece of text as it began.  After a call of
insert_formfeed the cursor remains on the inserted line.
 */

define insert_rubric(rubric);
    vedlineabove();
    vedinsertstring (convertchars(rubric(1)));
    vedcentre       (convertchars(rubric(2)));
    vedright        (convertchars(rubric(3)));
    vednextline()
enddefine;

define insert_blank_lines(num);
    repeat num times
        vedlineabove();
        vednextline()
    endrepeat
enddefine;

define insert_formfeed();
    vedlineabove();
    1 -> vedcolumn;
    vedinsertstring(popformfeed)
enddefine;

/*
DELETE procedure
 */

define delete_lines(num);
    repeat num times
        vedlinedelete()
    endrepeat
enddefine;

/*
REMOVE_PAGE starts with cursor on line with formfeed.  Lines from breakline
to formfeed line are deleted; cursor jumps to top of page and deletes
inserted lines there, leaving cursor on line following last formfeed.
 */

define remove_page();
    vars ffline;
    vedline -> ffline;
    vedjumpto(breakline, 1);
    delete_lines(ffline - vedline + 1);
    find_previous_page_top();
    delete_lines(topgap);
    if headtext then
        delete_lines(headgap + 1)
    endif;
    if aligntop then
        insert_blank_lines(top_blanks_removed);
        0 -> top_blanks_removed
    endif;
    pagenum - 1 -> pagenum
enddefine;

/*
MOVE_LINES moves lines of text from the bottom of most recent complete page
to after the formfeed line if the argument is positive; if the argument is
negative, it moves them back.
 */

define move_lines(num);
    vars ffline;
    if foottext then
        vedline -> ffline;
        if num >= 0 then
            vedjumpto(breakline - 1, 1);     vedmarkhi();
            vedjumpto(vedline - num + 1, 1); vedmarklo();
            vedline -> breakline;
            ved_copy();
            vedjumpto(ffline, 1);
            ved_y();
            vedjumpto(breakline, 1);
            until vedline > vvedmarkhi do
                vedcleartail();
                vednextline()
            enduntil
        else
            abs(num) -> num;
            vedchardown();                   vedmarklo();
            vedjumpto(vedline + num - 1, 1); vedmarkhi();
            vedjumpto(breakline - 1, 1);
            ved_m();
            vedjumpto(vedline + num + 1, 1);
            vedline -> breakline;
            delete_lines(num)
        endif;
        vedjumpto(ffline, 1)
    else
        vedlinedelete();
        vedjumpto(vedline - num, 1);
        insert_formfeed();
        vedline -> breakline
    endif
enddefine;

/*
ASK_IF_OK handles dialogue with user.
 */

define ask_if_OK();
    vars response linesmoved linesremaining num;
    _oldchanged -> vedchanged;
    0 -> linesmoved;
    repeat
        vedwiggle(vedline, 1);
        vedputmessage('OK? Y or N or <RETURN> or <integer>');
        vedwiggle(vedline, 1);
        vedinascii() -> response;
        if response == `\r` then
            vednextline();
            return
        elseif response == `y` or response == `Y` then
            vednextline();
            vedputmessage('PAGING STOPPED');
            exitfrom(ved_page)
        elseif response == `n` or response == `N` then
            remove_page();
            centre_on_screen();
            vedputmessage('PAGING STOPPED - LAST PAGE REMOVED');
            exitfrom(ved_page)
        elseif isnumbercode(response) then
            response - `0` -> num;
            pagesize - linesmoved
                - if headtext then headgap + 1 else 0 endif
                - if foottext then footgap + 1 else 0 endif -> linesremaining;
            if linesremaining >= num then
                linesmoved + num -> linesmoved;
                move_lines(num);
                centre_on_screen();
            else
                vedscreenbell();
                vedputmessage('YOU CAN\'T MOVE MORE THAN '
                    sys_>< linesremaining sys_><' LINES FORWARD');
                charin_timeout(150) -> response
            endif
        elseif response == `-` then
            vedinascii() -> response;
            if isnumbercode(response) then
                `0` - response -> num;
                if linesmoved + num >= 0 then
                    linesmoved + num -> linesmoved;
                    move_lines(num);
                    centre_on_screen();
                else
                    vedscreenbell();
                    vedputmessage(  if linesmoved > 0 then
                                        'YOU CAN\'T MOVE MORE THAN '
                                        sys_>< linesmoved sys_>< ' LINES BACK'
                                    else
                                        'YOU CAN\'T MOVE ANY MORE LINES BACK'
                                    endif );
                    charin_timeout(150) -> response
                endif
            else
                vedscreenbell()
            endif
        else
            vedscreenbell()
        endif
    endrepeat
enddefine;

/*
MAKE_PAGE begins with cursor on line after final formfeed.  First, if
aligntop is true, blank lines are removed until a text line is the first
on the page.  Then the procedure works down the page and inserts topgap,
headtext and headgap (if there's a headline), measures off text, and inserts
footgap and foottext (if there's a footline) and formfeed line.
MAKE_LAST_PAGE copes with the special problems created by the last page.
 */

define make_page_top();
    true -> at_page_top;
    if aligntop and vedline /== 1 then
        0 -> top_blanks_removed;
        until vvedlinesize > 0 do
            vedlinedelete();
            1 + top_blanks_removed -> top_blanks_removed
        enduntil
    endif;
    while isrnoline() do
        obey_rno()
    endwhile;
    if nextpage then
        nextpage -> pagenum;
        false -> nextpage;
    endif;
    pagenum + 1 -> pagenum;
    insert_blank_lines(topgap);
    if headtext then
        insert_rubric(headtext);
        insert_blank_lines(headgap)
    endif;
    if at_page_top then
        false -> at_page_top
    else
        vednextline()
    endif
enddefine;

define make_last_page_bottom();
    if foottext then
        insert_rubric(foottext);
        vedcharup();
        insert_blank_lines(breakline - vedline + footgap);
        vedchardown()
    else
        vedline -> breakline
    endif;
    insert_formfeed();
    exitfrom(make_page)
enddefine;

define make_page_bottom();
    if vedatend() do
        make_last_page_bottom()
    endif;
    checkbottom();
    if foottext then
        insert_blank_lines(footgap);
        insert_rubric(foottext)
    endif;
enddefine;

define make_page();
    ;;; find breakline, which is where page is likely to end
    vedline + pagesize - if foottext then footgap + 1 else 0 endif
        -> breakline;
    false -> dont_ask;
    make_page_top();
    until vedline >= breakline or vedatend() do
        if isrnoline() do
            obey_rno()
        else
            vednextline()
        endif
    enduntil;
    make_page_bottom();
    if nextpage then
        nextpage -> pagenum;
        false -> nextpage;
    endif;
    insert_formfeed()
enddefine;

/*
CHECK_NAME checks to see if the current file has already had paging done
on it (by looking for a '.pag' suffix); if it isn't, it writes it and adds
the suffix.
 */

define check_name();
    vars directory vedargument;
    unless sys_fname_extn(vedcurrent) = '.pag' then
        if vedwriteable then
            ved_w1();
            sys_fname_path(vedcurrent) -> directory
        else
            veddirectory -> directory
        endif;
        directory dir_>< sys_fname_nam(vedcurrent) sys_>< '.pag' -> vedargument;
        ved_name()
    endunless
enddefine;

/*
OBEY_PAGE: the controlling procedure
 */

define obey_page();
    vars argument vedautowrite _oldchanged;
    check_name();

    ;;; user may have used ENTER SET to assign false to these variables:
    unless topgap then 0 -> topgap endunless;
    unless headgap then 2 -> headgap endunless;
    unless footgap then 2 -> footgap endunless;

    false -> vedautowrite;
    if vedchanged then
        vedchanged + 1
    else
        1
    endif -> _oldchanged;
    vedendfile();
    find_previous_page_top();
    if vedargument then
        if strnumber(vedargument) ->> argument then
            argument - 1 -> pagenum
        endif;
        '' -> vedargument
    elseif vedline == 1 then
        0 -> pagenum
    endif;
    until vedatend() do
        make_page();
        if never_ask or dont_ask then
            vednextline()
        else
            centre_on_screen();
            ask_if_OK()
        endif
    enduntil;
    vedputmessage('PAGING STOPPED - END OF FILE');
    _oldchanged -> vedchanged
enddefine;

/*
VED_PAGE, VED_AUTOPAGE, VED_UNPAGE
 */

define global ved_page();
    false -> never_ask;
    obey_page()
enddefine;

define global ved_autopage();
    true -> never_ask;
    obey_page()
enddefine;

define global ved_unpage();
    vars n;
    if vedargument = '*' then
        1000000
    else
        vedargint(vedargument)
    endif -> n;
    vedendfile();
    find_previous_page_top();
    repeat n times
        unless vedline == 1 then
            vedcharup()
        else
            vedputmessage('ALL PAGES REMOVED');
            return
        endunless;
        vedline - if foottext then footgap + 1 else 0 endif -> breakline;
        remove_page()
    endrepeat;
    centre_on_screen()
enddefine;

endsection;
endsection;

/*  --- Revision History ---------------------------------------------------
--- John Williams, Oct  7 1997
        Increased maxpagesize from 60 to 66.
--- John Williams, Aug  3 1995
        Now sets compile_mode +oldvar.
--- Adrian Howard, Sep  8 1992
        Now uses sys_><
--- Aaron Sloman, Nov 11 1989
    changed vedargnum to vedargint
--- Gareth Palmer, Sep  8 1989
        Added use of -next_page-.  See bugreport SFR 4190.
--- John Gibson, Aug  4 1989
        check_name now uses sys_fname...procedures
--- Roger Evans, April 1983 - RNO commands generalised.
 */
