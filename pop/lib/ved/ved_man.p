/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_man.p
 > Purpose:         Read Unix 'man' file into VED
 > Author:          John Williams, Mar 10 1989 (originally Aaron Sloman) (see revisions)
 > Documentation:   HELP * MAN
 > Related Files:   LIB * PIPEIN, LIB * VED_NOUL
 */

compile_mode :pop11 +strict;

section;

include sysdefs.ph;

vars vedmanname = 'man';

define vars vedmandefaults();
    vedveddefaults();
    "man" -> vedfileprops;
    false ->> vedcompileable ->> vedwriteable ->> vedbreak -> vednotabs;
    8 -> vedindentstep
enddefine;


define lconstant Trimwhite(i, s);
    lvars j;
    fast_for j from i to datalength(s) do
        quitif(fast_subscrs(j, s) fi_> `\s`)
    endfast_for;
    allbutfirst(j fi_- 1, s)
enddefine;


define vars ved_man();
    lvars arg, name = false, sect = false, c, l, i, name, get_buff;

    define lconstant Get_buff_1(_) -> vbuff;
        lvars command;
        if strmember(`/`, arg) then
            'nroff -man ' <> sysfileok(arg)
        else
           #_IF DEF BERKELEY
            'man ' <> arg <> ' | cat -s'
           #_ELSE
            'man ' <> arg
           #_ENDIF
        endif -> command;

        vedputmessage('\{b}reading ' <> name);
        {%  'MAN ' <> lowertoupper(arg),
            nullstring,
            vedreadin(pipein('/bin/sh', ['sh' '-ce' ^command], false), `\[ib]`) ->
        %} -> vbuff;

        if datalength(vbuff) < 4 then vederror('\{b}not found') endif
    enddefine;

    vedsetup();
    if (vedargument ->> arg) = nullstring then
        vedmanname -> arg
    else
        arg -> vedmanname
    endif;

    /* convert all upper case names to lower (except X !) */
    if arg /= 'X'
    and (true,  for c in_string arg do
                    if islowercode(c) then ->, false, quitloop endif
                endfor)
    then
         uppertolower(arg) -> arg
    endif;

    /* look for section number */
    datalength(arg) -> l;
    Get_buff_1 -> get_buff;

    if arg(l) == `)` and (locchar_back(`(`, l - 1, arg) ->> i) then
        substring(i + 1, l - i - 1, arg) -> sect;
        substring(1, i - 1, arg) -> arg
    elseif (isnumbercode(arg(1)) or arg(1) == `?`)
    and (strmember(`\s`, arg) ->> i) then
        substring(1, i - 1, arg) -> sect;
        Trimwhite(i, arg) -> arg
    elseif isstartstring('-l ', arg) then
        '?' -> sect;
        Trimwhite(4, arg) -> arg
    endif;

    if sect = '?' then
        #_IF DEFV SUNOS >= 5.0
        define lconstant Get_buff_2(_);
            lvars dev, line, i, j, sect, c;
            pipein(consref('man'), ['man' '-l' ^arg], false) -> dev;
            {% for line from_repeater line_repeater(dev, #_<inits(127)>_#) do
                if (locchar(`(`, 1, line) ->> i)
                and (locchar(`)`, i, line) ->> j) then
                    substring(i + 1, j - i - 1, line) -> sect;
                    consvedstring
                        (#| for c in_string line do c fi_|| `\{2A}` endfor |#)
                        -> line;
                    {% 1, '(SNbp) qman -s' <> sect <> ' ' <> arg %}
                        -> vedstring_data_prop(line);
                endif;
                line
            endfor %}
        enddefine;
        Get_buff_2 -> get_buff;
        'man -l ' <> arg -> name;
        #_ELSE
        vedputmessage('\{b}man -l only available on Solaris platforms');
        #_ENDIF
    elseif sect then
        #_IF not(DEF BERKELEY)
        '-s' <>
        #_ENDIF
        sect <> '\s' <> arg -> arg
    endif;

    unless name then 'man ' <> arg -> name endunless;
    vededit(consref({% name, get_buff, false %}), vedmandefaults)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar 12 1998
        Removed test strmember(`.`, arg) from Get_buffer_1 as it is not
        needed to identify explicit pathnames and prevent locating
        man pages with a `.` in the title.
--- John Williams, Aug 27 1996
        Translates a section number of `?' to `man -l' (Solaris only)
        and makes the output from `man -l' active.
--- John Williams, May  9 1996
        Allows section numbers to be specified by either man topic(section)
        or man section topic. (Fixes BR aarons.125).
--- John Gibson, Apr 21 1994
        Removed sys*wait (no longer necessary)
--- John Gibson, Mar  7 1994
        Changed to use vededit
--- John Gibson, Jan 19 1994
        Changed to use new facility in vedgetfile/vedreadfile that allows
        a user buffer-producing procedure to be specified (now says
        'READING man xxx' while command is being executed)
--- John Williams, Nov 16 1993
        Displays the message "Please Wait" while man command is being
        executed. Also, the procedure Display now uses vedtopfile to set
        cursor position and vvedlinesize, rather than just vedsetlinesize.
--- John Gibson, Feb 10 1992
        Added extra attributes arg to call of -vedreadin- to specify
        how underlining should be interpreted
--- John Gibson, Jan  6 1992
        Removed use of ved_noul -- no longer required, since -vedreadin-
        now interprets backspace-style underlining and bold.
        Also made vedmandefaults call vedveddefaults first
--- John Williams, Nov  5 1991
        Added Jon Meyer's changes which make it possible to access
        MAN files not included in $MANPATH via their explicit pathname.
--- John Williams, Mar 26 1991
        Added -vedmanname- (fixes BR jasonh.3)
--- Robert John Duncan, Mar 20 1991
        Added wait for child process to prevent zombies
--- John Williams, Mar 19 1991
        Moved case-conversion of all-upper-case names (except X) back
        into -ved_man-  (to fix BR danielep@cogs.susx.ac.uk.2)
--- John Williams, Jan  4 1991
        Now only the -vedgetsysfilepdr- version does case-conversion
--- John Williams, Nov 12 1990
        Edits existing MAN file if possible. No longer uses -vedpipein-.
--- Aaron Sloman, Apr 16 1990
        Moved the definition of syntax word "man" out of this file.
        Required altering -ved_man- so that it can be invoked outside VED.
--- James Goodlet, May  2 1989
        Now pipes manual pages through 'cat -s' to compress adjacent blank
        lines.  Only on BERKELEY-type Unix.
--- John Williams, Mar 22 1989
        Mostly re-written
 */
