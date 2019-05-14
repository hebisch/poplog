/*  --- Copyright University of Sussex 1997.  All rights reserved. ---------
 > File:           C.all/lib/auto/tabify.p
 > Purpose:        turn leading (and other) spaces into tabs
 > Author:         Mark Rubinstein, Jan 22 1986 (see revisions)
 > Documentation:  HELP * TABIFY
 */
compile_mode :pop11 +strict;

section;

lconstant BUFSIZE = 4096;

;;; returns the next tab position from column -col-.
;;; uses vedindentstep
define lconstant nexttab(col);
    lvars col = col fi_+ vedindentstep;
    col fi_- (col fi_rem vedindentstep)
enddefine;

;;; tabifyline transcribes the first -nchars- from -frombuf- into -tobuf-
;;; turning spaces into tabs.  It returns the number of significant chars
;;; transcribed into -tobuf-.
;;; If -all- is true then tabs are inserted whenever they would replace
;;; two or more characters, otherwise only leading blanks and tabs are
;;; converted.
;;; If -striptrail- is true, strip trailing tabs/spaces.
;;; If -vedtype- is true then tab characters are inserted until the next tab
;;; stop (as in a ved buffer) rather than just one.

define lconstant tabifyline(frombuf, tobuf, nchars, all, striptrail, vedtype)
                                                -> nchars;
    lvars   f, frombuf, tobuf, nchars, char, all, striptrail, vedtype,
            tcol = 0, fcol = 0, t = 1;
    ;;; -f- is index into -frombuf-. -t- is index into -tobuf-.
    for f from 1 to nchars do
        fast_subscrdstring(f, frombuf) -> char;
        ;;; leave anything with attributes alone
        if char == `\s` then
            fcol fi_+ 1 -> fcol;
        elseif char == `\t` then
            if vedtype then fcol fi_+ 1 else nexttab(fcol) endif -> fcol;
        else
            unless tcol fi_+ 1 == fcol then
                while (nexttab(tcol) fi_<= fcol) do
                    `\t` -> fast_subscrdstring(t, tobuf);
                    t fi_+ 1 -> t;
                    if vedtype then tcol fi_+ 1 else nexttab(tcol) endif -> tcol;
                endwhile;
            endunless;
            while tcol fi_< fcol do
                `\s` -> fast_subscrdstring(t, tobuf);
                t fi_+ 1 -> t;
                tcol fi_+ 1 -> tcol;
            endwhile;
            unless all then
                nchars fi_- f fi_+ 1 -> nchars;
                move_subvector(f, frombuf, t, tobuf, nchars);
                t fi_+ nchars -> t;
                quitloop
            endunless;
            char -> fast_subscrdstring(t, tobuf);
            t fi_+ 1 -> t;
            tcol fi_+ 1 -> tcol;
            fcol fi_+ 1 -> fcol;
        endif;
    endfor;
    t fi_- 1 -> nchars;

    if striptrail then
        while nchars fi_> 0
        and ((fast_subscrdstring(nchars, tobuf) ->> char) == `\s`
             or char == `\t`)
        do
            nchars fi_- 1 -> nchars
        endwhile
    endif;

enddefine;


;;; tabify the file called -file-.  (Actually moves the file into a temporary
;;; file and then writes back into the current file.  Use with care.)
;;; If -all- is true then all spaces are converted where a tab will convert
;;; two or more spaces, otherwise only leading blanks and tabs are converted.
define global tabify(file, all);
    lvars   file, all, indev, outdev, nchars, readbuffer = inits(BUFSIZE),
            writebuffer = inits(BUFSIZE);

#_IF hd(sys_os_type) == "unix"
    lvars oldinterrupt = interrupt;
    lconstant tempfile = systmpfile('', 'tabify', popusername);

    sysunlink(tempfile) ->;
    ;;; move the file into the temporary file, for safety.
    unless syslink(file, tempfile) and sysunlink(file) then
        mishap(file, 1, 'file not found')
    endunless;

    ;;; reassign interrupt in case of trouble
    define dlocal interrupt();
        oldinterrupt -> interrupt;
        sysunlink(file) ->;
        if syslink(tempfile, file) then sysunlink(tempfile) endif;
        interrupt();
    enddefine;

    ;;; open the devices
    sysopen(tempfile, 0, "line", `N`) -> indev;
#_ELSE
    sysopen(file, 0, "line", `N`) -> indev;
#_ENDIF

    syscreate(file, 1, "line") -> outdev;
    ;;; tabify each line of the file;
    until (sysread(indev, readbuffer, BUFSIZE) ->> nchars) == 0 then
        tabifyline(readbuffer, writebuffer, nchars, all, false, false) -> nchars;
        syswrite(outdev, writebuffer, nchars);
    enduntil;
    sysclose(indev);
    sysclose(outdev);
#_IF hd(sys_os_type) == "unix"
    sysunlink(tempfile) ->;
#_ENDIF
enddefine;

;;; tabify the current file (converting all spaces to tabs if the argument is
;;; "-a" where it would convert two or more spaces, otherwise only leading
;;; blanks and tabs are converted).  In ved style all positions covered by a
;;; tab are filled with a tab character.  For safety's sake it copies
;;; the vedbuffer, converts the copy and then makes the copy the buffer.  Uses
;;; more space but is safer.
;;; If -strip is supplied, strip trailing tabs/spaces.

define global ved_tabify;
    lvars   line, lineno, nchars, all = false, striptrail = false, arg,
            buffer = copy(vedbuffer), writebuffer = initdstring(BUFSIZE);

    vedputmessage('converting spaces to tabs ...');
    false -> vednotabs;
    for arg in sysparse_string(vedargument) do
        if isstartstring(arg, '-all') then
            true -> all
        elseif isstartstring(arg, '-strip') then
            true -> striptrail
        else
            vederror('usage: tabify [-all] [-strip]')
        endif
    endfor;

    for lineno from 1 to datalength(buffer) do
        fast_subscrv(lineno, buffer) -> line;
        tabifyline(line, writebuffer, datalength(line), all, striptrail, true)
                                                            -> nchars;
        subdstring(1, nchars, writebuffer) -> fast_subscrv(lineno, buffer);
    endfor;
    buffer -> vedbuffer;
    vedsetlinesize();   ;;; to be safe;
    vedrefresh();
    if vedchanged then vedchanged + 1 else 1 endif -> vedchanged;
    vedputmessage('spaces converted');
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 27 1997
        Increased BUFSIZE to 4096
--- John Gibson, Mar 10 1992
        Changed to work properly with VED buffer dstrings. Also gave
        -ved_tabify- a -strip argument to strip trailing tabs
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- Aaron Sloman, Sep 30 1986 - Altered to use vedindentstep instead of
    simply assuming step of 4. Made public
--- Mark Rubinstein, Jan 23 1986 - vednotabs set false, not true.  Fixed error
    which occured if tabifying a VED file with some tabs already in.
*/
