/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_mdir.p
 > Purpose:        Build a directory of messages in a Unix mail file
 > Author:         Aaron Sloman, Nov  1 1986 (see revisions)
 > Documentation:  HELP * VED_MDIR
 > Related Files:  LIB * VED_GM * VED_REPLY * VED_SEND
 */

/*
<ENTER> mdir
In a file of Unix mail messages produces a temporary file with the name of
the original file at the top and a directory of messages. Each message has
a number, a line number (which can get out of date if the file is edited)
the 'From ..' line from the header and the subject, e.g.
----
    /home/csuna/aarons/mymail Sat Jun  3 22:17:34 BST 1989
    1: 6 From philh@rsunb.uucp Thu Mar  2 13:33:13 1989
    To: aifac
    Subject: examining allocation
    2: 403 From Aaron Sloman Sun Mar  5 15:31:55 GMT 1989
    To: writer
    Subject: Book by Edward De Bono
    3: 415 From Aaron Sloman Tue Mar  7 02:13:50 GMT 1989
    To: philh,desw
    Subject: CS2 Lectures
    4: 435 From johnw@psung.uucp Wed Mar  8 13:03:20 1989
    To: poplocal
    Subject: init files
---
LIB * VED_GM can be used to Go to a Message. E.g. '<ENTER> gm 3' will Go
to Message number '3:'. It uses the rest of the line as a search string,
so will find the message even if the file has been edited and line numbers
have therefore changed.
*/

section;
define global ved_mdir;
    ;;; make a mail directory - list mail files in current file
    lvars linenum, list = [], pathname=vedpathname,
         tmpfile=systmpfile(false,'MDIR',nullstring);
    dlocal cucharout, vedediting;
    vedpositionpush();
    vedendfile();
    ;;; first collect information in header, and build into list
    lblock lvars line, subjline, fromline, toline, loc, minline=0;
    repeat
        if vedteststartsearch('From ') then
            if minline == 0 then vedline -> minline
            elseif vedline == minline then quitloop
            endif;
            vedline -> linenum;
            vedthisline()-> fromline; false ->> subjline -> toline;
            repeat
                vednextline();
                vedthisline() -> line;
                locchar(`:`, 1, line) -> loc;
                unless loc and vvedlinesize > loc then quitloop endunless;
                if isstartstring('Subject: ', line) then line -> subjline
                elseif isstartstring('To: ', line) then line -> toline
                endif;
            quitif(subjline and toline)
            endrepeat;
            conspair({%linenum, fromline,toline,subjline%}, list) -> list;
        else vederror('NOT MAIL FILE');
        endif;
    endrepeat;
    endlblock;
    ;;; Now store entries in index file
    vedpositionpop();
    vededit(tmpfile, vedhelpdefaults);
    false -> vedbreak;
    ;;; insert information to show where the entries came from (for ved_gm)
    vedinsertstring(pathname); vedcharright();
    vedinsertstring(sysdaytime()); vedlinebelow();
    vedputmessage('PLEASE WAIT');
    false -> vedediting;
    fast_ncrev(list) -> list;
    ;;; insert the entries
    lblock lvars linevec, loc = 0, string, count;
    fast_for linevec in list do
        loc fi_+ 1 -> loc;
        vedcharinsert -> cucharout;
        pr(loc); pr(': ');
        pr(fast_subscrv(1,linevec)); vedcharright();
        pr(fast_subscrv(2,linevec)); vedlinebelow();
        ;;; put in To: line and Subject: line
        fast_for count from 3 to 4 do
            if (fast_subscrv(count,linevec) ->> string) then
                copy(string) -> vedthisline(); vedlinebelow()
            endif
        endfor
    endfor
    endlblock;
    vedtopfile();
    true -> vedediting; vedputmessage(nullstring); vedrefresh();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  2 1989 altered to include "To:" line. uses
    lblocks and other minor changes.
--- Aaron Sloman, Mar  3 1989 made vedbreak false in temporary file
--- Aaron Sloman, Jan 14 1989 removed vednullstring
 */
