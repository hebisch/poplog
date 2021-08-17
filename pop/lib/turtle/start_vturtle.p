/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/turtle/start_vturtle.p
 >  Purpose:        a program to enable users to run vturtle from outside ved
 >  Author:         Aaron Sloman, May 1983 (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

if identprops("popturtlefile") == undef then
    pop11_compile([uses vturtle;])
endif;

endsection;

section $-vturtle => start_vturtle, vturtle_readline;

define global vars vturtle_readline() with_props readline;
dlocal vedbreak = false, vedstatic = false;
lvars char proc plen, done = false;
    if vedcolumn fi_< vvedlinesize then vedcharinsert(`\r`); endif;
    vedcharinsert(`\r`); vedcharup();
    if vvedlinesize fi_> 0 then vedlinebelow() endif;
    vedputmessage('READLINE HERE - FINISH WITH ENTER OR RETURN');
start:
    datalength(pop_readline_prompt) fi_+ 1 -> plen;
    vedinsertstring(pop_readline_prompt);
    vedscreenbell();
    until done do
        max(plen, vedcolumn) -> vedcolumn;
        vedcheck();
        vedcursorset() ->;
        vedinascii() -> char;
        if char == `\r` or char == `\^D` then
            true -> done
        elseif (vedgetproctable(char) ->> proc) == vedinsertvedchar
                or proc == vedcharmiddle or proc == vedtextright
                or proc == vedrefresh
        then proc()
        elseif (proc == vedchardelete or proc == vedwordleft
                    or proc == vedcharleft or proc == vedwordleftdelete)
                and vedcolumn fi_> plen
        then
            proc()
        elseif (proc == vedcharright or proc == vedwordright
                    or proc == veddotdelete or proc == vedwordrightdelete
                    or proc == vedcleartail or proc == vedchangecase)
                and vedcolumn fi_<= vvedlinesize
        then proc()
        elseif proc == vedenterkey then true -> done
        elseif proc == vedscreenleft or proc == vedtextleft then
            plen -> vedcolumn;
        elseif proc == vedclearhead then
            vedclearhead(); goto start
        elseif proc == vedlinedelete then
            vedclearhead(); vedcleartail(); goto start
        elseif proc == vedcleartail then
            vedcleartail();
        else vedscreenbell()
        endif;
        vedsetlinesize();
    enduntil;
    ;;; now create the list of text
    vedtrimline();
    copy(vedthisline()) -> proc;
    stringin(proc) -> proc;
    repeat plen fi_- 1 times erase(proc()) endrepeat;
    incharitem(proc) -> proc;
    [% until (proc() ->> done) == termin do done enduntil%];
    vedlinebelow();
enddefine;

define startup(start_proc);
    dlocal cucharout, vedprintingdone, readline = vturtle_readline;
    vedcharinsert -> cucharout;
    vedsetonscreen(vedopen('output'),false);
    vedendfile();
    ;;; true -> vedwriteable;
    vedrefresh();
    start_proc();
enddefine;


define global start_vturtle(start_proc, _num);
    vedsetup();
    procedure;
        dlocal vedstartwindow;
        _num -> vedstartwindow;
        vedobey('output', startup(%start_proc%));
    endprocedure()
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  1 1995
        Transferred in old ved*readline as vturtle_readline
--- Aaron Sloman, Jan 27 1987
    stopped it making vedwriteable true
*/
