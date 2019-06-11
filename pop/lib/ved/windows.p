/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:          C.all/lib/ved/windows.p
 >  Purpose:       experimental multiple windows for ved
 >  Author:        Aaron Sloman, Feb 1983 (see revisions)
 >  Documentation:  HELP * WINDOWS
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

include sysdefs.ph;
include sigdefs.ph;

section;

;;;Globals and utilities ----------------------------------------------

;;; save some system procedures;
;;; Could be lconstants if the file is only compiled once
global constant
    oldved_ved, oldved_q, oldved_rb, oldvedrestorescreen,
    oldvedrestorewindows, oldvedsetonscreen, oldvedswapfiles;

unless isprocedure(oldvedsetonscreen) then
    ved_ved -> oldved_ved;
    ved_q -> oldved_q;
    ved_rb -> oldved_rb;
    vedrestorescreen -> oldvedrestorescreen;
    vedrestorewindows -> oldvedrestorewindows;
    vedsetonscreen -> oldvedsetonscreen;
    vedswapfiles -> oldvedswapfiles;
endunless;

global vars
    vednewmapped=false,     ;;; true once new system initialised by newmap
    ved_full_screen=false,  ;;; switched by <ESC> w
    vedwindownum=0,         ;;; the number of the current window
    veddefaultlength,       ;;; default window length
    time_out_time=50,
    vedinputfocus,          ;;; used by system now
;
unless isinteger(veddefaultlength) then
    round(vedscreenlength/3) -> veddefaultlength
endunless;

define lconstant vedfullname(file);
    vedbuffername(file,true)
enddefine;


define lconstant 1 SAVEGLOBALS;
    vedsaveglobals(vedcurrentfile);
enddefine;

lconstant _ved_no_name= '__noname__' ;    ;;; Used for 'anonymous' file names

define lconstant has_no_name(string);
    issubstring(_ved_no_name,1,string)
enddefine;

define lconstant Vedwinsearch(name) -> file;
    ;;; Name may be a string or a list derived from vedsearchlistname
    ;;; Return the corresponding file if in vedbufferlist, or name
    ;;; that should be used.
    ;;; If vedsearchlist exists, get the file name that would
    ;;; be found. If not use original. The result could be a list.
    lvars name,file;
    if islist(name) and length(name) == 1 then name(1) -> name endif;
    if isstring(name) then
        sysfileok(name) -> name;
        vedpresent(name) -> file;
    else
        vederror('WRONG ARGUMENT FOR Vedwinsearch ' >< name)
    endif;
    if not(file) and isstring(name) then
        ;;; not in vedbufferlist or library try vedsearchlist
        if ispair(vedsearchlist) and name /= nullstring
        and not(strmember(fast_subscrs(1,name),'$~/'))
        and not(isstartstring('./', name))  ;;; could be a bug?
        and not(isstartstring('../', name))  ;;; could be a bug?
            ;;; #_IF DEF VMS
        and not(strmember(`[`,name)) and not(strmember(`:`,name))
            ;;; #_ENDIF
        then
            if readable(name) ->> file then sysclose(file); name
            else syssearchpath(vedsearchlist, name)
            endif -> file;
            unless file then name -> file endunless;
        else name -> file;
        endif
    endif;
    if islist(file) then hd(file) -> file endif;
enddefine;

define lconstant vedgetready(name) -> file;
    ;;; given file name make sure there's a ved file structure on vedbufferlist
    lvars name, file defaultproc=vedveddefaults;
    vars vedargument prwarning vedediting;
    erase -> prwarning;
    Vedwinsearch(name) -> file;
    if isvector(file) then
    else
        if (isstring(name) and has_no_name(name))
        then vedhelpdefaults -> defaultproc
        endif;
        vedopen(file,defaultproc, false) -> file;
    endif
enddefine;

define lconstant vedmarkcursorline(bool);
    ;;; should have been exported
    ;;; put or remove a mark on left of screen to indicate where cursor is
    lvars bool;
    if vedediting
    and vedline > vedlineoffset
    and vedline fi_< vedlineoffset fi_+ vedwindowlength
    then
        vedmarkset(vedline,
            if vedmarked(vedline) and vvedmarkprops then
                if bool then vedscreencursormark else vedscreenmark endif
            else
                if bool then vedscreencursor else `\s` endif
            endif);
    endif;
enddefine;


;;;Window records and associated mappings -----------------------------

;;; WINDOWS REPRESENTED BY RECORDS OF TYPE WINDOW

defclass lconstant procedure window
    {winnameof,winfileof,winnumof:short,
        winstartof:short,winendof:short,
        winvisof,winrefreshed};

;;; MAPPING FROM FILE NAMES TO WINDOWS
;;; use assoc, not newassoc or newproperty, so that strings will be recognised
vars procedure windowof_name = assoc([]);

;;; mapping from window numbers to files. Should be lvars;
lvars windownumbers;

define appwindows(proc);
    ;;; proc is a procedure of two arguments number and window
    ;;; apply it to all number windows pairs
    lvars num=0, win, procedure proc, lim=datalength(windownumbers);
    until num == lim do
        num fi_+ 1 -> num;
        if subscrv(num,windownumbers) ->> win then proc(num,win) endif
    enduntil;
enddefine;

define windowof_file(file) -> win;
    lvars file,win=false;
    if isvector(windownumbers) then
        appwindows(procedure(n,w); lvars n,w;
                if winfileof(w) == file then w -> win; exitfrom(appwindows)
                endif endprocedure);
    endif;
enddefine;



define  windowof_number(wnum);
    lvars wnum;
    if isvector(windownumbers) and isinteger(wnum) and 0 fi_< wnum
    and wnum fi_<= datalength(windownumbers) then
        subscrv(wnum,windownumbers)
    else false
    endif
enddefine;

define updaterof windowof_number();
    lvars n=vedscreenlength;
    unless isvector(windownumbers) then
        cons_with consvector
            {%until n == 0 do false, n fi_- 1 -> n; enduntil%} -> windownumbers;
    endunless;
    -> subscrv(windownumbers)
enddefine;

define nameof_window(win);
    lvars win;
    if win then winnameof(win) else false endif;
enddefine;

define updaterof nameof_window(name,win);
    lvars win;
    unless name == winnameof(win) then
        false -> windowof_name(winnameof(win));
        name -> winnameof(win);
        win -> windowof_name(name)
    endunless;
enddefine;

define nameof_number(wnum);
    ;;; return name corresponding to window of this number, or false
    lvars wnum;
        nameof_window(windowof_number(wnum))
enddefine;

define numberof_name(name) -> name;
    ;;; return number corresponding to window of this name, or false
    lvars name;
    if windowof_name(name) ->> name then
        winnumof(name) -> name
    endif;
enddefine;

define nextwindownum(num) -> num;
    ;;; get number of next window on screen.
    num + 1 -> num;
    unless iswindow(windowof_number(num)) then
        1 -> num
    endunless;
enddefine;

define lconstant vedwindowerr(item);
    lvars item;
    vederror('NO SUCH WINDOW: ' sys_>< item)
enddefine;

define lconstant procedure checkwindow(item) -> win;
    lvars item,win;
    if iswindow(item) then item -> win; return
    elseif isinteger(item) then windowof_number(item) -> win
    elseif isstring(item) then windowof_name(item) ->win
    endif;
    unless iswindow(win) then vedwindowerr(item) endunless;
enddefine;

define print_window(win);
    ;;; for debugging
    lvars win;
    printf(winendof(win),winstartof(win),nameof_window(win), winnumof(win),
            '<window %p : %p {%p %p}>')
enddefine;

print_window -> class_print(window_key);


define lconstant vedget_good_file(noname_nogood, inwindow_nogood) -> found;
    ;;; Find a file in vedbufferlist, not already on the screen,
    ;;; if noname_nogood is true it must not be an 'anonymous' file.
    ;;; Return false if none found

    lvars name,file, found=false, noname_nogood, inwindow_nogood;

    for file in vedbufferlist do
        if ved_full_screen
        or not(inwindow_nogood)
        or not(windowof_file(file))
        then
            unless noname_nogood and has_no_name(vedbuffername(file)) then
                file -> found;
                return;
            endunless
        endif
    endfor
enddefine;


define lconstant vedget_new_name() -> name;
    ;;; find an unallocated file, or new anonymous name
    ;;; guaranteed to produce an 'anonymous' name not clashing with
    ;;; something already displayed.
    lvars name,good, num;

    if (vedget_good_file(true, true) ->> good)
    or (vedget_good_file(false, true) ->> good) then
        vedfullname(good) -> name
    else
        ;;; create a new name
            1 -> num;
            repeat
                popdirectory dir_>< _ved_no_name sys_>< num ->name;
            quitunless(windowof_name(name));
                num + 1 -> num;
            endrepeat;
    endif;
    sysfileok(name) -> name;
enddefine;


global vars visible, vedwindownum, vedgotowindow,;


define lconstant shownamestring();
    ;;; display window number and name of current file
    if vedwindownum > 0 and visible(vedwindownum) then
        vedputmessage(vedwindownum sys_>< space sys_>< vednamestring
            sys_>< '(' sys_>< vvedbuffersize sys_>< ' lines)');
    endif;
enddefine;


;;;Manipulating windows -----------------------------------------------
define visible(win);
    ;;; win is a number or a window record. Return true or false
    lvars win;
    checkwindow(win) -> win;
    if win then winvisof(win) else false endif;
enddefine;

define updaterof visible(bool,win);
    ;;; change visibility status of window.
    lvars wnum old bool win file;
    checkwindow(win) -> win;
    isvector(winfileof(win)) -> file;
    winnumof(win) -> wnum;
    vedwindownum -> old;
    vedgotowindow(wnum);
    if winvisof(win) then  ;;; was visible
        bool -> winvisof(win)  ;;; may or may not be now
    else
        ;;; was not visible
        if bool then    ;;; making it visible
            true -> winvisof(win);
            ;;; now visible;
            if file then vedrefresh(); shownamestring(); endif
        endif;
    endif;
    vedgotowindow(old);
enddefine;

vars procedure (windowstart, windowend);

define vedsetupwindow(num)-> win;
    ;;; prepare to use window
    lvars name, num, win;
    checkwindow(num) -> win;
    winnumof(win) -> num;
    if ved_full_screen then 0, vedscreenlength
    else
        max(0,windowstart(win) - 1),
        winendof(win) - windowstart(win) + 1
    endif -> vedwindowlength -> vedscreenoffset ;
    min(vedscreenlength - vedscreenoffset, vedwindowlength) -> vedwindowlength;
    num -> vedwindownum;
enddefine;

define vedsetfileinwindow(file,num);
    lvars file,name,num,win,oldfile;
    ;;; num is a window or a number or a name. Set up without refreshing
    checkwindow(num) -> win;
    winfileof(win) -> oldfile;
    unless file == oldfile then
        file -> winfileof(win);
        vedfullname(file) -> name;
        name -> nameof_window(win); ;;; sets up other links
        false -> winrefreshed(win);
    endunless;
enddefine;

define vedsetonscreen(file,string);
    ;;; use current window - vedwindownum, unless file in another window
    ;;; or ved_full_screen is true

    lvars file, string,name,win,wnum,
        gotoplace=vvedgotoplace,oldnum=vedwindownum;

    unless vednewmapped then chain(file,string,oldvedsetonscreen) endunless;
    if iscaller(vedfileselect) then
        ;;; screws up window if not full screen
        true -> ved_full_screen
    endif;
    if isstring(file) then
        file -> name;
        vedgetready(name) -> file;      ;;; now front of vedbufferlist
    endif;
    unless isvector(file) then mishap(file,1,'NO FILE ') endunless;

    unless windowof_file(file) ->> win then
        windowof_number(vedwindownum)-> win
    endunless;
    SAVEGLOBALS;
    winnumof(win) -> wnum;
    unless wnum == vedwindownum or ved_full_screen then
        vedmarkcursorline(true);
    endunless;
    vedsetfileinwindow(file,win);   ;;; just link up - no display
    vedsetupwindow(wnum) ->;        ;;; set the globals, and vedscreenoffset
    if file /== vedcurrentfile or gotoplace or not(winrefreshed(win)) then
        file ->> vedcurrentfile -> vedlowerfile; ;;; bodge for putmessage
        vedsetglobals(file);
        vedsetupwindow(vedwindownum) ->;    ;;; ensure screen parameters OK
        SAVEGLOBALS;
        setfrontlist(file,vedbufferlist) -> vedbufferlist;
        false -> vedprintingdone;
        vedinitfile();
        if gotoplace then
            false -> vvedgotoplace;
            if isinteger(gotoplace) then
                vedtrimline();
                min(vvedbuffersize,gotoplace) -> vedline;
                vedsetlinesize();
            elseif isstring(gotoplace) then
                unless vedtestsearch(gotoplace,false) then
                    vederror(gotoplace sys_>< ' NOT FOUND')
                endunless
            else ;;;; should do a check?
            endif;
        endif;
        if visible(win) and vedediting then
            if vedwindowlength==1 then vedrefreshstatus()
            elseif ved_full_screen or not(winrefreshed(win)) then
                vedrefresh(); shownamestring();
                true -> winrefreshed(win);
            endif;
        endif;
    endif;
    if string then vedputmessage(string)
    elseif iscaller(vedswapfiles) then shownamestring()  ;;; UGH
    endif;
    if vedwindowlength ==1 then
        true -> ved_on_status;
    elseif visible(vedwindownum) then
                vedmarkcursorline(false);
    endif;
    vedcurrentfile -> vedinputfocus;
enddefine;

define lconstant createnewwindow(name,wnum, start, fin, win) -> win;
    ;;; May be given an exisiting window
    lvars name, wnum, start, fin, win, file;
    if name then
        vedgetready(name)-> file; vedfullname(file) -> name;
    else vedget_new_name() -> name; vedgetready(name) -> file;
    endif;
    if win then
        name -> winnameof(win);
        file -> winfileof(win);
    else
        conswindow(name,false,wnum,start,fin,true,false) -> win;
        win -> windowof_number(wnum);
        win -> windowof_name(name);
    endif;
    vedsetfileinwindow(file,win);
enddefine;


vars ved_redraw;    ;;; defined below
define Vedrefreswindow(wnum);
    ;;; set the window on the screen
    lvars wnum, win, newwindow=(wnum/==vedwindownum);
    if newwindow and vedwindowlength /== 1 and not(ved_full_screen) then
        vedmarkcursorline(true);
    endif;
    SAVEGLOBALS;
    if isinteger(wnum) and iscaller(ved_redraw) then
        ;;; file may have disappeared via ved_q, so create a new one.
        windowof_number(wnum) -> win;
        unless winnameof(win) then
            ;;; need to create a new file for this window
            createnewwindow(vedget_new_name(),false, false, false, win) -> win;
        endunless;
    endif;
    vedsetupwindow(wnum) -> win;    ;;; does wnum -> vedwindownum
    vedsetonscreen(winfileof(win),false);

    if newwindow then
        shownamestring()
    endif;
enddefine;

define vedgotowindow(wnum);
    lvars wnum;
    if ved_full_screen and wnum /== vedwindownum then
        false -> winrefreshed(checkwindow(wnum));
    elseif wnum /== vedwindownum then shownamestring()
    endif;
    Vedrefreswindow(wnum);
enddefine;


define lconstant vedrefreshscreenrange(x,y);
    ;;; x and y are screen line numbers
    ;;; assume they are part of the current window
    ;;; need to get buffer line numbers
    lvars x y;
    x - 1 - vedscreenoffset + vedlineoffset  -> x;
    y- 1 - vedscreenoffset + vedlineoffset  -> y;
    ;;; make sure vedscreenoffset etc are ok
    vedgotowindow(vedwindownum);
    vedrefreshrange(x,y,undef)
enddefine;

define ved_redraw;
    lvars old;
    unless vednewmapped then chain(vedrefresh) endunless;
    false -> ved_full_screen;
    vedwindownum -> old;
    appwindows(
        procedure(n,w);
            lvars n,w;
            SAVEGLOBALS; false -> vedcurrentfile;
            if visible(w) then false -> winrefreshed(w); vedgotowindow(n) endif;
        endprocedure);
    vedgotowindow(old);
enddefine;

define vednextwindow;
    ;;; wait for a while . If no number typed, go to next window.
    ;;; otherwise go to window of that number
    lvars wnum, window, oldnum = vedwindownum, timedout = false;
    unless vednewmapped then chain(oldvedswapfiles) endunless;
    if time_out_time == 0 then `\r` -> wnum;
    elseunless (charin_timeout(time_out_time) ->> wnum) then
        `\r` -> wnum;
        true -> timedout;
    endif;
    repeat forever
    quitif(wnum == `\r` or windowof_number(wnum - `0`));
        vedscreenbell();
        vedwiggle(0,60);
        vedputmessage('WHICH WINDOW?');
        rawcharin() -> wnum;
    endrepeat;
    if wnum == `\r` then vedwindownum fi_+ 1 else wnum fi_- `0` endif -> wnum;
    repeat
        nextwindownum(wnum - 1) -> wnum;    ;;;wrap around if necessary
        unless ved_full_screen or visible(wnum) then
            vederror('NOT A VISIBLE WINDOW: ' sys_>< wnum)
        endunless;
        vedgotowindow(wnum);
    quitunless(timedout);
    returnif(wnum == oldnum)(vedputmessage('NO OTHER USEFUL FILES'));
        ;;; user did not explicitly specify a number. If this is a
        ;;; noname file then try another one, otherwise quit.
    quitunless(has_no_name(vedpathname));
        nextwindownum(wnum) -> wnum
    endrepeat;
enddefine;

define lconstant parse_argument(string) -> list;
        ;;; make sure commas are separated by spaces for sysparse_string
        ;;; and add comma at end
    lvars string;
        cons_with consstring
        {%
            appdata(string,
                procedure(c); lvars c;
                    if c == `,` then ` `, c, ` ` else c endif
                endprocedure),
            ` `, `,` %}.sysparse_string -> list;
enddefine;


define ved_newmap;
    ;;; ENTER newmap followed by number name, number name, ....
    ;;; or ENTER newmap off
    ;;; see comments at top of file
    lvars fin, arglist, stack=stacklength();

    vars name, command, rest;   ;;; vars used in matcher

    dlocal vedargument;

    unless vedinvedprocess then
            -> name;    ;;; string argument for ved_newmap. (not yet in ved)
        ;;;     vedget_new_name() -> vedargument;
        vedinput(veddo(%'newmap ' >< name%));
        chain(oldved_ved);
    elseif vedargument = 'off' then
        if vednewmapped then
            false -> vednewmapped;
            if vedwindowlength < vedscreenlength then
                dlocal vedediting = false;
                vedsetwindow()
            endif
        endif;
        chain(vedrefresh)
    elseif vednewmapped and vedargument = nullstring then
        chain(ved_redraw)
    else
        false ->> vedupperfile -> vedlowerfile;
        ;;; Initialise structures
        assoc([]) -> windowof_name;
        false -> windownumbers;
        0 -> vedwindownum;

        if vedargument = nullstring and not(vednewmapped) then
            ', ,' -> vedargument;   ;;; three window default.
        endif;
        true -> vednewmapped;

        false -> ved_full_screen;
        0-> vedscreenoffset;

        ;;; make sure commas are separated by spaces for sysparse_string
        ;;; and add comma at end
        parse_argument(vedargument) -> arglist;

        unless arglist.front.isinteger then 1::arglist -> arglist;
        endunless;  ;;; start location defaults to top of screen

        lblock lvars win, start, wnum = 0;
            ;;; go through making windows and building up file names
            until arglist == [] or hd(arglist) > vedscreenlength do
                1 + wnum -> wnum;    ;;; number of window
                destpair(arglist) -> arglist -> start;
                if arglist matches [',' ?? rest] then
                    ;;; only window start given, use existing file or 'no name'
                    vedget_new_name() -> name;
                    false -> command;
                elseif arglist matches [? name:isstring ',' ?? rest] then
                    ;;; window name given
                    false -> command;
                elseif arglist matches [??command:2 ',' ?? rest] then
                    vedget_new_name()-> name;
                else vederror('ILL FORMED WINDOW SPEC ' sys_>< vedargument)
                endif;
                rest -> arglist;

                ;;; find end of window

                if arglist == [] then vedscreenlength -> fin    ;;; last window
                elseif isinteger(front(arglist)) then
                    front(arglist) - 1  -> fin
                else start + veddefaultlength - 1 -> fin;
                    (fin + 1):: arglist -> arglist  ;;; for start of next window
                endif;
                min(fin,vedscreenlength) -> fin;
                if fin < start then
                    vederror('WINDOW '><wnum><'  BAD BOUNDS: '
                        >< start ><space >< fin)
                endif;

                createnewwindow(name, wnum, start, fin, false) -> win;

                if command then
                    vedgotowindow(wnum);
                    vedputcommand(command(1) sys_>< space sys_>< command(2));
                    vedredocommand();
                    ;;; catch any procedures stuck in the stream
                    while ved_char_in_stream matches
                        [?command:isprocedure ??ved_char_in_stream]
                    do command()    ;;; this can abort ved_newmap
                    endwhile;
                else Vedrefreswindow(wnum)
                endif;

            enduntil;
        endlblock;
        ;;; now fix up the files.
        vedgotowindow(1);
    endunless;
enddefine;

define newmap(string);
    ;;;for use outside VED
    lvars string;
    false -> vednewmapped;
    if vedvedname = nullstring then
        'temp' -> vedvedname;
    endif;
    chain(string,ved_newmap)
enddefine;


define ved_showmap;
    ;;; print out information about current windows
    pr(newline);
    appwindows(
        procedure(num, window);
        lvars num,window;
            printf(winstartof(window), nameof_window(window), num,
                'Window : %p  Name: %p  Start: %p\n')
        endprocedure);
enddefine;

define vedswitchwindowsize();
    if vednewmapped then
        not(ved_full_screen) -> ved_full_screen;
        if ved_full_screen then
            appwindows(procedure(num,win); lvars num,win;
                        false -> winrefreshed(win)
                        endprocedure);
            false -> vedupperfile;
            Vedrefreswindow(vedwindownum);
            ;;;vedsetonscreen(winfileof(windowof_number(vedwindownum)),false);
        else ved_redraw()
        endif;
    else vedsetwindow()
    endif
enddefine;

define windowstart(wnum);
    ;;; given a number, find the start of the window of that number
    lvars wnum,win=checkwindow(wnum);
    if isref(winstartof(win) ->> wnum) then cont(wnum) else wnum endif;
enddefine;

define updaterof windowstart(start, wnum, where);
    ;;; alter upper limit of window number wnum, and propagate changes
    ;;; may cause windows to become visible or invisible.
    lvars fin old start wnum where file win=checkwindow(wnum);
    unless wnum == vedwindownum then
        vedgotowindow(wnum);       ;;; ensure globals are set
    endunless;
    max(start,1) -> start;
    min(start,vedscreenlength) -> start;
    ;;; if wnum == 1 then 1 -> start endif;
    isvector(winfileof(win)) -> file;
    windowstart(wnum) -> old;
    if start == old and visible(wnum) then
        return();   ;;; no boundary to shift
    endif;
    winendof(win) -> fin;
    if start > fin  then
        if where == "fromabove" then
            if visible(wnum) then false -> visible(wnum); endif;
            if windowof_number(wnum + 1) then
                start -> windowstart(wnum + 1, where);
            endif;
            return();
        else
            vederror('CANT MAKE START AFTER END: win ' >< wnum)
        endif;
    endif;
    vedmarkcursorline(false);
    start -> winstartof(win);
    vedsetupwindow(win) ->;
    vedlineoffset + (start - old) -> vedlineoffset;
    if vedlineoffset < 0 then

        0 -> vedlineoffset; true -> winvisof(win);
        vedgotowindow(wnum);
        if file then vedrefreshscreenrange(start,fin) endif
    elseif not(visible(win)) then
        true -> visible(win);
        if windowof_number(wnum + 1) then
            windowend(wnum ) + 1 -> windowstart(wnum + 1,"fromabove");
        endif
    elseif start < old then
        vedgotowindow(wnum); ;
        if file then vedrefreshscreenrange(start,old) endif
    endif;
    ;;; alter preceding window(s)
    unless wnum == 1 or where == "fromabove" then
        start - 1 -> windowend(wnum - 1, "frombelow");
    endunless;
    vedgotowindow(wnum); ;
    if file then
            vedrefreshstatus();
    endif;
enddefine;

define windowend(wnum);
    ;;; given a number, find the end of the window of that number
    lvars wnum,win=checkwindow(wnum);
    winendof(win)
enddefine;

define updaterof windowend(newend, wnum,where);
    ;;; alter bottom boundary of win. If necessary inform other windows
    lvars start oldend newend wnum where oldlen file,
        win=checkwindow(wnum);
    min(newend, vedscreenlength ) -> newend;
    windowend(wnum) -> oldend;
    if oldend == newend and visible(wnum) then
        ;;; no need to adjust win, but if necessary make it visible
        return()
    endif;
    windowstart(wnum) -> start;
    oldend + 1 - start -> oldlen;
    if newend < start then   ;;; going up, covering this win
        if where == "frombelow" then
            if visible(wnum) then false -> visible(wnum); endif;
            unless wnum == 1  then
                newend -> windowend(wnum - 1, where);
            endunless;
            return();
        else
            vederror('CANT SET WINDOW END: WRONG START for win '>< wnum ><
            [^newend ^start])
        endif
    endif;
    isvector(winfileof(win)) -> file;
    newend -> winendof(win);
    unless where == "frombelow" then
        ;;; alter bounds of next win, and others if necessary.
        ;;; next win may become invisible or visible
        if windowof_number(wnum + 1) then
            newend + 1 -> windowstart(wnum + 1, "fromabove")
        endif
    endunless;
    vedgotowindow(wnum);
    if not(visible(wnum)) then
        true -> visible(wnum);
        unless wnum == 1 then
            windowstart(wnum) - 1 -> windowend(wnum - 1, "frombelow")
        endunless;
    elseif file and oldend < newend then
        vedrefreshscreenrange(oldend+1,newend);
    endif;
    if file and oldlen == 1 then vedrefreshstatus() endif;
enddefine;


define ved_wu;
    ;;; move window up at top, vedargument lines, possibly negative
    if vedargument = nullstring then 1 else strnumber(vedargument) endif
        -> vedargument;
    windowstart(vedwindownum) - vedargument -> windowstart(vedwindownum,false)
enddefine;

define ved_wd;
    ;;; move window down at bottom, vedargument lines, possibly negative
    if vedargument = nullstring then 1 else strnumber(vedargument) endif
        -> vedargument ;
    windowend(vedwindownum) + vedargument -> windowend(vedwindownum,false)
enddefine;

define ved_wed;
;;; Like ved_ved, but this allows optional integer argument
;;; forcing file to go in that window e.g.
;;;     <ENTER> wed 3 foo
;;; will move file foo to window 3 even if it was somewhere else
;;;     <ENTER> wed 3
;;; just goes to window 3, like <ENTER> |3
;;;     <ENTER> wed foo
;;; edit foo in the next window, unless it is already in a window

    lvars newfile, oldnum,win,isold=false,oldwin=false;
    vars _name, _wnum;  ;;; used in matcher. NOT LVARS

    unless vednewmapped then chain(oldved_ved)
    elseunless vedinvedprocess then ;;; not in VED
        vedinput(ved_redraw); chain(oldved_ved)
    endunless;

    sysparse_string(vedargument) -> vedargument;
    if vedargument == [] then
        if vedvedname = nullstring then vedget_new_name()
        else vedvedname
        endif -> _name;
        vedwindownum -> _wnum;
    elseif vedargument matches [? _wnum:isinteger] then
        false -> _name; ;;; use file in that window if possible
    elseif vedargument matches [? _name:isstring] then
        false-> _wnum;   ;;; no number specified try to infer it
    elseif vedargument matches [? _wnum:isinteger ? _name:isstring] then
    else
        err:
        vederror('ENTER wed <window number> <file name>')
    endif;
    if _name then
        Vedwinsearch(_name) -> newfile;     ;;; file structure or name
        unless isvector(newfile) then
            ;;; its a string. open new VED buffer
            vedgetready(newfile) -> newfile
        endunless;
        windowof_file(newfile) -> oldwin;
        if oldwin then
            winnumof(oldwin), winnameof(oldwin)
        else
            nextwindownum(vedwindownum), vedfullname(newfile)
        endif-> _name -> oldnum;
        if _wnum then
            checkwindow(_wnum) ->win;
            vedsetfileinwindow(newfile,win);
            if oldwin and _wnum /== oldnum then
                ;;; make sure there is something in the other file
                vedsetfileinwindow(vedgetready(vedget_new_name()), oldwin)
            endif;
        else
            checkwindow(oldnum->> _wnum) -> win;
            vedsetfileinwindow(newfile, win)
        endif;
    elseif _wnum then
        checkwindow(_wnum) -> win;
        winnameof(win) -> _name;
        winfileof(win) -> newfile;
    else goto err
    endif;
    nameof_window(win) -> _name;
    unless not(_name) or has_no_name(_name) then _name -> vedvedname endunless;
    vedgotowindow(_wnum);
    unless ved_full_screen or _wnum == oldnum then
        if vedwindowlength == 1 then
            nullstring -> vedargument;
            if windowstart(_wnum) == 1 then ved_wd() else ved_wu() endif;
        endif;
    endunless;
enddefine;

define procedure ved_q();
    lvars char goodfile,win;
    vars vedargument ;
    unless vednewmapped then chain(oldved_q) endunless;
    if vedonstatus then vedstatusswitch() endif;
    if vedchanged and (vedwriteable or vedwriteallfiles) then
        repeat forever
            vedputmessage(nullstring);
            vedputmessage('FILE CHANGED. WRITE IT? TYPE \'y\' OR \'n\', OR \'c\' TO CONTINUE ');
            uppertolower(vedinascii()) ->char;
            if char == `n` then ;;; do nothing
            elseif char == `c` then
                vedputmessage('CONTINUING TO EDIT ' >< vedcurrent); return
            elseif char == `y` then ved_w1();
            else
                sys_clear_input(poprawdevin);
                nextloop
            endif;
            quitloop;
        endrepeat;
    endif;
    vedputmessage('QUITTING '><vedcurrent);
    ;;; quit window
    windowof_number(vedwindownum) -> win;
    false ->> windowof_name(winnameof(win)) ->>winfileof(win) ->>
        ->> winnameof(win) ->vedcurrentfile;
    ;;; current file should be at front of vedbufferlist. Get rid of it.
    back(vedbufferlist) -> vedbufferlist;


    if vedbufferlist == [] then
        if vedfileprops == "teach" then
            'teach' -> vedteachname
        elseif vedfileprops == "help" then nullstring -> vedhelpname
        elseif vedfileprops == "ref" then nullstring -> vedrefname
        else nullstring -> vedvedname
        endif;
        false -> goodfile;
    else
        ;;; see if there is anything 'good' left
        vedget_good_file(true, true) -> goodfile
    endif;

    if goodfile then
        vedsetonscreen(goodfile, false);
    elseif not(ved_full_screen) and vedget_good_file(true, false) then
        ;;; good file in another window, so put an anonymous one here
        vedget_new_name() -> vedargument;
        vedsetonscreen(vedargument, false);
        if has_no_name(vedargument) then false -> vedwriteable; endif;
    else
        nullstring -> vedvedname;
        vedputmessage('NO MORE FILES'); ved_pop()   ;;; leave VED
    endif;
enddefine;


define ved_rb;
    ;;; find a file not on screen and put it on screen
    lvars file,goodfile,name, oldfile=vedcurrentfile;
    unless vednewmapped then chain(oldved_rb) endunless;
    false -> goodfile;
    SAVEGLOBALS;
    repeat
        ;;; make sure every file gets a chance to be found
        setfrontlist(last(vedbufferlist)->> file, vedbufferlist) -> vedbufferlist;
    quitif(file == oldfile);
        unless windowof_file(file) or has_no_name(vedfullname(file)) then
            file -> goodfile;
            quitloop()
        endunless;
    endrepeat;
    if goodfile then
        vedsetonscreen(goodfile,false);
    else
        oldfile -> vedcurrentfile; vedsetglobals(vedcurrentfile)
    endif;
enddefine;

define ved_xw();
    ;;; exchange current window contents with those of the
    ;;; named file, e.g. ENTER XW 3
    ;;;  ENTER XW foo.p
    ;;;  ENTER XW help foo
    lvars oldnum newnum oldname newname ;
    vars vedargument;
    ;;; argument may be number or file name
    nameof_number(vedwindownum ->> oldnum) -> oldname;
    if vedargument = nullstring then
        if vedwindownum = 1 then 2 else vedwindownum - 1 endif
            ->> vedargument -> newnum
    endif;
    if isinteger(vedargument) or (strnumber(vedargument)->> newnum) then
        checkwindow(newnum) ->;
        nameof_number(newnum) -> newname;
    else
        Vedwinsearch(sysparse_string(vedargument)) -> newname;
        if isvector(newname) then vedfullname(newname) -> newname
        endif;
        numberof_name(newname) -> newnum;
        checkwindow(newnum) ->;
    endif;
    newnum >< space >< oldname -> vedargument; ved_wed();
    oldnum >< space >< newname -> vedargument; ved_wed();
    return;
enddefine;

;;; re-define some system procedures;
define vedrestorescreen;
    lvars oldset=vedsetonscreen;
    dlocal vedsetonscreen;
    unless vednewmapped then chain(oldvedrestorescreen) endunless;
    if vedprintingdone then
        #_< erase<>erase >_# -> vedsetonscreen; ;;; suppress temporarily
        oldvedrestorescreen();
        oldset -> vedsetonscreen;               ;;; reset it temporarily
        if ved_full_screen then vedrefresh() else ved_redraw() endif;
    endif
enddefine;


define vedrestorewindows;
    if vednewmapped then
        false -> vedprintingdone;
        if ved_full_screen then vedrefresh() else ved_redraw() endif
    else
        oldvedrestorewindows()
    endif;
enddefine;

define ved_|;
    ;;; get an arbitrary command obeyed in a given window
    ;;; e.g. <ENTER> |3 showlib windows
    lvars num, oldfile = vedcurrentfile,
        loc=locchar(`\s`,1,vedargument);
    strnumber( if loc then substring(1,loc-1,vedargument) else vedargument
            endif) -> num;
    if num and windowof_number(num) then
        vedgotowindow(num);
        if loc then
            vedputcommand(allbutfirst(loc,vedargument));
            vedredocommand();
            vedputcommand('|' sys_>< vedargument);
        endif;
        if oldfile and lmember(oldfile, vedbufferlist)
        and oldfile /== vedbufferlist(1) and oldfile /== vedbufferlist(2)
        then
            setfrontlist(oldfile, back(vedbufferlist)) -> back(vedbufferlist)
        endif
    else
        vedwindowerr(if loc then substring(1,loc,vedargument) else vedargument
            endif)
    endif;
enddefine;

sysunprotect("vedswapfiles");
define vedswapfiles();
    ;;; modified version

    if vednewmapped then
        if back(vedbufferlist) == [] then
            vederror('NO FILE TO SWAP')
        else
            vedsetonscreen(vedbufferlist(2), false)
        endif;
    else
        oldvedswapfiles()
    endif
enddefine;

define global ved_wstop();
    ;;; like ved_stop, but refreshes whole screen on re-entry

    ;;; THIS HAS TO BE ALTERED FOR NON-BERKELEY 4.2/3 SYSTEMS
    lconstant STOP = SIG_TSTP;

    dlocal vedstartwindow vedscreenbell;

    if vednewmapped then
    SAVEGLOBALS;
    vedscreenreset();
    charout(`\n`);    ;;; this forces stty for popdevout i.e. non-raw
    identfn -> vedscreenbell;
    sys_send_signal(poppid,STOP) -> ;  ;;; suspend process
    ;;; next part runs only after process is restarted
    false -> vedprintingdone;
    if windowstart(1)/==1 then
        vedscreenreset();
        nl(vedscreenlength - windowstart(1) - 4);   ;;; push printing up
        false -> vedprintingdone;
    endif;
    if ved_full_screen then vedrefresh() else ved_redraw() endif
    else
        valof("ved_stop")() ;;; autoload when necessary
    endif;
enddefine;

sysunprotect("ved");
define syntax ved;
    if vednewmapped then
        sysPUSH("ved_redraw");
        sysCALL("vedinput");
    endif;
    popvedcommand("ved_ved");
enddefine;

sysprotect("ved");

vedsetkey('\^[w',   "vedswitchwindowsize");     ;;; ESC w
vedsetkey('\^[x',   "vedswapfiles");
vedsetkey('\^[;',   "vednextwindow");           ;;; ESC ;
vedsetkey('\^[\^V', "ved_redraw");              ;;; ESC CTRL-V
vedsetkey('\^[q',   "ved_q");                   ;;; ESC CTRL-q
vedsetkey('\^[0',   "ved_wstop");               ;;; ESC 0

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jul 27 1999
    renamed vedrefreshwindow as Vedrefreshwindow, to avoid clash with system version.
--- Aaron Sloman, May  3 1991
    Made to use sigdefs.ph and SIG_TSTP to improve portability
--- Aaron Sloman, Nov 27 1990
    Changed vedsetonscreen to use vedinputfocus
--- Aaron Sloman, Nov 26 1990
    Had to change nonmac ved to nonsyntax ved
--- Aaron Sloman, Oct 16 1990
        vedfileselect could screw up windows if set up a special window
            for its menu. Altered so thatin that case it makes
            ved_full_screen true. Users can later do ESC w to contract.

        Added modified vedrestorewindows. (Needed e.g. for ved_sourcefile)

        Redefined vedswapfiles to replace system vedswapfiles for ESC x

        Totally re-wrote HELP * WINDOWS, including  summary of changes
        for V14

--- Aaron Sloman, Oct  8 1990
        Changed vednextwindow to skip useless windows, if no number typed

--- Aaron Sloman, Oct  7 1990
    ENTER wed <file>
        changed so that if file is not already in any window then
        it is put into the _next_ window in cycle

    ENTER | <command>
        changed so that vedswapfiles works properly after it

--- Aaron Sloman, Oct  5 1990
    Permits ENTER newmap off, to turn off multi-window mechanism.

    Altered keybindings. ESC x reverts to vedswapfiles, ESC ;
        does the cycling round windows.
    Assigned ved_wstop toESC 0, and made it work properly if
        vednewmapped false
    Added top level macro -ved- to get back to mapped screen.

    Replaced vednullstring with nullstring

    Fixed various things that did not work right. Entailed major
    re-organisation of some bits.

    ved_redraw now creates new temporary files if there are not enough
    files for the windows.

    Made ved_newmap re-initialise windownumbers

    Redefined vedfullname in terms of vedbuffername

--- Aaron Sloman, Oct  5 1990
        removed -ved_wq-. Now in the system.
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevin-,
        and -sys_purge_terminal- with -sys_clear_input-
--- Aaron Sloman, May 21 1986 fixed initialisation using newmap
--- Aaron Sloman, May 11 1986 fixed minor bug in vedsetonscreen, to do with
    ved_full_screen true
--- Aaron Sloman, May  6 1986
    vedsetlinevisible redundant: removed
--- Aaron Sloman, May  6 1986
    Added ved_|, vedwindowerr, ved_wstop, and substantially re-organised
    replaced ved_ved with ved_wed. Re-wrote documentation.
--- Aaron Sloman, Apr 30 1986
    Added ved_wstop, and cleaned up vedsetonscreen, vedredraw, etc.
--- Aaron Sloman, Apr 29 1986
    cleaned up interaction with vedputmessage and other things.
    Still too messy/
--- Aaron Sloman, Apr 27 1986

    A host of bugs fixed, documentation re-written, and can now be loaded
    in advance of running VED. For details see HELP * WINDOWS
*/
