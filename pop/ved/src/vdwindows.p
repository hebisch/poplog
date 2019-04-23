/* --- Copyright University of Sussex 1997.  All rights reserved. ---------
 > File:        C.all/ved/src/vdwindows.p
 > Purpose:     Manipulating windows on screen
 > Author:      Aaron Sloman & John Gibson (see revisions)
 */

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedrefreshrange, vedrefreshstatus, vedstatusswitch,
        ved_try_search, vedinsertstring, vedinascii, vedpresent, vedqget
        )
    ;

vars
        procedure (vedrestorescreen, vedsetonscreen,
        vedscreenscrollregionup, vedscreenscrollregiondown,
        vedscreencooked, ved_qved, vedsetnormalmessage, vedsysfile,
        vedvscr_set_wlinewidth
        ),
        vednamestring, vedscreenwidth, wvedwindowchanged,
    ;

section $-Sys$-Ved;

constant
        procedure (Quit_file, Clear_statusline, Safe_search,
        Mark_cursor_line, Unmark_cursor_line, Set_sysfile_defaults)
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Ved =>
                vedupperfile, vedlowerfile, vvedgotoplace, vedwarpcontext,
                wvedalwaysraise, wvedbreaktofit,

                wved_set_input_focus, wved_destroy_window, wved_create_window,
                wved_open_window, wved_close_window, wved_raise_window,
                wved_window_size, wved_is_live_window, wved_is_open_window,
                wved_window_label, wved_icon_label,

                vedstartwindow, wved_window_of_file, wved_file_of_window,
                wved_should_warp_mouse,
                vedsetonscreen, vedsetwindow, vedswapfiles,
                ved_xdn, ved_xup, ved_rb,
                vedrestorewindows, vedfileselect, vedfileisonscreen,
                wved_ved_set_window, wved_set_size,
                vedusewindows
            ;

vars
    vedupperfile    = false,    ;;; the one at top of screen
    vedlowerfile    = false,    ;;; the one at the bottom of the screen
    vvedgotoplace   = false,    ;;; where to start editing (integer string or false)
    vedwarpcontext  = true,     ;;; contexts in which to warp pointer

    ;;; if true, expose a window whenever user starts typing into it
    wvedalwaysraise = false,

    ;;; if true, reset the point at which ved breaks lines whenever the
    ;;; window is resized
    wvedbreaktofit  = false,

    Using_windows   = false,    ;;; whether using windows or not
    ;

define lconstant not_using_windows =
    mishap(% 0, 'NOT USING Ved WINDOWS' %)
enddefine;

vars procedure (
    wved_set_input_focus    = not_using_windows,
    wved_destroy_window     = not_using_windows,
    wved_create_window      = not_using_windows,
    wved_open_window        = not_using_windows,
    wved_close_window       = not_using_windows,
    wved_raise_window       = not_using_windows,
    wved_window_size        = not_using_windows,
    wved_is_live_window     = not_using_windows,
    wved_is_open_window     = not_using_windows,
    wved_window_label       = not_using_windows,
    wved_icon_label         = not_using_windows,
    );

;;; vedstartwindow:
;;;     size of initial window when a new file is set on screen.
;;;     Active variable, maintained as a proportion of the screen length.
;;;     The actual proportion is the ratio of the two local variables
;;;         startwindow_size / startwindow_base
;;;     We have to do the computations long-hand to avoid forcing inclusion
;;;     of ratios.

lvars
    startwindow_size    = 12,
    startwindow_base    = 24,
;

/*
The next active variable represents the amount of screen that should
be used for the lower window. It is an active variable so that if
the screen size is ever changed, then the ratio between lower and upper
window will be maintained.
*/
define vars active vedstartwindow();
    (2 fi_* startwindow_size fi_* vedscreenlength fi_+ startwindow_base)
     fi_div
    (2 fi_* startwindow_base);
enddefine;

define updaterof vars active vedstartwindow(n);
    lvars n;
    Check_integer(n,false);
    if n fi_< 0 then
        0
    elseif n fi_> vedscreenlength then
        vedscreenlength
    else
        n
    endif -> startwindow_size;
    ;;; new size is interpreted relative to the current screen length
    vedscreenlength -> startwindow_base;
enddefine;


define vars wved_window_of_file(file);
    ;;; depends on macro defined in vddeclare.ph
    lvars file;
    subscrv(VF_WINDOW, file)
enddefine;
;;;
define updaterof wved_window_of_file(file);
    lvars file;
    -> subscrv(VF_WINDOW, file)
enddefine;


;;; return the file which is using the given window
define vars wved_file_of_window(win) -> file;
    lvars win file;
    for file in vedbufferlist do
        returnif (win == wved_window_of_file(file))
    endfor;
    false -> file;
enddefine;

;;; Test for whether cursor needs to be warped to new window
define vars wved_should_warp_mouse(word);
    lvars word, context = vedwarpcontext;
    USEWINDOWS and context
        and (not(islist(context)) or lmember(word, context))
enddefine;

define Set_screen_width();
    lvars width = vedscreenwidth;
    ;;; leave a free space at the right hand edge of the screen if the
    ;;; terminal autowraps
    if vedscreenwrap then width - 1 ->> width -> vedscreenwidth endif;

    vedvscr_set_wlinewidth();       ;;; set vedwlinewidth

    ;;; adjust poplinemax & poplinewidth for LMR and IM
    width - 2 -> poplinemax;
    (7 * width) >> 3 -> poplinewidth;   ;;; Ie. 7/8 of width
enddefine;

define wved_set_size(window);
    ;;; set ved variables to correspond to the window
    lvars window, cols;
    dlocal ved_on_status = false;

    wved_window_size(window) -> (vedscreenwidth, vedwindowlength);
    Set_screen_width();
    vedwindowlength -> vedscreenlength;

    if wvedbreaktofit then
        vedscreenwidth - 2 -> vedlinemax    ;;; adjust break point
    endif
enddefine;

    ;;; Last file for which we did a vedsetonscreen for XWINDOWS
    ;;; (not necessarily the current). Used to decide which other file
    ;;; to tidy in vedsetonscreen, and which file to use for vedputmessages
    ;;; during vedreadfile.
define lconstant Last_setonscreen() -> file;
    lvars file, l = vedbufferlist;
    ;;; vedupperfile could be a pair after a quit
    if ispair(vedupperfile) or l == []
    or not(wved_is_live_window(wved_window_of_file(hd(l) ->> file)))
    then
        false -> file
    endif
enddefine;


;;; ------------------------------------------------------------------------

define lconstant Set_current_file(file) -> wasonstatus;
    lvars file, wasonstatus;
    file -> ved_current_file;
    ved_on_status -> wasonstatus;
    false -> ved_on_status
enddefine;

define lconstant Check_status(wasonstatus);
    lvars wasonstatus;
    Unmark_cursor_line();
    if wasonstatus ->> ved_on_status then
        Mark_cursor_line()
    endif;
    Set_wait_cursor(true, true)
enddefine;

    /*  This finishes off the work done by vedsetonscreen
    */
define lconstant Set_window_rest(redisplay, initarg, wasonstatus);
    lvars redisplay, initarg, wasonstatus;

    ;;; change defaults for VED REF TEACH and HELP commands.
    unless testdef vedsysfile
    and weakref[vedsysfile] Set_sysfile_defaults(true) then
        vedcurrent -> vedvedname
    endunless;

    vedinitfile();

    if redisplay then Clear_statusline() endif;

    if vvedgotoplace then
        ;;; make sure vvedgotoplace is false first in case something
        ;;; goes wrong (e.g. ved_try_search causes a mishap).
        lvars place = vvedgotoplace;
        false -> vvedgotoplace;
        if isinteger(place) then
            true -> redisplay;          ;;; ensure screen is refreshed
            fi_min(place, vvedbuffersize) -> place;
            if place fi_> 0 then
                vedjumpto(place, vedcolumn);
                if vvedbuffersize fi_> vedwindowlength then
                    place fi_- 1 -> vedlineoffset
                endif
            endif
        elseif place = nullstring then
            '\{b}no search string' -> initarg
        elseif Safe_search(place, [noembed], ved_try_search) then
            true -> redisplay;          ;;; ensure screen is refreshed
            if vvedbuffersize fi_> vedwindowlength then
                vedline fi_- 1 -> vedlineoffset
            endif
        else
            place <> ' \{b}not found' -> initarg
        endif
    endif;

    if isprocedure(initarg) then
        fast_apply(redisplay, initarg) -> (redisplay, initarg)
    endif;

    if redisplay and not(isstring(initarg) and initarg /= nullstring) then
        vednamestring <> (',\s' sys_>< vvedbuffersize sys_>< '\slines')
                    -> initarg
    endif;

    if isstring(initarg) then
        vedputmessage(initarg)
    else
        nullstring -> vedmessage;
        vedsetnormalmessage(true)
    endif;

    ;;; despite its name, this calls vedrefreshwindow
    if redisplay then vedalignscreen() endif;

    ved_current_file -> vedinputfocus;
    Check_status(wasonstatus)
enddefine;

    /*  Tidy up the last file for which we did a -vedsetonscreen-
        (not necessarily the current)
    */
define lconstant Tidy_last_setonscreen(nextfile);
    lvars nextfile, other;
    if (Last_setonscreen() ->> other) and other /== nextfile then
        Check_status(Set_current_file(other));
        vedsetnormalmessage(true)
    endif
enddefine;

    /*  This is used to tidy an old window before going to a new one
        Used by Set_new_window.
    */
define lconstant Tidy_other_window(other, initarg, isupper);
    lvars other, initarg, isupper, wasonstatus;

    ;;; tidy up the other window on the screen before leaving it
    Set_current_file(other) -> wasonstatus;

    Check_status(wasonstatus);
    vedputmessage(if initarg = nullstring then nullstring
                  else vednamestring
                  endif);

    procedure;
        dlocal ved_on_status = false;
        Mark_cursor_line();    ;;; show the current line, in other window
        ;;; correct the window length for the other file
        if vedwindowlength fi_>= vedscreenlength then
            if isupper then vedstartwindow
            else vedscreenlength fi_- vedstartwindow
            endif -> vedwindowlength;
        endif;
    endprocedure()
enddefine;

    /*  set the given file on the screen, as upper or lower file, with the
        given text on the status line. If it isn't an old file, i.e. already on
        the screen, then redisplay the whole file.  if vvedgotplace is an
        integer then go to that line, if its a string then search for it.
    */
define lconstant Set_new_window(file, isupper, initarg, redisplay);
    lvars   file, isupper, initarg, redisplay, onscreen, other,
            wasonstatus, _offset, _scrollup;

    if vedprintingdone then
        ;;; if vedprintingdone, then restore screen
        file -> ved_current_file;
        chain(vedrestorescreen, fast_chain)
    endif;

    ;;; check if file already in place.
    if isupper and file == vedupperfile
    or (not(isupper) and file == vedlowerfile) then file
    else false
    endif -> onscreen;

    if vedupperfile or vedlowerfile then
        ;;; remove any 'dead' windows left by Quit_file
        if ispair(vedupperfile) then false -> vedupperfile endif;
        if ispair(vedlowerfile) then false -> vedlowerfile endif;
        false -> _scrollup
    else
        ;;; no windows set up
        false -> isupper;   ;;; start new file at the bottom
        true -> _scrollup;  ;;; will ensure previous printing remains visible
    endif;

    if not(onscreen) and vedstartwindow fi_>= vedscreenlength then
        true -> isupper;
        false ->> _scrollup -> vedlowerfile;
    else
        if isupper and isvector(vedlowerfile) then
            vedlowerfile
        elseif not(isupper) and isvector(vedupperfile) then
            vedupperfile
        else
            false
        endif -> other;
        if other then
            Tidy_other_window(other, initarg, isupper);
        endif;
    endif;

    MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;

    if _scrollup then
        ;;; preserve any printing on the screen, but shift it up
        vedscreenxy(1, vedscreenlength);
        vedscreencleartail();   ;;; there will not be anything useful on bottom line
        repeat vedstartwindow fi_- 1 times vedscr_char_out(`\n`) endrepeat;
        vedscreenlength fi_- vedstartwindow
    elseif isupper then
        0
    else
        vedscreenlength fi_- vedstartwindow
    endif -> _offset;

    setfrontlist(file, vedbufferlist) -> vedbufferlist;
    Set_current_file(file) -> wasonstatus;
    file -> if isupper then vedupperfile else vedlowerfile endif;

    unless onscreen then
        _offset -> vedscreenoffset;
        if vedstartwindow fi_>= vedscreenlength then
            vedstartwindow
        elseif isupper then
            vedscreenlength fi_- vedstartwindow
        else
            vedstartwindow
        endif -> vedwindowlength
    endunless;

    chain(redisplay, initarg, wasonstatus, Set_window_rest)
enddefine;

define lconstant Set_top_screen(file, initarg, redisplay);
    ;;; if redisplay is false then only reprint the 'status' line
    lvars file, initarg, redisplay;
    if file == vedlowerfile then false -> vedlowerfile endif;
    Set_new_window(file, true, initarg, redisplay)
enddefine;

define lconstant Set_bottom_screen(file, initarg, redisplay);
    lvars file, initarg, redisplay;
    if file == vedupperfile then false -> vedupperfile endif;
    Set_new_window(file, false, initarg, redisplay)
enddefine;

    /*  Put file on the screen.
        initarg gets passed to Set_window_rest and controls the first message
        displayed.
    */
define vars vedsetonscreen(file, initarg);
    lvars file, initarg, window, redisplay, wasonstatus;
    dlocal popgctrace = false, vedediting = true;

    returnunless(file);
    Check_vedfile(file);

    if USEWINDOWS then

        ;;; Tidy up the last file for which we did a vedsetonscreen
        ;;; (not necessarily the current)
        Tidy_last_setonscreen(file);

        setfrontlist(file, vedbufferlist) -> vedbufferlist;
        Set_current_file(file) -> wasonstatus;

        lvars warp = wved_should_warp_mouse("vedsetonscreen");
        if wved_is_live_window(wvedwindow) then
            if wvedfreewindow then wved_destroy_window(wvedfreewindow) endif;
            if warp and wvedalwaysraise then
                wved_open_window(wvedwindow)
            endif;
            false
        else
            ;;; get a new window -- returns false if window started iconic
            wved_create_window() and warp -> warp;
            true
        endif -> redisplay;

        if warp then true -> wvedwindowchanged endif;
        Set_window_rest(redisplay, initarg, wasonstatus)

    else
        if file == vedupperfile then
            Set_top_screen(file, initarg, false)
        elseif file == vedlowerfile then
            Set_bottom_screen(file, initarg, false)
        else
            lvars currfile = ved_current_file;
            unless isvector(currfile) then false -> currfile endunless;
            procedure;
                dlocal ved_on_status = false;
                if currfile
                and (currfile == vedupperfile or currfile == vedlowerfile)
                and vedwindowlength fi_> vedstartwindow then
                    ;;; make sure cursor remains in visible part of window
                    if vedline fi_>
                    (vedlineoffset fi_+ (vedscreenlength fi_- vedstartwindow)
                            fi_- 1)
                    then
                        false -> vedupperfile;
                        ved_current_file -> vedlowerfile;
                        vedstartwindow -> vedwindowlength;
                        vedlineoffset
                            fi_+ (vedscreenlength fi_- vedstartwindow)
                                -> vedlineoffset;
                        vedscreenlength fi_- vedstartwindow -> vedscreenoffset;
                        MAXSCREENCOL -> vedscreenline;
                        vedrefreshstatus()
                    endif
                endif
            endprocedure();
            if currfile and currfile == vedlowerfile then
                Set_top_screen
            elseif currfile and currfile == vedupperfile then
                Set_bottom_screen
            elseif isvector(vedlowerfile) then
                Set_top_screen
            else
                Set_bottom_screen
            endif(file, initarg, true)  ;;; true because not already on screen
        endif
    endif
enddefine;

define Setonscreen_next(ignore_first, initarg);
    lvars ignore_first, initarg, list = vedbufferlist, file = false, win;
    if ignore_first and list /== [] then tl(list) -> list endif;
    if XWINDOWS then
        ;;; try not to go back to an iconic file if possible
        for file in list do
            wved_window_of_file(file) -> win;
            quitif(wved_is_live_window(win) and wved_is_open_window(win));
            false -> file
        endfor
    endif;
    unless file then
        returnif(list == []);
        hd(list) -> file
    endunless;
    chain(file, initarg, vedsetonscreen)
enddefine;

define Set_basewindow(context);
    lvars context;
    if USEWINDOWS then
        Tidy_last_setonscreen(false);
        if wvedalwaysraise then wved_open_window("basewindow") endif;
        if wved_should_warp_mouse(context) then
            wved_set_input_focus("basewindow")
        endif
    else
        vedscreenreset()
    endif
enddefine;


define vars wved_ved_set_window();
    vedscreenbell();
enddefine;

define vedsetwindow();
    ;;; make current window fill whole screen, or reduce to half size

    define lconstant Setwindow();
        lvars   currfile, _oldlen, _offset;
        dlocal  vedstartwindow, ved_on_status = false;
        vedwindowlength -> _oldlen;
        ved_current_file -> currfile;
        if _oldlen == vedscreenlength then
            false -> vedupperfile;      ;;; Make it lower window
            unless vedstartwindow fi_< vedscreenlength then
                vedscreenlength >> 1 -> vedstartwindow;
            endunless;
            Bottom_of_window() fi_- vedstartwindow fi_+ 1 -> _offset;
            if _offset fi_< vedline then
                _offset -> vedlineoffset;
                ;;; cursor in new window position, so don't refresh whole screen
                currfile -> vedlowerfile;
                vedstartwindow -> vedwindowlength;
                vedscreenlength fi_- vedwindowlength -> vedscreenoffset;
                vedrefreshstatus();
            elseif terminal_can_scroll then
                currfile -> vedlowerfile;
                0 -> vedscreenoffset;
                repeat vedscreenlength fi_- vedstartwindow times
                    vedscreenscrollregiondown(1, vedscreenlength);
                endrepeat;
                vedscreenlength fi_- vedstartwindow -> vedscreenoffset;
                vedstartwindow -> vedwindowlength;
            else
                false -> vedlowerfile;
                chain(currfile, false, true, Set_bottom_screen)
            endif;
        else
            false -> vedlowerfile;
            if currfile == vedupperfile then
                vedscreenlength -> vedwindowlength;
                vedrefreshrange(vedlineoffset fi_+ _oldlen, Bottom_of_window(), undef)
            else
                currfile -> vedupperfile;
                vedscreenlength -> vedwindowlength;
                0 -> vedscreenoffset;
                if terminal_can_scroll then
                    repeat vedscreenlength fi_- _oldlen times
                        vedscreenscrollregionup(1, vedscreenlength);
                    endrepeat;
                    vedrefreshrange(vedlineoffset fi_+ _oldlen, Bottom_of_window(), undef)
                else
                    vedrefresh();
                endif
            endif;
        endif
    enddefine;

    if USEWINDOWS then
        ;;; XVed changes size of window
        wved_ved_set_window()
    else
        Setwindow()
    endif
enddefine;

define vedswapfiles();
    lvars file, winchanged = wvedwindowchanged;

    unless listlength(vedbufferlist) fi_>= 2 then
        vederror('\{b}no file to swap')
    endunless;

    Setonscreen_next(true, false);

    ;;; now consider whether to have the input focus changed.
    if wved_should_warp_mouse("vedswapfiles") then
        true
    else
        winchanged
    endif -> wvedwindowchanged;
    Check_status(false)
enddefine;

define vars ved_xdn();
    ;;; eXchange and DownN: scroll other window down then come back
    lvars temp, waschanged = wvedwindowchanged;
    ;;; suppress bringing other window up
    dlocal vedwarpcontext = false, wvedalwaysraise = false;

    vedswapfiles();

    vedjumpto(Bottom_of_window(),vedcolumn);
    if vvedbuffersize fi_< vedline then
        true
    else
        vedscreendown(); vedcheck(); false
    endif -> temp;
    vedswapfiles();
    waschanged -> wvedwindowchanged;
    if temp then
        vedputmessage('\{b}end of other file'); vedscreenbell();
    endif;
enddefine;

    ;;; scroll other screen up a windowful
define vars ved_xup();
    lvars waschanged = wvedwindowchanged;
    ;;; suppress bringing other window up
    dlocal vedwarpcontext = false, wvedalwaysraise = false;

    vedswapfiles();

    vedlineoffset fi_+ 1 -> vedline;
    if vedline fi_> 1 then
        vedscreenup(); vedcheck();
    endif;
    vedswapfiles();
    waschanged -> wvedwindowchanged;
enddefine;


    /*  If the given file is in a window, leave behind a pair containing the
        window or screenoffset and statusline
    */
define Set_residual_window();
    lvars file = ved_current_file, other;
    if XWINDOWS and (Last_setonscreen() ->> other) and other /== file then
        conspair(other(VF_WINDOW), other(VF_STATUSLINE)) -> vedupperfile
    elseif file then
        conspair(if XWINDOWS then wvedwindow else vedscreenoffset endif,
                                                    vedstatusline)
            ->  if file == vedupperfile then vedupperfile
                elseif file == vedlowerfile then vedlowerfile
                else
                endif
    endif
enddefine;

    ;;; rotate buffers
define vars ved_rb();
    lvars waschanged = wvedwindowchanged;
    unless USEWINDOWS then
        Set_residual_window()   ;;; remove current from window
    endunless;
    vedsetonscreen(vedbufferlist(listlength(vedbufferlist)), false);

    ;;; now consider whether to have the input focus changed.
    waschanged or wved_should_warp_mouse("ved_rb") -> wvedwindowchanged
enddefine;

define vars vedrestorewindows();
    lvars file;
    returnunless(vedediting);
    vedscreencooked();
    ved_current_file -> file;
    if isvector(vedupperfile) then
        vedupperfile -> ved_current_file;
        vedrefresh();
    endif;
    if isvector(vedlowerfile) then
        vedlowerfile -> ved_current_file;
        vedrefresh();
    endif;
    file -> ved_current_file;
    false -> vedprintingdone
enddefine;


;;; vedfileselect mechanisms taken from LIB VEDFILESELECT by Mark Rubinstein
;;;

define vedfileselect();
    lvars   nfiles, c, n, fstring, inselect, winchanged = wvedwindowchanged,
            warping = wved_should_warp_mouse("vedfileselect");
    dlvars  attr;
    dlocal  0 % if dlocal_context == 1 then false -> inselect endif,
                if inselect and dlocal_context == 2 then Quit_file(false) endif
              %;

    define lconstant hexchar(n);
        lvars n;
        n fi_+ (if n fi_<= 9 then `0` else `7` endif)
    enddefine;

    define lconstant addattr();
        () fi_|| attr fi_|| VEDCMODE_ACTIVE
    enddefine;

    define lconstant Make_select_string();
        lvars f, n = 1, c;
        `\[b7]` -> attr;
        consvedstring(#|
            for f in vedbufferlist do
                hexchar(n) -> c;
                addattr(`\s`);
                conspair(addattr(c), consstring(#|explode('(ip)>'), c|#));
                addattr(`\s`);
                appdata(sys_fname_namev(f(VF_NAME)), addattr);
                addattr(`\s`);
                `\s`;
                n fi_+ 1 -> n
            endfor ->           ;;; erase final space
        |#);
    enddefine;

    define lconstant Getnumber(_char, _filecount);
        lvars _char, _filecount;
        max(if _filecount fi_< 10 then
                _char fi_- `0`
            else    ;;; convert what may be a non-numeric label
                uppertolower(_char) -> _char;
                if (_char - `0` ->> _char) fi_>= 10 then
                    _char fi_- 39
                else
                    _char
                endif
            endif, 1)
    enddefine;

    /* Quick_fileselect does no displaying but goes quickly to the file
     * appropriate for the character n given nfiles files.
     * Run by chain outside of vedfileselect
     */
    define lconstant Quick_fileselect(n, nfiles, warping);
        lvars n, nfiles, warping;

        if (Getnumber(n, nfiles) ->> n) fi_> nfiles then
            vederror('\{b}not enough files')
        else
            dlocal vedwarpcontext = false;
            vededit(vedbufferlist(n));
            if warping then true -> wvedwindowchanged endif;
        endif;
    enddefine;

    if (listlength(vedbufferlist) ->> nfiles) fi_< 2 then
        vederror('\{b}only editing one file');
    endif;

    syssleep(40);   ;;; Added by A.Sloman 12 Jul 1999

    if vedinputwaiting() then
        ;;; don't display message if char is waiting
        chain(vedinascii(), nfiles, warping, Quick_fileselect)
    else
        Make_select_string() -> fstring;    ;;; make string and display if short
        if datalength(fstring) < vedscreenwidth - #_< STATUS_HEADER_LEN+5 >_#
        then
            vedputmessage(fstring);
            nullstring -> vedmessage;
            chain(vedinascii(), nfiles, warping, Quick_fileselect)
        endif
    endif;

    define lconstant Get_files(setonscr);
        lvars f, n = 1, c, len, name, setonscr;
        `\[6]` -> attr;
        nullstring;
        for f in vedbufferlist do
            consvedstring(#|
                hexchar(n) ->> c, explode('\s\s\s');
                f(VF_NAMESTRING) -> name;
                allbutfirst(locchar(`\s`,1,name), name) -> name;
                appdata(name, addattr);
                datalength(name) -> len;
                conspair(subscr_stack(len),
                            consstring(#|explode('(bp)>'), c|#))
                                    -> subscr_stack(len)
            |#);
            n fi_+ 1 -> n
        endfor;
        consvector(n)
    enddefine;

    vededit(consref({['fileselect\s' Select\.fileselect] ^Get_files}),
                                                vedhelpdefaults);
    true -> inselect;

    if USEWINDOWS then
        if wved_should_warp_mouse("vedfileselect") then
            ;;; change input window so that it will change back later
            wved_set_input_focus(wvedwindow)
        endif;
    elseif vedwindowlength < nfiles fi_+ 3
    and vedwindowlength < vedscreenlength then
        ;;; make the window suitably large
        vedsetwindow()
    endif;

    vedputmessage(if USEWINDOWS then '\{b}click file or type label:'
                    else '\{b}type file label:'
                    endif);
    nullstring -> vedmessage;
    vedclearinput();
    until (Getnumber(vedinascii(), nfiles) ->> n) fi_<= nfiles do
        vedscreenbell()         ;;; number not acceptable
    enduntil;
    false -> inselect;
    vedqget(vededit(%vedbufferlist(n + 1)%), true);
    warping or winchanged -> wvedwindowchanged
enddefine;

define Open_file_window();
    if USEWINDOWS and wved_is_live_window(wvedwindow) then
        wved_open_window(wvedwindow)
    endif
enddefine;

    /*  file is a string or Ved file structure.
        Return false if file has no associated window, or true if it
        has (and window contents must be updated to reflect file changes).
    */
define vedfileisonscreen(file);
    lvars file, window;

    if isstring(file) then vedpresent(file) -> file endif;

    returnunless(file)(false);
    Check_vedfile(file);

    if USEWINDOWS then
        wved_is_live_window(wved_window_of_file(file))
    else
        ;;; Single window, possibly split
        file == vedupperfile or file == vedlowerfile
    endif
enddefine;


;;; --- STARTING UP WINDOWS --------------------------------------------

define active vedusewindows();
    Using_windows
enddefine;
;;;
define updaterof active vedusewindows(newval);
    lvars newval, new_p, old_p;

    define lconstant get_change_p(w);
        lvars w, p, name, id;
        consword('wved_change_usewindows_' sys_>< w) -> name;
        (isdefined(name) ->> id) and not(isundef(idval(id) ->> p))
        and isprocedure(p) and p
    enddefine;

    returnif(newval == Using_windows);

    if vedinvedprocess or vedbufferlist /== [] then
        vederror('\{b}can\'t switch Ved window system while editing')
    endif;

    if newval then
        Check_word(newval);
        unless get_change_p(newval) ->> new_p then
            mishap(newval, 1, 'INVALID VALUE FOR vedusewindows (window system unknown or not loaded)');
        endunless
    endif;

    if Using_windows then
        unless get_change_p(Using_windows) ->> old_p then
            mishap(Using_windows, 1, 'SYSTEM ERROR IN vedusewindows')
        endunless;
        false -> vedprintingdone;
        old_p(false, ident Using_windows)   ;;; will set Using_windows
    endif;

    if newval then
        false -> vedprintingdone;
        new_p(newval, ident Using_windows)  ;;; will set Using_windows
    endif
enddefine;


endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 12 Jul 1999
        Removed spurious true in call to vedputmessage, and added
        syssleep in vedfileselect.
--- John Gibson, Aug 15 1997
        Changes for XVed variable-width mode
--- John Gibson, May  2 1997
        Changed wved_set_size to call Set_screen_width, and removed
        setting of vedstatusline (now relies on Ens*ure_statusline in
        vdstatus.p)
--- John Gibson, Sep 12 1996
        Quit_file argument now reversed; added extra bool arg to call of
        vedqget.
--- John Gibson, Apr 16 1996
        Rewrote vedfileselect to use text actions etc.
--- John Gibson, Jan 17 1996
        Removed wved_g*et_one_input
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- Robert John Duncan, Oct  6 1995
        Made ved_xup and ved_xdn behave the same w.r.t. raising the other
        window
--- John Gibson, Aug 22 1995
        In vedsetonscreen, made XVed test for calling wved_open_window on an
        existing window be 'warp and wvedalwaysraise' instead of just
        wvedalwaysraise (thus disabled when vedwarpcontext is false).
--- John Gibson, Mar  7 1994
        Uses vededit instead of ved*editor, ved*select
--- John Gibson, Jan 21 1994
        Got rid of wved_is_ch*anged_window except for PW*M Is_changed_window
--- John Gibson, Nov 18 1993
        Added Setonscreen_next, made vedswapfiles use it.
--- Jonathan Meyer, Sep 25 1993
        Converted to ved_try_search. Now uses Safe_search.
--- John Gibson, Jun 12 1993
        Rewrote vedusewindows to call procedure wved_change_usewindows_X for
        arg X etc, instead of wved_ch*ange_ved_mode (avoids having
        multiple definitions of the latter).
--- John Gibson, May  8 1993
        Removed call of charout in Set_basewindow
--- John Gibson, Feb 12 1993
        Made vedusewindows set vedprintingdone false when switching mode
--- John Gibson, Dec 19 1992
        Weakened all PW*M references, added defautl initialisations for
        wved_ procedures
--- Adrian Howard, Sep  8 1992
        dlocally set vedediting to true in vedsetonscreen so calls to
        vedputmessage work properly
--- John Gibson, Nov 19 1991
        Moved test for $-Sys$-Pw*m$-user_switch == "pw*mnewinputsource" into
        -wved_should_warp_mouse- (so it gets tested in the call of
        wved_should_warp_mouse in ved*editor).
--- John Gibson, Nov 11 1991
        Corrected -vedfileisonscreen- so that it returns true simply if
        file has a window (which is what it was intended to do).
--- John Gibson, Oct 19 1991
        Fix to pw*m vedsetonscreen to recognise $-Sys$-Pw*m$-user_switch ==
        "pw*mnewinputsource" as meaning don't set wvedwindowchanged true.
--- John Gibson, Sep  6 1991
        Aded dlocal popgctrace in vedsetonscreen
--- Jonathan Meyer, Aug  2 1991
        vedfileisonscreen now uses USEWINDOWS and not vedusewindows
--- Adrian Howard, Aug  1 1991 : Changed -vedfileisonscreen- so it returns true
        only when the window is not iconic for all windowed VED systems
--- John Gibson, Jul 15 1991
        Fix to vedsetonscreen
--- John Gibson, Jul 11 1991
        Added Set_basewindow
--- Aaron Sloman, Jul  5 1991
        Commented out one occurrence of wvedalwaysraise because it
        causes excessive refrefreshing in pw*m. Bug reported by V14.0
        users.
--- John Gibson, Jul  4 1991
        Revision to -vedusewindows-
--- John Gibson, Jun 22 1991
        More changes
--- John Gibson, Jun 21 1991
        Various changes
--- Jonathan Meyer, Jun 18 1991
        Added wved_ved_set_window - redefined by XVED to enlarge the
        window. PW*M rings a bell when this is called.
--- John Gibson, Jun 15 1991
        Allowed vedsetonscreen to take 2nd arg true to mean vednamestring
        only displayed
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- Jonathan Meyer, Jun  5 1991
        Made -wved_file_of_window- a vars
--- John Gibson, Jun  4 1991
        Made -vedsetwindow- just ring bell when using windows
--- John Gibson, Apr 11 1991
        Moved -vedusewindows- in from wved.p and rewrote.
        Also various other changes.
--- John Gibson, Mar 21 1991
        Some procedures into Sys$-Ved
--- Aaron Sloman, Jan 15 1991
    Made vedsetonscreen sensitive to wvedalwaysraise
--- Aaron Sloman, Jan 15 1991
    Made vedfileselect work on status line.
    More changes to prevent spurious movements of cursor if input
        focus changed.
--- John Gibson, Jan  8 1991
        Made -vedfileisonscreen- return true for pw*m window only if
        not iconic.
--- John Gibson, Nov  2 1990
        Uses -ved_current_file-
--- John Gibson, Oct 30 1990
        Made -vedsetonscreen- set -vedinputfocus- (to -ved_current_file-).
        Put whole file into section Sys$-Ved.
--- Aaron Sloman, Oct 30 1990
        Added vedfileisonscreen and simplified vedswapfiles
--- Aaron Sloman, Oct 29 1990
        Altered vedfileselect to check whether vedfileprops is word or string
--- Aaron Sloman, Oct 23 1990
        renamed pw*m_set_vedsize as wved_set_size
--- Aaron Sloman, Oct  8 1990
        Changed guard on xvedinitfile
--- Aaron Sloman, Sep 27 1990
        Put Set_residual_window in section $-Sys$-Ved
        Added Tidy_other_window, for stuff common to single and multi window
        moved in ved_rb
        Added wved_window_of_file, for use by Pw*m or Xved
--- Aaron Sloman, Sep 23 1990
        Replaced Vedfreewindow with wvedfreewindow (exported)
--- John Williams, Sep  7 1990
        Fixed bug in -vvedgotoplace- (if file was empty)
--- John Gibson, Jul 17 1990
        vedq*uitfile -> Sys$-Ved$-Quit_file
--- Aaron Sloman, Jul 10 1990
        Altered to use wvedwindowchanged.
        Made -vedswapfiles- use -Vedpw*mwarping-
        Fixed vedfileselect to work with PW*M, and to use Vedpw*mwarping
--- Rob Duncan, Feb 22 1990
        Removed mishap from updaterof -vedstartwindow-: value is now
        quietly adjusted to be in range: 0 .. vedscreenlength
--- Rob Duncan, Feb  2 1990
        Made -vedstartwindow- an active variable
--- Rob Duncan, Nov  7 1989
        Changed -vedsetwindow- to test -terminal_can_scroll- instead of
        -vednolineinsert-
--- John Gibson, Aug  2 1989
        Now uses -sys_fname_name- and -sys_fname_namev-. Also commoned
        up code from -Set_new_window- and -Set_pw*m_window-.
--- John Williams, Mar  1 1988
        -vedfileselect- now localises -pr- to -sys_syspr-
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
*/
