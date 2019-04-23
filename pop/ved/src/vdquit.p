/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdquit.p
 > Purpose:         VED: quitting ved buffers
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

;;;----------------- QUITTING THE EDITOR ----------------------------------

;;; This file contains those commands for quitting the editor that do not
;;; cause a compilation.

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedinascii)
    ;

vars
        procedure (ved_ved, wved_destroy_window, vedsysfile),
        vednamestring, wvedwindowchanged
    ;

section $-Sys$-Ved;

constant
        procedure (Set_residual_window, Set_sysfile_defaults)
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedvedquitfile, vedalwaysask, wved_ved_quit_file,

                    ved_rqq, ved_xx,
                    ved_qq, ved_q, ved_get_reply, ved_wq, vedqget, ved_qved,
                    ved_rrq, vedpopexit
                    ;

vars
    vedvedquitfile  = identfn,
    vedalwaysask    = false,    ;;; if true, then vedpopexit asks before writing files

    procedure wved_ved_quit_file = erase,
    ;


define lconstant Any_changed();
    lvars file;
    ved_save_file_globals();
    for file in vedbufferlist do
        if file(VF_CHANGED) and (file(VF_WRITEABLE) or vedwriteallfiles) then
            return(true)
        endif
    endfor;
    false
enddefine;

define vars ved_rqq();
    lvars file, win;
    false -> ved_current_file;
    if USEWINDOWS then
        fast_for file in vedbufferlist do
            if wved_window_of_file(file) ->> win then
                false -> wved_window_of_file(file);
                wved_destroy_window(win)
            endif
        endfor;
        false -> wvedwindow
    endif;
    [] -> vedbufferlist;
    sysexit();
enddefine;

define vars ved_xx();
    ;;; write all files and leave the editor and pop
    unless USEWINDOWS then pr(newline) endunless;
    vedwritefiles();
    ved_rqq();
enddefine;

define vars ved_get_reply(mess_string, answers) -> char;
    lvars char, mess_string, answers;
    Open_file_window();
    repeat
        vedscreenbell();
        vedputmessage(nullstring);
        vedputmessage(mess_string);
        uppertolower(vedinascii()) -> char;
        if strmember(char, answers) then
            vedputmessage(nullstring);
            return
        endif;
        vedclearinput()
    endrepeat
enddefine;

    /*  Quit and leave pop11. Will complain if there are changed and
        writeable files.
    */
define vars ved_qq();
    lvars char;
    if Any_changed() then
        ved_get_reply(
            '\{b}files changed - write them? \{b7A} \[+u'(pi)>y']yes \[]  \[+u'(pi)>n']no \[]  \[+u'(pi)>c']continue ',
                'ync') -> char;
        if char == `c` then
            vedputmessage(vednamestring);
            return
        elseif char == `y` then
            chain(ved_xx)
        endif
    endif;
    ved_rqq();
enddefine;


    /*  If editing_new_file is false or 1, destroy associated window.
        If 0, get new file using the old window.
    */
define Quit_file(editing_new_file);
    lvars   editing_new_file, waschanged = wvedwindowchanged, forceinput,
            temp_bufferlist, back_temp_bufferlist;
    dlocal  vedwarpcontext;

    wved_should_warp_mouse("ved_q") -> forceinput;

    if vedonstatus then vedstatusswitch() endif;
    ;;; call user defined quit routine - Aled, 88/1/5
    vedvedquitfile();

    Set_residual_window();  ;;; remove file from window

    ;;; remove file from vedbufferlist, if present
    if front(vedbufferlist) == vedcurrentfile then
        back(vedbufferlist) -> vedbufferlist
    else
        vedbufferlist -> temp_bufferlist;
        until temp_bufferlist == [] do;
            back(temp_bufferlist) -> back_temp_bufferlist;
            if front(back_temp_bufferlist) == vedcurrentfile then
                back(back_temp_bufferlist) -> back(temp_bufferlist);
                quitloop();
            else
                back_temp_bufferlist -> temp_bufferlist;
            endif;
        enduntil;
    endif;

    if USEWINDOWS then
        wved_ved_quit_file(editing_new_file);
        if editing_new_file then
            ;;; reuse existing window
            wvedwindow -> wvedfreewindow
        else
            ;;; destroy existing window
            wved_destroy_window(wvedwindow);
            false -> vedupperfile;
            unless forceinput then
                false -> vedwarpcontext ;;; e.g. won't open iconised windows
            endunless;
        endif;
        false -> wvedwindow
    endif;

    if vedcurrentfile == vedinputfocus then false -> vedinputfocus endif;
    ;;; ensure window zapped and that other locals of file get restored
    ;;; correctly ...
    false -> ved_current_file;

    ;;; ... then make sure file buffer get garbaged if file is
    {} -> vedbuffer;

    ;;; tidy up default filename of appropriate type
    unless testdef vedsysfile
    and weakref[vedsysfile] Set_sysfile_defaults(false) then
        ;;; user file -- create a temporary filename
        'temp' sys_>< pop_default_type -> vedvedname
    endunless;

    unless editing_new_file then
        if vedbufferlist == [] then
            unless USEWINDOWS then
                vedputmessage('\{b}quitting ' <> vedpathname
                                    <> ' \{b}no files left')
            ;;; else don't bother with a message for XVed
            endunless;
            vedexit(identfn)
        else
            Setonscreen_next(false, false)
        endif
    endunless;
    waschanged or forceinput -> wvedwindowchanged
enddefine;

    ;;; If file changed then check if OK to quit
define lconstant Ok_to_quit() -> bool;
    lvars char, bool = true;

    if vedchanged and (vedwriteable or vedwriteallfiles) then
        ved_get_reply(
            '\{b}file changed - write it? \{b7A} \[+u'(pi)>y']yes \[]  \[+u'(pi)>n']no \[]  \[+u'(pi)>c']continue ',
                    'ync') -> char;
        if char == `c` then
            vedputmessage(vednamestring);
            false -> bool;
            return()
        elseif char == `y` then
            ved_w1()
        endif
    endif;
    unless XWINDOWS then
        ;;; this message is pretty useless even for ordinary Ved, but
        ;;; totally so for XVed
        vedputmessage('\{b}quitting ' <> vedpathname);
    endunless
enddefine;

define vars ved_q();
    if Ok_to_quit() then Quit_file(false) endif
enddefine;

define vars ved_wq();
    ved_w1();
    ved_q();
enddefine;

define vedqget(edit_p);
    lvars char, edit_p, want_new_window = false, old_win;

    ;;; an exit action to tidy up in case the supplied procedure
    ;;; exits (normally or abnormally) and fails to provide a file to edit
    define lconstant exit_tidy_up(context);
        lvars context;
        returnif(context fi_> 2);
        if USEWINDOWS and old_win then wved_destroy_window(old_win) endif;
        returnunless(vedinvedprocess and not(vedcurrentfile));
        if USEWINDOWS then
            if wvedfreewindow then
                wved_destroy_window(wvedfreewindow);
                false -> wvedfreewindow
            endif;
            false -> vedupperfile
        endif;

        if vedbufferlist == [] then
            if context == 2 and vedmessage /= nullstring then
                ;;; repeat error message on basewindow
                vedputmessage(vedmessage)
            endif;
            ;;; abandon Ved -- clear stack to get rid of
            ;;; stuff placed there by currently active 'exitfrom'
            clearstack();
            vedexit(identfn)
        else
            Setonscreen_next(false, vedmessage)
        endif
    enddefine;

    dlocal 0 % false -> old_win, exit_tidy_up(dlocal_context) %;

    if isboolean(edit_p) then
        (), edit_p -> (edit_p, want_new_window)
    endif;

    if Ok_to_quit() then
        nullstring -> vedmessage;
        Quit_file(true);
        if want_new_window then
            wvedfreewindow, false -> (old_win, wvedfreewindow)
        endif;
        edit_p()            ;;; run the supplied procedure
    endif
enddefine;

define vars ved_qved();
    ;;; quit current edit and start another
    if vedargument = nullstring then chain(ved_q) endif;
    vedqget(ved_ved)
enddefine;

define vars ved_rrq();
    ;;; really really quit!
    Quit_file(false)
enddefine;


    ;;; Bad_err_interrupt recognises sysexit interrupt handlers by their
    ;;; pdprops being sysexit
define lconstant Writefiles_continue = exitto(%vedappfiles%) enddefine;
sysexit -> pdprops(Writefiles_continue);

define vedpopexit();
    lvars char, mess, notify;
    dlocal interrupt;
    ;;; In case this is a 'bad' exit, don't attempt any I/O other than
    ;;; writing the files if popdevin is not a (foreground) terminal
    systrmdev(popdevin) == true -> notify;
    if notify and vedinvedprocess then vedscreenreset() endif;
    returnunless(ispair(vedbufferlist) and Any_changed());
    if notify then
        vedscreenbell();
        if vedalwaysask then
            repeat
                printf('FILES CHANGED. WRITE THEM? TYPE \'y\', \'n\' OR Ctrl-C ');
                sysflush(pop_charout_device);
                rawcharin() -> char;        ;;; NOT vedinascii!!
                cucharout(`\n`);
                quitif(char == `n` or char == `y`);
                sys_clear_input(poprawdevin)
            endrepeat;
            returnif(char == `n`)
        endif;
        printf('WRITING ALTERED FILES\n')
    endif;

    if iscaller(sysexit) then Writefiles_continue -> interrupt endif;
    vedwritefiles()
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 12 1996
        Changed sense of argument to Quit_file; allowed vedqget to take
        optional bollean saying whether to use the old window or not.
--- John Gibson, Mar  6 1996
        Improved ved_rqq
--- John Gibson, Feb  6 1996
        Removed dlocal vederror and pr*mishap from vedpopexit
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Feb 27 1995
        Made vedpopexit assign Writefiles_continue to interrupt if called
        from sysexit.
--- John Gibson, Jan 29 1994
        Changed ved_wq so it just does ved_w1() and ved_q().
--- John Gibson, Nov 18 1993
        Changed to exit_tidy_up to use Setonscreen_next
--- John Gibson, Apr 20 1993
        Made Quit_file assign false to ved_current_file (not just to
        vedcurrentfile).
--- John Gibson, Oct  9 1992
        Undid last change, but made Quit_file assign false to wvedwindow
        and wved_window_of_file
--- John Gibson, Aug  8 1992
        Made Quit_file assign {} to buffer slot in file structure as well as
        to vedbuffer.
--- Adrian Howard, Aug 16 1991 : Fixed bug in -Quit_file- which incorrectly
        assumed that the file being quit was at the front of -vedbufferlist-.
--- Jonathan Meyer, Jul 27 1991
        Passed editing_new_file as argument to wved_ved_quit_file (needed to
        test if this is the last file being quitted, so that popup menus
        can be dismissed)
--- John Gibson, Jul 13 1991
        Made Quit_file with no new file to go try to get a non-iconic
        file if possible
--- Jonathan Meyer, Jul  8 1991
        Added wved_ved_quit_file
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- Aaron Sloman, Nov  4 1990
        Stopped PW*M moving input focus when quitting,if USEWINDOWS
        is true.
--- John Gibson, Nov  1 1990
        Moved -vedpopexit- in from vdprocess.p
--- John Gibson, Oct 30 1990
        Made -Quit_file- set -vedinputfocus- false if equal to
        -vedcurrentfile-.
--- Aaron Sloman, Oct 12 1990
        Changed xved to wved
--- Aaron Sloman, Sep 26 1990
        moved ve*d_rb to vdwindows.p
        Simplified stuff for vedusepw*mwindows
        Replaced Vedfreewindow with wvedfreewindow(exported)
        Replaced Get_reply with ved_get_reply, exported but vars.
--- Aaron Sloman, Aug 10 1990
        Removed vedscreenreset declaration
--- John Gibson, Jul 17 1990
        Put into section Sys$-Ved, added -ved_wq- from library.
--- Aaron Sloman, Jul  9 1990
        Introduced wvedwindowchanged and Vedpw*mw*arping, and made
        ved_rb use the latter
--- Aaron Sloman, Jul  6 1990
        Pulled common code out of ved_q and vedqget, and put into Ok_to_quit
--- Aaron Sloman, Apr  9 1988
        replaced -vedcurrent- with -vedpathname-
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- Aled Morris, Jan  5 1988
        Added -vedvedquitfile-, user defined routine called when a file
        is quitted
 */
