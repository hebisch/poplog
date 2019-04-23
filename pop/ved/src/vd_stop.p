/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/ved/src/vd_stop.p
 > Purpose:         VED handlers for TSTP and CONT
 > Author:          Roger Evans, Oct 11 1988 (see revisions)
 > Documentation:   HELP *STOP
 > Related Files:   sig_stop.p
 */

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../lib/include/sigdefs.ph'


    /* File empty if none of these defined */
#_IF DEF SIG_TSTP or DEF SIG_TTIN or DEF SIG_TTOU

global vars procedure
    (vedscreenraw, vedscreenreset, vedrestorewindows, wved_raise_window);


;;; --------------------------------------------------------------------

section $-Sys$-Ved => vedstop_refresh_both;

global vars vedstop_refresh_both = false;

lvars save_vedprintingdone;

define Stop_handler(sig);
    lvars sig;

    returnunless(vedinvedprocess);

    if XWINDOWS then
        if sig == SIG_TSTP then
            ;;; raise base window when suspending
            wved_raise_window(popdevout)
        elseif sig == -SIG_TSTP then
            ;;; raise current XVed window on continuing
            wved_raise_window(wvedwindow)
        endif;
        return
    endif;

    ;;; rest is for non-windowed ved
    if sig fi_> 0 then
        ;;; stopping
        vedprintingdone -> save_vedprintingdone;
        if sig /== SIG_TTOU then  ;;; not if we can't output!
            ;;; get to bottom of screen and reset keypad etc.
            vedscreenreset()
        endif;
        true -> vedprintingdone

    else
        ;;; continuing
        returnif(save_vedprintingdone);
        dlocal vedscreenbell = identfn;
        _CHECKINTERRUPT;
        if vedstop_refresh_both then
            vedrestorewindows();
            Set_wait_cursor(true, true)
        else
            vedscreenreset();       ;;; ensure cursor at bottom of screen
            1000 ->> vedscreenline -> vedscreencolumn;  ;;; cursor may be anywhere
            vedscreenraw();
            false -> vedprintingdone;
            procedure;
                dlocal vedstartwindow;
                false ->> vedupperfile -> vedlowerfile;
                if vedwindowlength == vedscreenlength then
                    vedscreenlength -> vedstartwindow
                endif;
                vedsetonscreen(ved_current_file, false);
            endprocedure();
        endif
    endif
enddefine;

endsection;     /* $-Sys$-Ved */


#_ENDIF


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 19 1992
        Made XVed raise basewindow when stopping etc
--- John Gibson, May  8 1992
        Changed to work better.
--- John Gibson, Jun  8 1991
        Corrected tests for when to do nothing
--- Aaron Sloman, Aug 10 1990
        vedscreenreset made vars
--- Rob Duncan, Feb  2 1990
        Reduced the scope of dlocal vedstartwindow in -Cont_handler- by
        adding new procedure -redisplay-: the call to -vedscreenraw- may
        want to change -vedstartwindow- globally if the terminal window
        size has been changed during the suspend.
--- John Gibson, Aug 24 1989
        Moved from C.bsd into C.unix and wrapped all code in #_IF
        testing for presence of appropriate signals.
--- John Gibson, Apr 30 1989
        Renamed procedures Into section Sys$-Ved
--- John Williams, Dec  7 1988
         Re-installed because not linked into 'S' directories
--- Roger Evans, Nov 17 1988
        moved to C.bsd (instead of C.unix, which was wrong)
 */
