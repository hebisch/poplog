/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_clock.p
 > Purpose:         A status-line clock for VED
 > Author:          Roger Evans, July 1983 (see revisions)
 > Documentation:   HELP * VED_CLOCK
 > Related Files:   LIB * VED_ALARM, REF * TIMES
 */
/* needs mods for VMS */

/* test for stop_handler temporary till stop handler fixed in system

     THIS TEST NOW COMMENTED OUT
*/

/*
This program originally by Roger Evans was re-written to use the
new multiple timer mechanism in Poplog V14.02

*/

/*
    The clock appears on the status line on the right hand side

    Note: if you go out of VED the clock stops displaying, but the
          timer continues.

*/

compile_mode:pop11 +strict;

;;; include sigdefs.ph  ;;; not needed from V14.04

section;

lvars
    clockinterval   = 5,        ;;; minimum reasonable interval for update
    clockon         = false,    ;;; clock off initially
    clocksecs       = 60,       ;;; clock tick rate (secs) (show mins only)
    lasttime        = false,    ;;; last value of SYS_REAL_TIME DIV CLOCKSECS
    lastoffset      = false,    ;;; last value of VEDSCREENOFFSET
    ;

;;; lconstant stop_handler = sys_signal_handler(SIG_TSTP);

/* check the time - update if it's changed */


define lconstant CLOCKCHECK;
    ;;; The interrupt procedure
    lvars time, timelen = if clocksecs == 60 then 5 else 8 endif;

    /* Don"t update if
        (a) clock turned off
        (b) vedediting is set false (not in editor)
        (c) input is waiting
    */

    define lconstant doscreenmark();
        vedscreenoutput(vedscreenstatus_-_mark);
    enddefine;

    unless clockon then
        return
    endunless;
    clockinterval * 1e6 -> sys_timer(CLOCKCHECK);

    if vedusewindows == "x" then
        returnunless(isvector(vedupperfile));
        dlocal vedediting = true;
    else
    returnunless(vedediting);
    endif;

    returnif(vedinputwaiting()
             or vedprintingdone or iscaller(vedrestorescreen));

    sys_real_time() div clocksecs -> time;
    if (time /== lasttime) or (vedscreenoffset /== lastoffset) then
        time -> lasttime;
        vedscreenoffset -> lastoffset;
        ;;; send new clock string
        substring(12, timelen, sysdaytime()) -> time;
        vedscreenxy(vedscreenwidth - 10, vedscreenoffset + 1);
        if clocksecs == 60 then
            ;;; doing only minutes
            doscreenmark();
            doscreenmark();
            doscreenmark()
        endif;
        doscreenmark();
        appdata(time, vedscreenoutput);
        doscreenmark();
        doscreenmark();
        ;;; reset cursor
        1000 ->> vedscreencolumn -> vedscreenline;
        vedsetcursor();
        vedscr_flush_output(); ;;; sysflush(popdevraw)
    endif;

enddefine;


define lconstant stopclock;
    if clockon then
        false -> sys_timer(CLOCKCHECK);
        vedrefreshstatus();
        false -> clockon
    endif;
    /* force clock to refresh next time */
    false ->> lasttime -> lastoffset
enddefine;

define lconstant startclock;
    true -> clockon;
    CLOCKCHECK();
    /* force clock to refresh next time */
    false ->> lasttime -> lastoffset
enddefine;


/* SECS just alters parameters detailed above */

define global ved_secs;
    lvars secs = strnumber(vedargument);
    unless vedargument = nullstring or isinteger(secs) then
        vederror('ONLY INTEGER ARGUMENTS FOR "ENTER secs"')
    endunless;
    if secs then min(secs, 60) -> secs endif;
    if secs or clocksecs == 60 then
        'SECS ON',  secs
    else
        'SECS OFF', 60
    endif -> clocksecs -> vedmessage;
    min(10, clocksecs) -> clockinterval;
    CLOCKCHECK();
    /* force clock to refresh next time */
    false ->> lasttime -> lastoffset
enddefine;



define global ved_clock;
    lvars secs = strnumber(vedargument);
    if secs then stopclock() endif;
    if not(secs) and clockon then
        stopclock();
        'CLOCK OFF'
    else
        if secs then ved_secs() endif;
        startclock();
        'CLOCK ON'
    endif -> vedmessage
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 26 1991
        Removed test for iscaller(charin) so that it works in xved
        (not necessary for ordinary ved since vedprintingdone gets set)
--- Aaron Sloman, Jun 16 1991
    Moved ved_alarm to a separate file.
--- Aaron Sloman, Jun  4 1991
    Fixed to use sys_timer
    Allowed ved_secs and ved_clock to take integer argument
 */
