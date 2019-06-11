/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_alarm.p
 > Purpose:         Set an alarm in VED
 > Author:          Aaron Sloman, Jun 16 1991
 > Documentation:   HELP * VED_ALARM (to be done)
 > Related Files:   LIB * VED_CLOCK
 */

/*

Based on an idea by Roger Evans. Re-written to be separate from
ved_clock, and to use sys_timer.

*/


compile_mode:pop11 +strict;


section;

lvars
    alarm_time      = false,    ;;; no alarm initially
    alarmcounter    = 0,        ;;; incremented by each alarm set
    alarmlist = [],
    default_message = '<<<<<<<<<<<<<<ALARM>>>>>>>>>>>>>>',
    ;


/* Alarm handling */


define global vars procedure vedalarm_action(string);
    lvars string;
    ;;; User definable, called by ALARMCHECK

    if vedusewindows == "x" then
        dlocal vedediting =true;
    endif;
    repeat 10 times
        vedputmessage('');
        vedputmessage(string);
        vedscreenbell();
        syssleep(10);   ;;; doesn't work across networks
    endrepeat;
    vedsetcursor();
    vedscr_flush_output(); ;;; sysflush(popdevraw)
enddefine;


define lvars ALARMCHECK(string);
    ;;; The interrupt procedure. The string will be frozen in
    lvars string, thistime = sysdaytime();

    if vedusewindows == "x" then
        dlocal vedediting = true;
    endif;

    max(alarmcounter - 1, 0) -> alarmcounter;
    if alarmcounter == 0 then
        false -> alarm_time;
        [] -> alarmlist;
    endif;
    vedalarm_action(string);
enddefine;

define lconstant make_alarm_message(string, day_time) -> string;
    lvars string, day_time;
    consstring(
    #|
        explode('Time now: '),
        explode(day_time),
        explode(' Alarm set for: '),
        explode(string)
    |#) -> string;
enddefine;

define lconstant number_at(loc, string);
    ;;; return the 2 digit number starting at loc in string
    ;;; or -false- if it isn't one.
    lvars loc, string;
    strnumber(substring(loc, 2, string))
enddefine;

define global vars ved_alarm;
    ;;; set the next alarm time, or print out current setting, if no
    ;;; argument is given.
    lvars
        loc, hours, mins, secs,
        new_hours, new_mins, new_secs,
        alarm_delay,
        alarm_pdr = false,      ;;; A closure of ALARMCHECK
        re_setting = isstartstring('new',vedargument), ;;; for additional alarm
        setting = true,
        len = datalength(vedargument),
        alarm_message = default_message,
        day_time = substring(12,8,sysdaytime()),
        ;

    ;;; If vedargument contains a space, assume that
    ;;; What follows it is an alarm message.
    strmember(`\s`,vedargument) -> loc;
    if loc then
        allbutfirst(loc, vedargument) -> alarm_message;
        substring(1, loc - 1, vedargument) -> vedargument;
        loc - 1 -> len
    endif;

    ;;; Decide whether setting the alarm time or not.
    if vedargument = 'off' then
        false -> alarm_time;
        false -> setting;
        lvars pdr;
        for pdr in alarmlist do
            false -> sys_timer(pdr);
        endfor;
        0 -> alarmcounter;
        [] -> alarmlist;
    elseif vedargument = nullstring then
        false -> setting
    else
        true -> setting;
    endif;

    returnunless(setting)
        (if alarm_time then
            make_alarm_message(alarm_time, day_time)
            else 'Alarm OFF. Time now ' sys_>< day_time
            endif -> vedmessage);

    unless alarm_pdr then
        ALARMCHECK(%alarm_message%) -> alarm_pdr;
        [^ alarm_pdr ^^alarmlist] -> alarmlist;
    endunless;

    ;;; Get strings for current time (Only Unix)
    number_at(1, day_time) -> hours;
    number_at(4, day_time) -> mins;
    number_at(7, day_time) -> secs;
    ;;; Decide whether argument is a number of minutes or a time
    if strmember(`:`, vedargument) then false
    else strnumber(vedargument)
    endif -> alarm_delay;

    if alarm_delay then
        ;;; Set alarm to go that number of minutes from now
        ;;; convert to seconds
        round(alarm_delay * 60) -> alarm_delay;


        ;;; Now add the appropriate number of seconds, and handle overflows
        alarm_delay + secs -> new_secs;

        ;;; Check if seconds over 60
        if new_secs > 59 then
            ((new_secs // 60) + mins) -> new_mins -> new_secs;
        else mins -> new_mins
        endif;

        ;;; Check if minutes over 60
        if new_mins > 59 then
            ((new_mins // 60) + hours) -> new_hours -> new_mins;
        else hours -> new_hours
        endif;
        new_hours mod 24 -> new_hours;


        ;;; (Re-)build alarm string
        ;;; First get strings for times
        if new_secs < 10 then '0' else nullstring endif
            sys_>< new_secs -> new_secs;

        if new_mins < 10 then '0' else nullstring endif
            sys_>< new_mins -> new_mins;

        if new_hours < 10 then '0' else nullstring endif
            sys_>< new_hours -> new_hours;

        consstring( #|
                explode(new_hours), `:`,
                explode(new_mins), `:`,
                explode(new_secs) |# ) -> alarm_time;
    else
        if len == 5 then
            vedargument sys_>< ':00'
        elseif len == 8 then
            vedargument
        else false
        endif -> alarm_time;

        ;;; now work out delay
        number_at(1, alarm_time) -> new_hours;
        number_at(4, alarm_time) -> new_mins;
        number_at(7, alarm_time) -> new_secs;
        new_hours - hours -> hours; ;;; delay, could be negative
        new_mins - mins -> mins;    ;;; ditto
        new_secs - secs -> secs;    ;;; ditto
        if secs < 0 then
            mins - 1 -> mins; 60 + secs -> secs
        endif;
        if mins < 0 then
            hours - 1 -> hours; 60 + mins -> mins;
        endif;
        if hours < 0 then
            24 + hours -> hours
        endif;
        3600 * hours + 60 * mins + secs -> alarm_delay;
    endif;

    unless isstring(alarm_time)
    and datalength(alarm_time) == 8
    and alarm_time(3) == `:` and alarm_time(6) == `:`
    and number_at(1, alarm_time)
    and number_at(4, alarm_time)
    and number_at(7, alarm_time)
    then
        vederror('ENTER alarm [ <mins to go> or hh:mm or hh:mm:ss]')
    endunless;

    make_alarm_message(alarm_time, day_time) -> vedmessage;

    alarm_delay * 1e6 -> sys_timer(alarm_pdr);
    alarmcounter + 1 -> alarmcounter;
enddefine;


endsection;
