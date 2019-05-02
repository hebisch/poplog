/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/time.p
 > Purpose:         Common Lisp time functions, also ROOM
 > Author:          John Williams, May 20 1987 (see revisions)
 > Documentation:   CLtL, p443-7
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;


constant internal_time_units_per_sec = 1000000;

constant _1900_1970 = 70 * 365 * 86400 /* 17 leap years */ + (17 * 86400);


define get_universal_time();
    sys_real_time() + _1900_1970
enddefine;

define get_internal_run_time();
    systime()*10000 ;;; internal_time_units_per_sec/100
enddefine;

define get_internal_real_time();
    sys_microtime()
enddefine;


define sleep(n);
    syssleep(intof(n * 100));
    nil;
enddefine;


;;; get-decoded-time
;;; decode_universal_time
;;; encode_universal_time
;;; Aled Morris - 13th August, 1986

global vars pop_time_zone = 0;

propsheet_idents pop_time_zone;

lconstant
    seconds_per_minute  = 60,
    minutes_per_hour    = 60,
    seconds_per_hour    = minutes_per_hour * seconds_per_minute,
    hours_per_day       = 24,
    days_per_week       = 7,
    days_per_year       = 365,
    days_per_leap_year  = days_per_year + 1,
    days_per_4_nl_years = days_per_year * 4,
    days_per_4_years    = days_per_leap_year + 3 * days_per_year,
    days_per_100_years  = days_per_4_nl_years + 24 * days_per_4_years,
    days_per_400_years  = days_per_100_years * 4 + 1,
    days_in_months      = {31 28 31 30 31 30 31 31 30 31 30 31},
    leap_days_in_months = {31 29 31 30 31 30 31 31 30 31 30 31},
    ;


define lconstant Is_leap_year(y);
    (y mod 4 == 0 and y mod 100 /== 0) or (y mod 400 == 0)
enddefine;


define lconstant Check_time_int(str, lo, hi, i);
    unless isintegral(i) and i >= lo then
        mishap(i, 1, (if lo == 0 then
                         'Non-negative'
                     else
                         'Positive'
                     endif) <> ' integer needed for ' <> str <> ' value');
    endunless;
    if hi and i > hi then
        mishap(i, 1, str <> ' value out of range');
    endif
enddefine;


define lconstant Checkr_tz_secs(tz);
    unless isrational(tz) and tz >= -24 and tz <= 24 do
        simple_type_error(
            'Illegal time zone value: ~S', tz, [^@RATIONAL -24 24])
    endunless;
    intof(tz * seconds_per_hour)
enddefine;


define decode_universal_time(u_time, tz);
    lvars year, month, date, hour, minute, second, day_of_week, month_days,
            tmp;
    defaults tz pop_time_zone;

    u_time - Checkr_tz_secs(tz) -> u_time;

    Check_time_int('universal time', 0, false, u_time);

    u_time // seconds_per_minute  -> u_time -> second;
    u_time // minutes_per_hour    -> u_time -> minute;
    u_time // hours_per_day       -> u_time -> hour;

    u_time mod days_per_week -> day_of_week;

    (u_time // days_per_400_years) * 400                  -> year -> u_time;
    ((u_time // days_per_100_years) ->> tmp) * 100 + year -> year -> u_time;
    if tmp /== 1 /* leap year is year 1 (ie second) of 4 year group */ then
        if u_time > days_per_4_nl_years then
            u_time - days_per_4_nl_years -> u_time;
            year + 4 /* skip the first 4 year group */ -> year;
        endif;
    endif;
    (u_time // days_per_4_years) * 4 + year -> year -> u_time;
    year + 1900 -> year;

    /* This section revised by Steve Knight, April 1994 */
    if u_time < days_per_leap_year then
        leap_days_in_months
    elseif u_time == days_per_leap_year then
        ;;; This next line is rather overdone, but it maintains the
        ;;; pattern established by the else-clause.
        u_time // days_per_leap_year + year -> year -> u_time;
        days_in_months
    else
        u_time - days_per_leap_year -> u_time;
        u_time // days_per_year + 1 /* leap day */ + year -> year -> u_time;
        days_in_months
    endif -> month_days;

    1 /* january */ -> month;
    while (u_time - month_days(month) ->> tmp) >= 0 do
        month + 1 -> month;
        tmp -> u_time;
    endwhile;

    u_time + 1 /* days are 1 (not 0) origin */ -> date;

    second, minute, hour, date, month, year,
    day_of_week, nil /* daylight saving */, tz
enddefine;


define get_decoded_time();
    decode_universal_time(get_universal_time(), pop_time_zone)
enddefine;


define encode_universal_time(second, minute, hour, date, month, year, tz);
    lvars this_year, u_time, i, month_days;
    defaults tz pop_time_zone;

    Check_time_int('year', 0, false, year);

    if year < 100 then /* use "obvious" year */
        {% get_decoded_time() %}(6) -> this_year;
        this_year div 100 * 100 + year -> year;
        if this_year - year > 50 then year + 100 -> year endif;
    endif;
    (year ->> this_year) - 1900 /* base year */ -> year;

    year // 400 * days_per_400_years -> u_time -> year;
    if year > 100 then u_time + 1 /* leap day per 400 years */ -> u_time endif;
    year // 100 * days_per_100_years + u_time -> u_time -> year;
    if year > 3 then        /* possibility of leap year */
        year - 4 -> year;   /* ignore non leap year at beginning of century */
        u_time + days_per_4_nl_years -> u_time;

        year // 4 * days_per_4_years + u_time -> u_time -> year;
        if year > 0 then u_time + 1 -> u_time endif; /* last was leap year */
    endif;
    year * days_per_year + u_time -> u_time;

    Check_time_int('month', 1, 12, month);
    if Is_leap_year(this_year) then
        leap_days_in_months
    else
        days_in_months
    endif -> month_days;
    for i from 1 /* january */ to month - 1 do
        u_time + month_days(i) -> u_time;
    endfor;

    Check_time_int('date', 1, month_days(month), date);
    u_time + (date - 1 /* convert date to 0 origin */) -> u_time;

    Check_time_int('hour', 0, 23, hour);
    u_time * hours_per_day + hour -> u_time;

    Check_time_int('minute', 0, 59, minute);
    u_time * minutes_per_hour + minute -> u_time;

    Check_time_int('second', 0, 59, second);
    u_time * seconds_per_minute + second -> u_time;

    u_time + Checkr_tz_secs(tz)
enddefine;


/* Room */

define room(verbose);
    lvars sl, csl;
    dlocal popgctrace = false;
    defaults verbose true;

    sysgarbage();
    stacklength() -> sl;
    callstacklength(0) -> csl;

    format(standard_output,
'~&~11A:  ~D used + ~D free = ~D~%~@[~*~11A:  ~D used~%~11A:  ~D used + ~D free = ~D~%~]',
    [%
        'HEAP',
        popmemused,
        popmemlim - popmemused,
        popmemlim,
        verbose,
        'ARG STACK',
        sl,
        'CALL STACK',
        csl,
        pop_callstack_lim - csl,
        pop_callstack_lim,
    %]) ->
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  6 1995
        Time zones can now range from -24 to 24.
--- John Williams, Apr 26 1994
        Added fix from Steve Knight.
--- John Williams, Apr 14 1992
        Installed Stephen Silver's fix at Sussex
--- Stephen Silver, Apr  1 1992
        Fixed bug in decode-universal-time
 */
