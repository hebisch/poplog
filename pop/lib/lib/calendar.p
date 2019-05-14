/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/lib/calendar.p
 >  Purpose:        print a calendar for a given month or yer
 >  Author:         Jonathan Cunningham (see revisions)
 >  Documentation:  HELP * CALENDAR
 >  Related Files:
 */
compile_mode :pop11 +strict;

include sysdefs.ph;
weak constant procedure (vedobey);

define vars day_in_week(year) -> day;
;;; returns a number from 1 to 7 for day in week
    lvars year, day, month, date, century;
    if year.isinteger then
        -> month;
        -> date
    else
        pdtolist(incharitem(stringin(year))) -> year;
        erase(length(year));
        year --> [?date- ?month ?year ==];
        -year -> year;
        procedure;
            lvars l;
            [JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC]
                --> [??l ^month ==];
            length(l) + 1
        endprocedure() -> month
    endif;
    if month < 3 then
        month + 10 -> month;
        year - 1 -> year
    else
        month - 2 -> month
    endif;
    year // 100 -> century -> year;
    intof( 2.6 * month - 0.2 ) + date - (2 * century) + intof( century / 4 )
        + intof( year / 4 ) + year + 70 -> day;
    (day rem 7) + 1 -> day;
enddefine;

define vars day_of_week() -> day;
;;; converts a date to a weekday
    lvars day;
    [sunday monday tuesday wednesday thursday friday saturday](day_in_week())
        -> day
enddefine;

define vars isleap(year);
    lvars year;
    ((year rem 4) == 0 and not((year rem 100) == 0)) or (year rem 400) == 0
enddefine;

define global ved_calendar();
    ;;; make a calendar
    lvars year, day, month, date, month_table;
    if vedargument = nullstring then
        ;;; get year -- depends on operating system
#_IF DEF VMS
        substring(8, 4, sysdaytime()) -> vedargument;
#_ELSE      ;;; assume Unix format
        substring(25,4,sysdaytime()) -> vedargument;
#_ENDIF
    endif;
    strnumber(vedargument) -> year;
    day_in_week(1,1,year) -> day;
    [[      [' JANUARY ' 31]
            ['FEBRUARY ' 28]
            ['  MARCH  ' 31]
            ['  APRIL  ' 30]]
        [   ['   MAY   ' 31]
            ['  JUNE   ' 30]
            ['  JULY   ' 31]
            [' AUGUST  ' 31]]
        [   ['SEPTEMBER' 30]
            [' OCTOBER ' 31]
            ['NOVEMBER ' 30]
            ['DECEMBER ' 31]]] -> month_table;
    if year.isleap then 29 else 28 endif -> month_table(1)(2)(2);
    vedobey('calendar.lis',
        procedure;
            lvars row;
            ved_clear();
            false -> vedbreak;
            vedjumpto(1,58);
            vedinsertstring(vedargument);
            vednextline();
            for row in month_table do
                vedendfile();
                vednextline();
                for month in row do
                    vedinsertstring('         ');
                    vedinsertstring(hd(month));
                    vedinsertstring('            ')
                endfor;
                vednextline();
                repeat 4 times
                    vedinsertstring('SUN MON TUE WED THU FRI SAT   ')
                endrepeat;
                vednextline();
                vednextline();
                for month from 1 to 4 do
                    vedpositionpush();
                    vedjumpto(vedline,30*month + 4*day - 33);
                    for date from 1 to row(month)(2) do
                        if date < 10 then
                            vedcharinsert(` `)
                        endif;
                        vedinsertstring(date><'');
                        if day == 7 then
                            1 -> day;
                            vedjumpto(vedline+1,30*month - 29)
                        else
                            day + 1 -> day;
                            vedinsertstring('  ')
                        endif
                    endfor;
                    vedpositionpop()
                endfor
            endfor
        endprocedure)
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Cleaned up
 */
