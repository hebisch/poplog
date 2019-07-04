/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiTimeUtils.p
 > Purpose:         Time and date utilities
 > Author:          Julian Clinton, April 1995 (see revisions)
 > Documentation:
 > Related Files:
*/

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section $-poplog_ui;

include sysdefs;
uses switchon;


/* ---------------------------------------------------------------
    Generating Time and Date Information In Different Formats
   --------------------------------------------------------------- */

;;; dumps the characters between two points in a string on the
;;; stack
define lconstant dump_chars(string, start, len);
lvars start, len, string;

    repeat len times
        subscrs(start, string);
        start+1->start;
    endrepeat;
enddefine;


#_IF DEF VMS

;;; VMS time format
;;;
lconstant procedure (
    get_hours = dump_chars(%13, 2%),
    get_mins = dump_chars(%16, 2%),
    get_secs = dump_chars(%19, 2%),

    get_date = dump_chars(%1, 2%),
    get_month = dump_chars(%4, 3%),
    get_year = dump_chars(%8, 4%),
    get_abbrevyear = dump_chars(%10, 2%),
);

#_ELSE

;;; UNIX time format
;;;
lconstant procedure (
    get_hours = dump_chars(%12, 2%),
    get_mins = dump_chars(%15, 2%),
    get_secs = dump_chars(%18, 2%),

    get_date = dump_chars(%9, 2%),
    get_month = dump_chars(%5, 3%),
    get_year = dump_chars(%25, 4%),
    get_abbrevyear = dump_chars(%27, 2%),
);

#_ENDIF

define lconstant get_monthnum(timestring);
lvars timestring;
lconstant month = writeable inits(3);

    fill(get_month(timestring), month) ->;
    switchon month
        case = 'Jan' then
            `0`, `1`;
        case = 'Feb' then
            `0`, `2`;
        case = 'Mar' then
            `0`, `3`;
        case = 'Apr' then
            `0`, `4`;
        case = 'May' then
            `0`, `5`;
        case = 'Jun' then
            `0`, `6`;
        case = 'Jul' then
            `0`, `7`;
        case = 'Aug' then
            `0`, `8`;
        case = 'Sep' then
            `0`, `9`;
        case = 'Oct' then
            `1`, `0`;
        case = 'Nov' then
            `1`, `1`;
        case = 'Dec' then
            `1`, `2`;
        else
            `0`, `0`;       ;;; error?
    endswitchon;
enddefine;


;;; process_time_string takes the new time string and substitutes
;;; certain substrings for actual values. The recognized substrings
;;; are:
;;;     'HH'        - hours     (checks to see whether 24 hour or am/pm)
;;;     'MM'        - minutes
;;;     'SS'        - seconds
;;;     ' (am/pm)'  - inserts am or pm depending on the hour value
;;;     ' (24 hr)'  - ignored
;;;
;;; The last 2 also signify the end of the string
;;;

lconstant
    am_pm = ' (am/pm)',
    t24_hr = ' (24 hr)',
;

define lconstant process_time_string(time_string, time_format);
lvars time_string, time_format, index = 1,
    is_pm = false, len = length(time_format);

    while index <= len do
        if issubstring_lim('HH', index, index, index+2, time_format) then

            get_hours(time_string);

            ;;; check if the user wants 12- or 24 hour time
            ;;;
            if issubstring(am_pm, time_format) then
                ;;; convert to 12 hour time
                lvars hr1, hr2;

                -> hr2 -> hr1;      ;;; retrieve chars from stack
                if (hr1 == `2`) or (hr1 == `1` and hr2 > `2`) then
                    ;;; must be afternoon
                    true -> is_pm;
                    (hr1 - `0`) * 10 + (hr2 - `0`) - 12 -> hr1;
                    if hr1 > 9 then
                        (hr1 div 10) + `0`
                    else
                        ` `     ;;; just put a space
                    endif;
                    (hr1 rem 10) + `0`;
                else
                    ;;; morning so no need to change
                    hr1, hr2;
                endif;
            endif;
            index + 2 -> index;
        elseif issubstring_lim('MM', index, index, index+2, time_format) then

            get_mins(time_string);
            index + 2 -> index;
        elseif issubstring_lim('SS', index, index, index+2, time_format) then

            get_secs(time_string);
            index + 2 -> index;
        elseif issubstring_lim(am_pm, index, index, index+8, time_format) then

            if is_pm then `p` else `a` endif, `m`;
            index + 8 -> index;
        elseif issubstring_lim(t24_hr, index, index, index+8, time_format) then
            index + 8 -> index;
        else
            subscrs(index, time_format);
            index + 1 -> index;
        endif;
    endwhile;
enddefine;


;;; process_date_string takes the new time string and substitutes
;;; certain substrings for actual values. The recognized substrings
;;; are:
;;;
;;;     DD      - the date
;;;     Mon     - 3 letter month identifier
;;;     MM      - 2 digit month identifier
;;;     YYYY    - a full year e.g. 1995
;;;     YY      - abbreviated year format e.g. 95
;;;
;;; The last 2 also signify the end of the string
;;;
define lconstant process_date_string(time_string, date_format);
lvars time_string, date_format, index = 1,
    is_pm = false, len = length(date_format);

    while index <= len do
        if issubstring_lim('DD', index, index, index+2, date_format) then
            get_date(time_string);
            index + 2 -> index;

        elseif issubstring_lim('Mon', index, index, index+3, date_format) then
            get_month(time_string);
            index + 3 -> index;

        elseif issubstring_lim('MM', index, index, index+2, date_format) then
            get_monthnum(time_string);
            index + 2 -> index;

        elseif issubstring_lim('YYYY', index, index, index+4, date_format) then
            get_year(time_string);
            index + 4 -> index;

        elseif issubstring_lim('YY', index, index, index+2, date_format) then
            get_abbrevyear(time_string);
            index + 2 -> index;

        else

            subscrs(index, date_format);
            index + 1 -> index;
        endif;
    endwhile;
enddefine;

define gen_date_chars(time, time_format, date_format, separator);
lvars time, time_format, date_format, separator;
lvars os_time = sys_convert_date(time, true);

    if time_format then
        process_time_string(os_time, time_format);
        if date_format and separator then
            ;;; add a separator before the date
            dump_chars(separator, 1, length(separator));
        endif;
    endif;

    if date_format then
        process_date_string(os_time, date_format);
    endif;
enddefine;

define gen_date_string(t, tf, df, s) -> str;
lvars t, tf, df, s, str;
    consstring(#| gen_date_chars(t, tf, df, s) |# ) -> str;
enddefine;

constant guiTimeUtils = true;

endsection; /* $-poplog_ui */
