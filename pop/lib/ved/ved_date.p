/*  --- University of Sussex POPLOG file -----------------------------------
 > File:           $usepop/master/C.all/lib/ved/ved_date.p
 > Purpose:        insert/display the date
 > Author:         unknown, ???
 > Documentation:
 > Related Files:
 */

section;

define global ved_date();
    if vedargument /= '' then
        vedinsertstring
    else
        vedputmessage
    endif (sysdaytime())
enddefine;

endsection;
