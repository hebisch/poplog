/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/vedappendrange.p
 > Purpose:         Append the marked range to a file
 > Author:          Robert John Duncan, Jan 14 1994
 > Documentation:   REF * vedappendrange
 > Related Files:   SRC * VDFILEIO (for defn. of vedwriterange)
 */
compile_mode:pop11 +strict;

section;

;;; vedappendrange(file, line_1, line_2)
;;;     Like vedwriterange, but appends to file instead of overwriting it

define vedappendrange(file, line_1, line_2);
    lvars file, line_1, line_2;
    lvars consume = discappend(file); ;;; seek to end
    if isdevice(file) then
        vedwriterange(file, line_1, line_2);
    else
        lvars descr = file sys_>< ', ' sys_>< (line_2 - line_1 + 1) sys_>< ' LINES';
        vedputmessage('APPENDING TO ' sys_>< descr);
        vedwriterange(discout_device(consume), line_1, line_2);
        consume(termin);
        vedputmessage(descr sys_>< ' APPENDED');
    endif;
enddefine;

endsection;
