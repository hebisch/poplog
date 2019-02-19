/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/sys_read_lines.p
 > Purpose:         Read specified range of lines from file
 > Author:          John Williams, Jan  3 1990 (see revisions)
 > Documentation:   REF * SYS_READ_LINES
 > Related Files:
 */


section;

#_INCLUDE 'declare.ph';

constant
        procedure (device_init_buffer)
    ;

weak constant
        procedure (vedfile_line_repeater)
    ;

lconstant macro BUF_LEN = 512;

define sys_read_lines(file, l1, l2);
    lvars dev, file, l1, l2, Ved_mode, buf;
    lvars _i;

    define lconstant Try_close_dev();
        false -> dev
    enddefine;

    define updaterof lconstant Try_close_dev();
        if dev and dev /== file then
            sysclose(dev)
        endif
    enddefine;

    dlocal 0 % Try_close_dev() %;

    if isboolean(l2) then
        l2 -> Ved_mode;
        l1 -> l2;
        file -> l1;
        -> file;
    else
        false -> Ved_mode;
    endif;

    define lconstant File_too_short_error();
        mishap(file, l2, 2, 'FILE TOO SHORT')
    enddefine;

    unless isinteger(l1) and isinteger(l2) and l2 fi_>= l1 do
        mishap(l1, l2, 2, 'BAD RANGE OF LINES')
    endunless;

    if isdevice(file) then
        file
    else
        sysopen(file, 0, "line", `N`)
    endif -> dev;

    device_init_buffer(dev, BUF_LEN) -> buf;

    fast_for _i from 1 to (l1 fi_- 1) do
        if fast_sysread(dev, 1, buf, BUF_LEN) == 0 then
            File_too_short_error()
        endif
    endfast_for;

    if Ved_mode then
        unless VED_LOADED then
            mishap(0, 'VED NOT LOADED');
        endunless;
        lblock;
            lvars line procedure rep;
            VED_WEAK vedfile_line_repeater(dev) -> rep;
            fast_for _i from l1 to l2 do
                if (rep() ->> line) == termin then
                    File_too_short_error()
                else
                    line
                endif;
            endfast_for;
        endlblock;
    else
        lblock;
            lvars _len;
            fast_for _i from l1 to l2 do
                fast_sysread(dev, 1, buf, BUF_LEN) -> _len;
                if _len == 0 then
                    File_too_short_error()
                else
                    substring(1, _len fi_- 1, buf)
                endif;
            endfast_for;
        endlblock;
    endif;

    l2 fi_- l1 fi_+ 1
enddefine;


endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 12 1997
        Changed to use new device_init_buffer.
--- John Gibson, Mar  5 1997
        Changed to use its own buffer allocated with inits(16)
--- John Gibson, Jun  1 1993
        Removed include for syspop.ph (included by declare.ph already)
--- John Williams, Jun  1 1993
        Added new Ved_mode optional argument (for ved_? and ved_??)
--- John Williams, May 27 1993
        Added missing argument 2 to call of MISHAP on 'BAD RANGE OF LINES'
--- John Gibson, May 28 1990
        Changed to use new arg to -sysopen-
 */
