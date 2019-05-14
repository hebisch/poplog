/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/auto/line_repeater.p
 > Purpose:         Produce a line repeater for a given file
 > Author:          Aaron Sloman, Sep  9 1990 (see revisions)
 > Documentation:   REF * SYSIO/line_repeater
 > Related Files:   REF * CHARIO REF * SYS_READ_LINES, * VEDREADIN
 */

compile_mode:pop11 +strict;

section;

define global line_repeater(file, lim);
    lvars file, lim, dev, buffer;

    define lconstant Get_line(dev, buffer, lim) with_props line_repeater;
        lvars dev, buffer, lim, n;
        fast_sysread(dev, 1, buffer, lim) -> n;
        if n == 0 then
            unless systrmdev(dev) or dev == popdevin then
                sysclose(dev)
            endunless;
            termin
        else
            substring(1, n fi_- 1, buffer)
        endif
    enddefine;

    if isdevice(file) then
        file
        ;;; Should also check that the device was opened in "line" mode
    else
        sysopen(file, 0, "line", `N`)
    endif -> dev;

    lim or 80 -> lim;
    if isstring(lim) then
        lim -> buffer;
        datalength(buffer) -> lim;
    elseif isinteger(lim) then
        device_init_buffer(dev, lim + 1) -> buffer
    else
        mishap(file, lim, 2, 'STRING (buffer) OR INTEGER (buffer size) NEEDED')
    endif;

    Get_line(% dev, buffer, lim %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Julian Clinton, Jun 30 1999
        Now calls sysclose on device when EOF encountered.
--- Robert Duncan, May 16 1997
        Fixed to work with encoded input
--- John Williams, Jan 12 1993
        Now uses ERR_CHAR `N` when opening the device. Also, lim may be
        false, in which case it defaults to 80.
 */
