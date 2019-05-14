/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/bits_out.p
 *  Purpose:        Write integers to disk in compact form
 *  Author:         John Williams, Jul 12 1985 (see revisions)
 *  Documentation:  HELP *BITS_OUT
 *  Related Files:  BITS_IN
 */

section $-lisp => bits_out;

define write_bits(int, device, nextunitpdr, unitsize) with_props false;
    lvars int device procedure nextunitpdr unitsize n_units nbytes;
    frozval(1, nextunitpdr) -> n_units;
    if int == termin then
        unless (((n_units fi_- 1) fi_* unitsize) fi_// 8 -> nbytes) == 0 do
            nextunitpdr(0);
            nbytes fi_+ 1 -> nbytes
        endunless;
        syswrite(device, frozval(2, nextunitpdr), nbytes);
        sysclose(device)
    else
        if n_units fi_> 8 then
            syswrite(device, frozval(2, nextunitpdr), unitsize);
            1 ->> n_units -> frozval(1, nextunitpdr)
        endif;
        nextunitpdr(int);
        n_units fi_+ 1 -> frozval(1, nextunitpdr)
    endif
enddefine;

define global procedure bits_out(file, spec) -> rep;
    lvars device file key rep spec;
    if isboolean(spec) then
        if spec then
            negate(file)
        else
            file
        endif -> spec;
        -> file
    endif;
    conskey("bits_in", spec) -> key;
    if isdevice(file) then
        file
    else
        syscreate(file, 1, false)
    endif -> device;
    write_bits(% device,
                 updater(class_fast_subscr(key))(% 1, class_init(key)(8) %),
                 abs(spec)
               %) -> rep;
    device_open_name(device) -> pdprops(rep)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Sep 24 1986
        Now accepts negative specs (meaning signed)
*/
