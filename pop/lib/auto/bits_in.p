/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           $usepop/master/C.all/lib/auto/bits_in.p
 >  Purpose:        Read integers from a specially compacted file
 >  Author:         John Williams, Jul 12 1985 (see revisions)
 >  Documentation:  HELP * BITS_IN
 >  Related Files:  LIB * BITS_OUT
 */

section $-lisp => bits_in;

define read_bits(device, nextunitpdr, nbits_ref, unitsize) with_props false;
    lvars device procedure nextunitpdr nbits_ref unitsize n_units nbits;
    frozval(1, nextunitpdr) -> n_units;
    fast_cont(nbits_ref) -> nbits;
    if (n_units fi_* unitsize) fi_> nbits then
        sysread(device, frozval(2, nextunitpdr), unitsize) fi_* 8 -> nbits;
        nbits -> fast_cont(nbits_ref);
        1 ->> n_units -> frozval(1, nextunitpdr)
    endif;
    if nbits fi_< unitsize then
        sysclose(device);
        termin
    else
        nextunitpdr();
        n_units fi_+ 1 -> frozval(1, nextunitpdr)
    endif
enddefine;

define updaterof read_bits() with_nargs 5;
    erasenum(5);
    mishap(0, 'CANNOT UNREAD TO BINARY STREAM')
enddefine;

define global procedure bits_in(file, spec) -> rep;
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
        file -> device
    elseunless (sysopen(file, 0, false) ->> device) do
        mishap(file, 1, 'FILE NOT FOUND')
    endif;
    read_bits(% device,
                class_fast_subscr(key)(% 1, class_init(key)(8) %),
                consref(0),
                abs(spec)
              %) -> rep;
    device_open_name(device) -> pdprops(rep)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Jun 16 1987
        Provided updater for 'read_bits' (for benefit of Clisp)
--- John Williams, Sep 24 1986
        Now accepts negative specs (meaning signed)
*/
