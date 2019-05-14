/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/auto/getc.p
 > Purpose:        Read a character from a device
 > Author:         Aaron Sloman, Nov 22 1986
 > Documentation:  HELP *POPPROCS/getc
 > Related Files:
 */
;;; added to library because found in HELP * POPPROCS
;;; previous file presumably lost?

section;

define global getc(device);
    lconstant s = ' ';
    lvars device;
    if sysread(device,s,1) == 0 then termin
    else fast_subscrs(1,s)
    endif;
enddefine;

endsection;
