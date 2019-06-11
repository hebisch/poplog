/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/vedtermsetup.p
 > Purpose:         Maintains VEDTERMSETUP as synonym for VEDSETUP
 > Author:          Rob Duncan, Jan 26 1990
 > Documentation:   REF * VEDSETUP
 > Related Files:
 */


section;

define global vars vedtermsetup();
    unless iscaller(vedsetup) then vedsetup() endunless;
enddefine;

endsection;
