/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/vedteststartsearch.p
 > Purpose:         Search in VED buffer for line starting with string
 > Author:          Aaron Sloman, Oct 26 1988 (see revisions)
 > Documentation:   HELP * VEDPROCS/vedteststartsearch
 > Related Files:   LIB * VED_G LIB * VED_INDEXIFY
 */
compile_mode :pop11 +strict;

section;

;;; Different from VED search mechanisms, because patterns are not used.

define lconstant searchinrange(string, line1, line2) -> line1;
    ;;; Look for line between line1 and line2 starting with string.
    ;;; Used by vedteststartsearch
    lvars string, line1, line2;
    fast_for line1 from max(1, line1) to min(vvedbuffersize, line2) do
    returnif(isstartstring(string, fast_subscrv(line1, vedbuffer)))
    endfor;
    false -> line1;
enddefine;

define vedteststartsearch(string) -> line;
    ;;; Search from current location for string, wrapping if necessary
    lvars string, line;
    if     (searchinrange(string, vedline + 1, vvedbuffersize) ->> line)
    or     (searchinrange(string, 1, vedline) ->> line)
    then   vedjumpto(line, 1)
    else   false -> line
    endif
enddefine;

endsection;
