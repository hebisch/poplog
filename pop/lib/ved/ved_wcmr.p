/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_wcmr.p
 >  Purpose:        count words in a marked range
 >  Author:         Tom Khabaza, March 1985
 >  Documentation:  HELP * WC
 >  Related Files:  LIB * VED_WC, VEDWORDCOUNT_TEXT
 */
compile_mode :pop11 +strict;

section;

define vars ved_wcmr;
    if vvedmarklo > vvedbuffersize then
        ;;; if the whole marked range is off the end of the buffer
        vedputmessage('Marked range is empty.');
    elseif vvedmarkhi > vvedbuffersize then
        ;;; if its only the end of the range thats off the end
        ;;; then just do the count to the end of file.
        vedwordcount_text(vvedmarklo, vvedbuffersize);
    else
        vedwordcount_text(vvedmarklo, vvedmarkhi);
    endif;
enddefine;

endsection;
