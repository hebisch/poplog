/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_gsl.p
 > Purpose:         <ENTER> gsl command
 > Author:          Jonathan Meyer, Sept 9 1992
 > Documentation:   REF *VEDSEARCH
 > Related Files:   SRC *VDREGEXP.P
 */
section;
compile_mode :pop11 +strict;

define global vars ved_gsl =
    ved_search_or_substitute(%'line -ask -here', true%)
enddefine;

endsection;
