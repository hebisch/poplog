/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_gsp.p
 > Purpose:         <ENTER> gsp command
 > Author:          Jonathan Meyer, Sept 9 1992
 > Documentation:   REF *VEDSEARCH
 > Related Files:   SRC *VDREGEXP.P
 */
section;
compile_mode :pop11 +strict;

define global vars ved_gsp =
    ved_search_or_substitute(%'procedure -ask -here', true%)
enddefine;

endsection;
