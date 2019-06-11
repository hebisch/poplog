/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_sgs.p
 > Purpose:         <ENTER> sgs command
 > Author:          Jonathan Meyer, Sept 9 1992 (see revisions)
 > Documentation:   REF * VEDSEARCH
 > Related Files:   LIB * VED_SGSR, SRC * VDREGEXP.P
 */

section;

compile_mode :pop11 +strict;

define global vars ved_sgs =
    ved_search_or_substitute(%'-ask -verbose -here', true%)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 12 1994
        -verbose instead of +silent.
 */
