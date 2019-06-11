/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.unix/lib/ved/vedstop.p
 > Purpose:        Frees the 'local' control characters for use by VED.
 > Author:         Chris Slymon, Aug 1984 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*  Frees the 'local' control characters for use by VED. If the stop
    character was previously defined, VED_STOP is assigned to that character
    within VED.

    Equivalent to, but faster than
        sysobey('stty susp undef weras undef rprnt undef\
                flush undef lnext undef dsusp undef',`%`)

*/

section $-library$-temp;

vars temp stopch temp2;
inits(6) -> temp;

unless sys_io_control(poprawdevin,
            (16:40000000 + (6<<16) + (`t`<<8) + 116), temp)
    and
       ( temp(1) -> stopch;
         sys_io_control( poprawdevin, (16:80000000 + (6<<16) + (`t`<<8) + 117),
                fill( repeat 6 times 255 endrepeat, temp))) then
    mishap(0,'ERROR IN LIB VEDSTOP')
endunless;

if stopch < 127 then
    unless isinheap(vednormaltable) then
        copy(vednormaltable) -> vednormaltable;
    endunless;
    ved_stop -> vednormaltable(stopch);
endif;

section_cancel(current_section,true);
endsection;



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 29 1992
        Fixed mishap message; moved to C.unix
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevin-
 */
