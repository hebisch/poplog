/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedtrysetvalof.p
 >  Purpose:        used by ved_set and other commands to alter a global variable
 >  Author:         Aaron Sloman, 1983
 >  Documentation:
 >  Related Files:  LIB * VED_SET, * VED_BREAK, * VED_TABS etc.
 */
compile_mode :pop11 +strict;

section;

define vedtrysetvalof(w, iftrue, iffalse);
    lvars v, w, iftrue, iffalse;
    ;;; if the string is typed to ved, then check
    unless isword(w) then consword(w) -> w endunless;
    if isdefined(w) then
        valof(w) -> v;
        if isundef(v) or v == undef then
            vederror(w >< ' UNDEF - NOT INITIALISED')
        else
            not(v) ->> v -> valof(w);
            vedputmessage(if v then iftrue else iffalse endif)
        endif
    else
        vederror(w >< 'NOT A VARIABLE')
    endif
enddefine;

endsection;
