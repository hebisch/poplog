/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_timed_esc.p
 > Purpose:         Procedure for creating an escape key that times out
 > Author:          John Williams, Oct  6 1989 (see revisions)
 > Documentation:   REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

global vars ved_timed_esc_timeout;
unless isinteger(ved_timed_esc_timeout)
or ved_timed_esc_timeout == false then
    ;;; default 1 sec
    1 -> ved_timed_esc_timeout
endunless;


/* This procedure reads a character, and precedes it with escape,
    provided the character was typed within a given time.
    Often assigned to numeric keypad 5
*/

define global ved_timed_esc();
    dlocal pop_timeout_secs;

    define dlocal pop_timeout();
        chainfrom(ved_timed_esc, vedscreenbell)
    enddefine;

    if isinteger(ved_timed_esc_timeout) then
        ved_timed_esc_timeout -> pop_timeout_secs;
        vedinput(vedinascii())
    endif;
    vedinput(vedescape)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 12 1991
        Rewritten to use pop_timeout
 */
