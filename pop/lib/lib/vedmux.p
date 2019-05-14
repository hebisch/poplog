/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.unix/lib/lib/vedmux.p
 > Purpose:        VED interface to LIB MUX - for input multiplexing
 > Author:         Roger Evans, Dec 10 1986 (see revisions)
 > Documentation:  HELP VEDMUX
 > Related Files:  LIB MUX, LIB VSH
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

/*  LIB VEDMUX       R.Evans November 1986

    UNSUPPORTED - SEE DISCLAIMER IN HELP FILE

    VEDMUX interfaces LIB MUX to VED, redirecting keyboard input from
    rawcharin through the multiplexed input routines. This allows
    POPLOG to monitr and react to other input channels while allowing
    normal ved editing to occur. SeeLIb VED_VSH for an example of use.
*/

uses mux   ;;; basic input multiplexer library

section $-library => ved_mux ved_nomux mux_char vedmux;

/* for saving original rawcharin */
vars procedure mux_oldcharin;

/* new version of rawcharin - watches all input streams simultaneously */
define mux_newcharin -> mux_char;
    /* check for waiting terminal input first */
    if pop_timeout_secs or sys_input_waiting(poprawdevin) then
        mux_oldcharin() -> mux_char;
    else
        /* handle all input events (including terminal) till one
           returns a character in mux_char */
        false -> mux_char;
        until mux_char do
            applist(mux_input(),apply);
        enduntil;
    endif;
enddefine;

/* procedure to run when poprawdevin responds -
   read char from real rawcharin into mux_char */
define mux_getchar;
    mux_oldcharin() -> mux_char;
enddefine;

/* turn input multiplexing on */
define global ved_mux;
    unless mux_newcharin == rawcharin then
        mux_getchar -> mux_entry(poprawdevin);
        rawcharin -> mux_oldcharin;
        mux_newcharin -> rawcharin;
    endunless;
enddefine;

/* turn multiplexing off */
define global ved_nomux;
    if mux_newcharin == rawcharin then
        mux_oldcharin -> rawcharin;
        false -> mux_entry(poprawdevin);
    endif;
enddefine;

global constant vedmux = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1992
        Made it swap rawcharin at runtime
--- Robert John Duncan, Jun 29 1992
        Moved to C.unix
--- Aaron Sloman, Oct 24 1988
    Put in non-autoloadable public library
 */
