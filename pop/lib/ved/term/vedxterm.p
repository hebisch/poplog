/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:            C.all/lib/ved/term/vedxterm.p
 > Purpose:         Ved configuration for xterm vt102 emulator
 > Author:          Roger Evans et al (see revisions)
 > Documentation:   HELP * VEDXTERM
 > Related Files:   LIB * VEDXTERMSCREEN, * VEDXTERMKEYS, * VEDXGOTOMOUSE
 */
compile_mode :pop11 +strict;

/*
    This library suite configures VED for use with the X Windows 'xterm' vt102
    emulator.  Because there are several keyboard configurations that all use
    the same basic terminal control functions a different library is needed
    for each keyboard type that might be encountered.  The main terminal
    control library is *vedxtermcore.p which is called by *vedxtermscreen.p
    The keyboard libraries are ved???xtermkeys.p.

    Although these keyboard libraries can be loaded explicitly if desired,
    the library files *vedxterm.p (this file) *vedxtermscreen.p and
    *vedxtermkeys.p  provide the ved start-up procedure -veduseterm- with a
    way of attempting to select the correct one automatically.

    The Sun function keys R1 to R15 do not do anything under the standard
    xterm VT100 emulation. For information on how to remedy this, see
    HELP * VEDXTERM

NOTE
    When loaded from -vedsetup-, this file actually runs -vedxterm-,
    thereby effecting the customisation.  When loaded explicitly
    (eg in saved image building), you have to run -vedxterm- yourself to
    achieve the setup, if you want it (hence your image can contain several
    terminal customisations and only select one at run-time).
    See REF * VEDTERMINALS and HELP * TERMINAL

*/

section;

uses-by_name vedxtermscreen, vedxtermkeys;

define vars vedxterm();
    veduseterm("xterm") -> ;
    identfn -> vedxterm;
enddefine;

if iscaller(vedsetup) then vedxterm() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  6 1991
        Put proper HELP file reference
--- Aaron Sloman, Aug 30 1990
        Updated the introductory comment, and put details in to
        REF * VEDXTERM
--- Rob Duncan, Oct  9 1989
        Sectionised
--- Andreas Schoter, Aug  1 1989
        Modified to work with VED's new startup process by subsuming all the
        existing specific vedxterm libraries under the control of single
        *screen and *keys files
 */
