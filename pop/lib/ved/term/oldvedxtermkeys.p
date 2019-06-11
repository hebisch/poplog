/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedxtermkeys.p
 > Purpose:         Key bindings for XTERM terminal emulator
 > Author:          Rob Duncan, Nov 27 1989 (see revisions)
 > Documentation:   HELP * VEDXTERM
 > Related Files:   LIB * VEDHPXTERMKEYS, * VEDSUNXTERMKEYS, * VEDVT220KEYS
 */
compile_mode :pop11 +strict;

/*
    Choose a keyboard configuration for an XTERM terminal emulator.

    This is highly site-specific, since xterms may run on various hosts,
    all with different keyboards. The method given here simply chooses
    between HP, DEC and Sun keyboards depending on the host on which the
    library itself is loaded: this will fail if the terminal emulator
    is running remotely on a different type of machine.

    Sites requiring a more complex selection mechanism can provide a
    local version of this file in POPLOCALAUTO. See HELP * VEDXTERM for
    more details.
 */


include sysdefs.ph;

section;

#_IF DEF SUN

uses vedsunxtermkeys;

define vedxtermkeys();
    vedsunxtermkeys()
enddefine;

#_ELSEIF DEF HP9000

uses vedhpxtermkeys;

define vedxtermkeys();
    vedhpxtermkeys()
enddefine;

#_ELSEIF DEF DECSTATION

uses veddxtermkeys;

define vedxtermkeys();
    veddxtermkeys()
enddefine;

#_ELSE

/* Assume X terminal */

uses vedncdxtermkeys;

define vedxtermkeys();
    vedncdxtermkeys()
enddefine;

#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 15 1990
        Defaults to NCD. Uses LIB * VEDDXTERMKEYS for Decstation.
--- Rob Duncan, Jun 21 1990
        Added DECSTATION
--- John Williams, Jan 15 1990
        Fixed misspelt "enddefine"
--- Rob Duncan, Nov 27 1989
        Installed in place of the previous Sussex-specific version
 */
