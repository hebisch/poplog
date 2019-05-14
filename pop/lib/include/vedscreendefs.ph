/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/include/vedscreendefs.ph
 > Purpose:         Flag bits for -vedscreencharmode- etc
 > Author:          John Gibson, Dec 20 1991 (see revisions)
 > Documentation:   REF *VEDVARS
 */

#_TERMIN_IF DEF VEDSCREENDEFS_INCLUDED

section;

iconstant macro (
    ;;; Flags for vedscreencharmode.

    VEDCMODE_GRAPHIC    = 2:1e0,

    ;;; Character attributes start at bit 16 (so they can be or'ed with
    ;;; a byte (or a 16-bit char) to produce a 24-bit char-with-attributes).
    ;;; N.B. XVed assumes these are the same as the XpwF- flags defined in
    ;;; XpwScrollText.ph, so keep the two in step.

    VEDCMODE_COLOURNUM  = 2:111e16, ;;; colour in 16-18
    VEDCMODE_UNDERLINE  = 2:1e19,   ;;; underlined
    VEDCMODE_BOLD       = 2:1e20,   ;;; bold chars
    VEDCMODE_ALTFONT    = 2:1e21,   ;;; alternative font, e.g. italic
    VEDCMODE_BLINK      = 2:1e22,   ;;; blinking
    VEDCMODE_ACTIVE     = 2:1e23,   ;;; 'active' bit

    ;;; shift up/down for colour number
    VEDCMODE_COLOUR_SHIFT = 16,

    ;;; attributes that make whitespace significant
    VEDCMODE_SP_SIG_BITS = VEDCMODE_COLOURNUM || VEDCMODE_UNDERLINE
                                || VEDCMODE_ACTIVE,

    ;;; attributes that don't affect whitespace at all
    VEDCMODE_SP_INSIG_BITS = VEDCMODE_BOLD || VEDCMODE_ALTFONT,

    ;;; main highlight colour, normally inverse video
    VEDCMODE_HIGHLIGHT  = 1 << VEDCMODE_COLOUR_SHIFT,

    ;;; alternate (conventionally foreground-only) colours
    VEDCMODE_COLOUR2    = 2 << VEDCMODE_COLOUR_SHIFT,
    VEDCMODE_COLOUR4    = 4 << VEDCMODE_COLOUR_SHIFT,
    VEDCMODE_COLOUR6    = 6 << VEDCMODE_COLOUR_SHIFT,

    ;;; alternate (conventionally background/foreground) colours
    VEDCMODE_COLOUR3    = 3 << VEDCMODE_COLOUR_SHIFT,
    VEDCMODE_COLOUR5    = 5 << VEDCMODE_COLOUR_SHIFT,
    VEDCMODE_COLOUR7    = 7 << VEDCMODE_COLOUR_SHIFT,

    );

iconstant VEDSCREENDEFS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 30 1995
        Added VEDCMODE_ACTIVE
--- John Gibson, Mar 15 1992
        Attributes now start at bit 16 instead of 8 (allows extension
        to 16 bit actual chars in the future).
--- John Gibson, Feb  2 1992
        Changed to allow 3 bits for colours.
--- John Gibson, Jan 25 1992
        Added VEDCMODE_BLINK
--- John Gibson, Jan 13 1992
        Added VEDCMODE_ALTFONT
 */
