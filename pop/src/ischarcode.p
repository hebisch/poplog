/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/ischarcode.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STRINGS
 */

;;; --------------- TESTING FOR CHARACTER RANGES -------------------------

#_INCLUDE 'declare.ph'

;;; ---------------------------------------------------------------------

global vars
    pop_character_set = `1`;        ;;; i.e. ISO Latin 1

lconstant macro (
    _Agrave     = _16:C0,
    _multiply   = _16:D7,
    _Thorn      = _16:DE,
    _agrave     = _16:E0,
    _divide     = _16:F7,
    _thorn      = _16:FE,
);

define :inline lconstant ALPHA_TEST(_lo, _hi, _ILlo, _ILhi, _ILex);
    if isinteger(_item) and (_int(_item) ->> _item) _lteq _:16:FFFFFF then
        _item _bimask _:16:FFFF -> _item;
        (_lo _lteq _item and _item _lteq _hi)
        ;;; (a/A)grave <= item <= (t/T)horn and item /== (divide/multiply)
        or (_ILlo _lteq _item and _item _lteq _ILhi and _item /== _ILex
            and pop_character_set == `1`)
    else
        false
    endif
enddefine;

define isuppercode(_item);
    lvars _item;
    ALPHA_TEST(_:`A`, _:`Z`, _Agrave, _Thorn, _multiply)
enddefine;

define islowercode(_item);
    lvars _item;
    ALPHA_TEST(_:`a`, _:`z`, _agrave, _thorn, _divide)
enddefine;

    /*  N.B. For ISO Latin, isalphacode(c) is NOT the same as
        isuppercode(c) or islowercode(c) (because german double s
        and y dieresis don't have alternate case equivalents, but these
        are included by isalphacode).
    */
define isalphacode(_item);
    lvars _item;
    if isinteger(_item) and (_int(_item) ->> _item) _lteq _:16:FFFFFF then
        _item _bimask _:16:FFFF -> _item;
        (_:`A` _lteq _item and _item _lteq _:`z`
            and not(_:`Z` _lt _item and _item _lt _:`a`))
        or (_Agrave _lteq _item and _item /== _multiply and _item /== _divide
            and pop_character_set == `1`)
    else
        false
    endif
enddefine;

define isnumbercode(_item);
    lvars _item;
    if isinteger(_item) and (_int(_item) ->> _item) _lteq _:16:FFFFFF then
        _item _bimask _:16:FFFF -> _item;
        _:`0` _lteq _item and _item _lteq _:`9`
    else
        false
    endif
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 30 1992
        pop_i*so_latin replaced with more general -pop_character_set- with
        value `1` to mean ISO latin 1.
--- John Gibson, Mar 15 1992
        Attributes moved to bits 16-23 of 24-bit integer character, thus
        allowing char code to be 16-bit
--- John Gibson, Feb 24 1992
        Added ISO Latin 1 support
--- John Gibson, Jan 18 1992
        Made procedures allow for 16-bit characters
--- John Gibson, Mar 31 1988
        Moved out of util.p
 */
