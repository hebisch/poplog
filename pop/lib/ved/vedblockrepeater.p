/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/vedblockrepeater.p
 > Purpose:         Created a character repeater for a vector of strings
 > Author:          Aaron Sloman, Dec  8 1989
 > Documentation:   HELP * VEDBLOCKREPEATER
 > Related Files:   LIB * VEDBLOCKS
 */

;;; Create a character repeater for text in a vector of strings, such as
;;; might be created by vedcutblock


define lconstant Stringsin(len,statepair);
    ;;; A closure of this is a character repeater
    ;;; statepair contains a subscript and a list of strings,
    ;;; all assumed to be of the same length, len, as produced by
    ;;; -vedcutblock-
    lvars _i, len, statepair, string, strings;
    fast_destpair(statepair) -> strings -> _i;
    if null(strings) then
        termin
    else
        fast_front(strings) -> string;
        if _i fi_> len then
            ;;; end of that line, prepare for next one
            fast_back(strings) ->  fast_back(statepair);
            1 -> fast_front(statepair);
            `\n`
        else
            fast_subscrs(_i, string);
            _i fi_+ 1 -> fast_front(statepair)
        endif
    endif
enddefine;


define global vedblockrepeater(strings) -> pdr;
    ;;; Strings is the vector of strings. Return a procedure
    lvars pdr, strings;
    unless isvector(strings) then
        mishap(strings, 1, 'VECTOR OF STRINGS REQUIRED')
    endunless;
    ;;; Now replace it with a list of strings.
    datalist(strings) -> strings;
    Stringsin(%datalength(hd(strings)), conspair(1, strings) %)
        -> pdr;
    "vedblockrepeater" -> pdprops(pdr)
enddefine;
