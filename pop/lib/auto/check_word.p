/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/check_word.p
 > Purpose:         Check item is a word
 > Author:          John Gibson, Jan 12 1993
 > Documentation:   REF *WORDS
 */
compile_mode :pop11 +strict :vm -pentch;

section;

define check_word(item) with_props false;
    lvars item;
    unless isword(item) then
        mishap(item, 1, 'WORD NEEDED')
    endunless
enddefine;

endsection;
