/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/is_syntax_word.p
 > Purpose:         Test for Pop-11 syntax word
 > Author:          John Gibson, Sep 29 1992
 > Documentation:   REF *IDENT
 */
compile_mode :pop11 +strict;

section;

define global is_syntax_word(item);
    lvars item;
    isword(item) and (identprops(item) ->> item) /== "undef"
    and isword(item) and isstartstring('syntax', item) and item
enddefine;

endsection;
