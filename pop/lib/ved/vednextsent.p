/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.all/lib/ved/vednextsent.p
 > Purpose:         Find start of a sentence in VED
 > Author:          Aaron Sloman, May  2 1988 (see revisions)
 > Documentation:   REF *VEDPROCS
 > Related Filed:   C.all/lib/ved/vedprevsent.p C.all/lib/ved/vednextsentend.p
 >                  C.all/lib/ved/vedprevsentend.p
 */
compile_mode :pop11 +strict;

section;

;;; Whitespace characters
lvars whitespace = '\s\t';

;;; Characters that, by themselves, indicate the end of a sentence
lvars sentence_stops = '.!?';

;;; Characters that, preceded by a member of -sentence_stops-, indicates the
;;; end of a sentence
lvars odd_sentence_stops = '}])"\'';

;;; Like -vedcharnext- but doesn't use -vednextline- (see BR davidy.94)
define lconstant Char_forward();
    if vedcolumn fi_> vvedlinesize then
        vedchardown(); 1 -> vedcolumn
    else
        vedcharright()
    endif
enddefine;

;;; Moves to next non-whitespace character, or the end of the file
define lconstant move_to_next_non_whitespace();
    returnif(vedatend());
    repeat;
        Char_forward();
        unless  strmember(vedcurrentchar(), whitespace)
                and not(vedatend())
        then
            return();
        endunless;
    endrepeat;
enddefine;


;;; Returns the previous character, or the current character if at the start of
;;; the file
define lconstant get_last_char();
    returnif(vedatstart())(vedcurrentchar());
    vedcharleft();
    vedcurrentchar();
    Char_forward();
enddefine;


;;; Returns the length of the next line, or 0 if at the end of the file
define lconstant get_next_line_size();
    returnif(vedatend())(0);
    vedchardown();
    vvedlinesize;
    vedcharup();
enddefine;


;;; Returns true if character under cursor is a possible end of sentence
define lconstant is_sentence_end();
    lvars current_char = vedcurrentchar();
    lvars next_char, next_line_size, last_char;

    ;;; Call the end of the file the end of a sentence
    returnif(vedatend())(true);

    ;;; Ignore '.' at start of line since they are often text formatting
    ;;; commands
    returnif(vedcolumn == 1 and current_char == `.`)(false);

    get_next_line_size() -> next_line_size;

    ;;; If we're at the end of a line, and the next line is blank, call it the
    ;;; end of a sentence
    returnif(next_line_size == 0 and vvedlinesize == vedcolumn)(true);

    get_last_char() -> last_char;

    ;;; If we don't have end of sentence punctuation, we're not at the end of
    ;;; a sentence
    unless strmember(current_char, sentence_stops)
    or (strmember(last_char, sentence_stops)
        and strmember(current_char, odd_sentence_stops)
    ) do;
        return(false);
    endunless;

    vedpositionpush();
    move_to_next_non_whitespace();
    vedcurrentchar() -> next_char;
    vedpositionpop();

    ;;; We're at the end of the sentence if the end of sentence punctuation is
    ;;; followed by a capital letter, or the end of the file
    next_char == `\s` or isuppercode(next_char)

enddefine;


;;; Find the last occurance of the end of a sentence in a file
define global vedprevsentend();
    until (vedcharleft();is_sentence_end()) do;
        returnif(vedatstart());
    enduntil;
enddefine;

;;; Find the next occurance of the end of a sentence in a file
define global vednextsentend();
    if vedatend() then
        vederror('END OF FILE');
    endif;
    until (Char_forward();is_sentence_end()) do;
        returnif(vedatend());
    enduntil;
enddefine;

;;; Find the last occurance of the start of a sentence in a file
define global vedprevsent();
    lvars line = vedline, col = vedcolumn;
    vedprevsentend();
    unless vedatstart() do;
        move_to_next_non_whitespace();
    endunless;
    if line < vedline
    or (line == vedline and col <= vedcolumn)
    then
        vedprevsentend();
        vedprevsent();
    endif;
enddefine;

;;; Find the next occurance of the start of a sentence in a file
define global vednextsent();
    lvars line = vedline, col = vedcolumn;
    unless vedatstart() do;
        vedprevsentend();
        move_to_next_non_whitespace();
        returnif(vedline > line or (vedline == line and vedcolumn > col));
    endunless;
    vednextsentend();
    move_to_next_non_whitespace();
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May  3 1995
        Fixed BR davidy.94 (vedprevsent can go into infinite recursion
        if vedleftmargin /= 0).
--- Adrian Howard, Jul 31 1991
        - Rewrote large chunks
        - Added -vednextsentend- and -vedprevsentend-
--- Rob Duncan, May  8 1990
        Changed -vedlastsent- to -vedprevsent- (analogous to -vedprevline-).
--- Andreas Schoter, 6 Sep 1989
        Removed from LIB * VEDEMACS and made autoloadable.
 */
