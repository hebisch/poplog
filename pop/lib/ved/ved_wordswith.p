/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_wordswith.p
 > Purpose:         Display all words in dictionary containing vedargument
 > Author:          Aaron Sloman, Apr  3 1988 (see revisions)
 > Documentation:   HELP * WORDSWITH, HELP * VEDCOMMS
 > Related Files:   LIB * VEDWORDSWITH, LIB * MATCH_WORDSWITH
 */

/*
A VED-based version of -match_wordswith-

For a full description see HELP * WORDSWITH

ved_wordswith calls match_wordswith(vedargument) to obtain a list of
words in the dictionary matching vedargument. The list is displayed
in a temporary VED file, using the format controller -wordswith_spacer-.

Control of formatting:

The global variable -wordswith_spacer- must have a procedure or an
integer as its value. If it is an integer N, then N spaces are left
after each word when they are displayed in the file.

If it is a procedure, then the procedure is run after every word
is inserted in the file, to find the location to start the next word.

The most commonly used value for -wordswith_spacer- is likely to be
-vedtabright-, which will display the words in columns. To ensure
that this works, the procedure -ved_wordswith- locally changes
-vedindentstep- to be one more than the length of the longest word found.

The default value of -wordswith_spacer- is -vedtabright-, which causes
the words to be displayed in columns.

*/

section;

global vars wordswith_spacer;
if isundef(wordswith_spacer) then
    vedtabright -> wordswith_spacer         ;;; default = display in columns
endif;


define global procedure ved_wordswith;
    ;;; uses vedargument, which may contain pattern elements
    dlocal vedediting vedindentstep;
    lvars list, maxlen=0, word;

    match_wordswith(vedargument, vedwildcards) -> list;

    if list == [] then vederror('NO WORDS FOUND') endif;

    ;;; get temporary file
    vededit(systmpfile(false, 'wordswith', ''), vedhelpdefaults);
    ;;; find length of longest word
    fast_for word in list do
        max(maxlen,datalength(word)) -> maxlen
    endfast_for;
    maxlen + 1 -> vedindentstep;    ;;; defines column width
    false -> vedbreak;              ;;; line breaks handled below
    false -> vedediting;
    ;;; insert words in VED buffer
    fast_for word in list do
        if vedcolumn fi_+ datalength(word) fi_> vedlinemax then
            vedlinebelow()
        endif;
        vedinsertstring(word);
        if isinteger(wordswith_spacer) then
            wordswith_spacer fi_+ vedcolumn -> vedcolumn
        elseif isprocedure(wordswith_spacer) then
            wordswith_spacer()
        else
            vederror(wordswith_spacer sys_><
                ' -- INTEGER OR PROCEDURE NEEDED FOR WORDSWITH')
        endif
    endfast_for;
    vedtopfile();
    chain(vedrefresh)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 23 1995
        Replaced vars with dlocal
--- Jonathan Meyer, Sep 29 1993
        Added -vedwildcards- reference.
 */
