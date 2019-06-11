/* --- Copyright University of Birmingham 2008. All rights reserved. ------
 > File:             $usepop/pop/lib/ved/ved_highlight.p
 > Purpose:         Highlight syntax words in a program file
                    (default is by underlining them)
 > Author:          Aaron Sloman, Nov 22 2007 (see revisions)
 > Documentation:   REF * vedcomms/ved_highlight
 > Related Files:   REF * vedcomms/ved_chat
 */

section;

global vars
    ;;; items not to be highlighted (user definable)
    highlight_exceptions,
    ;;; command to use for highlighting (user definable)
    ;;; this string is given as argument to veddo
    highlight_commandstring;

if isundef(highlight_exceptions) then

    [% "(", ")", "[", "]", "{", "}", ";", ":", ",", ".", "%",

        "->", """, "^", "^^", "!" %]

        -> highlight_exceptions
endif;

if isundef(highlight_commandstring) then

    ;;; For more options see REF * ved_chat
    ;;; (change 'u' to 'b' for bold)
    'chat -w u' -> highlight_commandstring

endif;

define ved_highlight;

    ;;; save current position;
    vedpositionpush();

    ;;; jump to top of file
    vedtopfile();
    ;;; highlight every syntax word in file

    repeat
        quitif(vedatend());
        lvars item = vedmoveitem();
        if is_syntax_word(item)
        and not(lmember(item, highlight_exceptions))
        then
            ;;; highlight item just read
            veddo(highlight_commandstring);
        endif;
    endrepeat;

    ;;; jump to saved position
    vedpositionpop();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Feb  4 2008
        Altered to make the highlight command changeable by the user,
        and to use the user's value for highlight_exceptions, if already
        defined.
 */
