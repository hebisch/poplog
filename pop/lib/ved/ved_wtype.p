/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_wtype.p
 >  Purpose:        discover the type of identifier to right of cursor
 >  Author:         Aaron Sloman, Jan 1985 (see revisions)
 >  Documentation:  HELP * VEDCOMMS
 */

;;; Prints out on status line the type of thing to right of the cursor

define ved_wtype;
lvars item props val message;
    if vedline > vvedbuffersize
    or vedcolumn > vvedlinesize then
        vederror('PAST END OF TEXT')
    endif;

    if vedcurrentchar() == `\`` then
        vedputmessage('CHARACTER CONSTANT: ' sys_>< vednextitem());
        return()
    endif;
    vednextitem() -> item;
    if isnumber(item) then vedputmessage('NUMBER'); return
    elseif isstring(item) then vedputmessage('STRING'); return
    endif;

    ;;; it's a word
    identprops(item) -> props;
    if props == undef then
        '" is not in the current section'
    elseif isactive(item) then
        '": active variable, multiplicity: ' sys_>< isactive(item)
    else
        valof(item) -> val;

        if props /== "macro"
        and (isword(val) or isnumber(val) or isboolean(val)) then
            '": variable with value: ' sys_>< val
        elseif props == 0 then
            '": variable, with value of type: ' sys_>< dataword(val)
        elseif isnumber(props) then
            '": operator with precedence: ' sys_>< props
        elseif props == "macro" then
            '": macro (' sys_><
            if islist(val) then 'list)' else dataword(val) sys_>< ')' endif
        elseif props == "syntax" then
            '": syntax word'
        elseif isstartstring('syntax ', props) then
            '": syntax operator, precedence: ' sys_>< allbutfirst(7,props)
        else
            '": type ' sys_>< props
        endif;
    endif -> message;
    vedputmessage('"' sys_>< item sys_>< message)
enddefine;

/*  --- Revision History ---------------------------------------------------
--- James Goodlet, Jun 24 1991 - ':-' replaced by ':' in output messages.
        Too confusing with negative precedences.
--- Aaron Sloman, Apr  2 1989  fixed to deal with active variables and
        syntactic operators.
--- Aaron Sloman, Nov 20 1986 messages shortened. Now works with character
        constants
--- Aaron Sloman, Nov  5 1986 - made to work with active variables
--- Mark Rubinstein, Jun  7 1985 - altered the message relating to undeclared
        variables, and made it print values of word, integer and boolean
        variables.
--- Jonathan Laventhol, January 1985  - Simplified and some more information
        given about different identprops.
 */
