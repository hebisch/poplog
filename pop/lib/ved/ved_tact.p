/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_tact.p
 > Purpose:         Manipulate text actions
 > Author:          John Gibson, Nov  3 1995
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_tact;
    lvars c, n, (data, line, col) = ved_text_action_data(vedline, vedcolumn);

    unless line then
        vederror('NOT INSIDE AN ACTIVE SEGMENT')
    elseunless col then
        vederror('ACTIVE SEGMENT IS ALL SPACES')
    else
        vedjumpto(line, col)
    endunless;

    if vedargument = nullstring then
        'd' -> vedargument;
        ved_cc()
    elseif (vedargument(1) ->> c) == `=` or c == `:` then
        if skipchar(`\s`,2,vedargument) ->> n then
            allbutfirst(n-1, vedargument) -> data;
            if c == `:` then
                pop11_compile(stringin(data)) -> data;
                if data = nullstring then
                    vederror('CAN\'T SET TEXT ACTION TO NULLSTRING')
                elseunless isstring(data) or isword(data) then
                    vederror('ILLEGAL VALUE FOR TEXT ACTION')
                endif
            endif;
            conspair(vedcurrentdchar(), data) -> vedcurrentvedchar()
        elseif c == `=` then
            if isstring(data) then
                vedputcommand('tact = ' <> data);
                vedstatusswitch()
            else
                ;;; if no existing data, or not a string, just display it
                'd' -> vedargument;
                ved_cc()
            endif
        else
            vederror('POP-11 EXPRESSION IS EMPTY')
        endif
    else
        vederror('USAGE: \{b}tact\{} [ \[b]= [ \{i}chars\{} ] | \[b]: \{i}expression\{} ]')
    endif
enddefine;

endsection;
