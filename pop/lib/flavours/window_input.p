/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/window_input.p
 > Purpose:        a pdr for handling input to windows controlled by flavours
 > Author:         Mark Rubinstein, Jul  8 1986 (see revisions)
 > Documentation:
 > Related Files:  LIB * WINDOW_FLAVOURS
 */

section $-window_input => window_input report_or_send;

global vars report_mouse_events = false;

define report_input(input, window);
lvars input window;
    (if isvector(input) then
        'sequence "' sys_>< input
    else
        'character "' sys_>< consstring(input, 1)
    endif) <> '" received from window #' >< window<-name -> input;
    if report_mouse_events then
        if vedediting then
            vedputmessage(input);
            vedsetcursor();
        else
            pr(';;; ' <> input <> '\n');
        endif
    endif;
enddefine;

define global report_or_send(nargs, message, input, window, window_flave, nocheck);
lvars nargs message input window nocheck window_flave;
    if message == "updater" then
        nargs -> message; -> nargs;
        if nocheck or window_flave<- willrespondto(message, "updater") then
            -> window(message); ;;; send the message
        else
            erasenum(nargs);
            report_input(input, window);
        endif;
    else
        if nocheck or window_flave <- willrespondto(message) then
            window(message);        ;;; send the message;
        else
            erasenum(nargs);
            report_input(input, window);
        endif;
    endif;
enddefine;

define parse_button_input(string) -> button_number -> x -> y;
lvars string x y button_number;
    string(2) -> button_number;
    string(3) -> x;
    string(4) -> y;
enddefine;

define global window_input(input, sendpdr);
lconstant button_names = {left middle right};
lvars input button_message x y sendpdr inp = input(1);
    if isvector(input) then
        if inp == "mousexit" then
            sendpdr(0, "mouse_exited", input);
        elseif lmember(inp, [press release]) then
            parse_button_input(input) -> button_message -> x -> y;
            subscrv(button_message, button_names) -> button_message;
            if inp == "press" then
                button_message <> "_button_pushed"
            else
                button_message <> "_button_released"
            endif -> button_message;
            sendpdr(x, y, 2, button_message, input);
        elseif lmember(inp, [resized opened closed quitrequest]) then
            if inp == "closed" then
                sendpdr(0, "window_closed", input);
            elseif inp == "opened" then
                sendpdr(0, "window_opened", input);
            elseif inp == "resized" then
                sendpdr(0, "window_resized", input);
            else
                sendpdr(0, "quit_requested", input)
            endif;
        else
            sendpdr(input, 1, "unrecognized_input", input)
        endif;
    else
        sendpdr(input, 1, "nextchar", "updater", input);
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Poplog System, Sep 18 1987 (Ian Rogers)
    Removed vars window_input as it is already declared as a procedure.
--- Nic Ford, Aug 20 1987 - made -window_input- a var for -uses-
--- Ian Rogers - Nic Ford, Jul 20-21 1987
    Changed parse_button_input to parse new style (V13) vectors instead of
        old style (V12) strings.
    Changed window_input to accept new style words instead of old style
        strings
    Changed window_input to accept "window_resized" message.
--- Richard Bignell, Sep 24 1986 - exported report_or_send so that it is
    accessible by top section and therefore LIB * WINDOW_FLAVOURS.
*/
