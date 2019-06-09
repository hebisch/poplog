/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedinput.p
 > Purpose:         Adding events/strings to the ved input queue
 > Author:          Jonathan Meyer, Mar  8 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

#_INCLUDE 'xved_declare.ph'
include sigdefs.ph;

section $-xved;

vars
    xvedcurrinputwin = false,
;

lconstant macro (
    BUF_SIZE = 128,
);

lconstant
    input_buffer = writeable initv(BUF_SIZE),   ;;; BUF_SIZE char/event typeahead
;

lvars
    write_pos = 0,
    read_pos = 0,
    num_inputs = 0,
    in_read = false,
    last_event_window,
;

define :inline lconstant ADVANCE_POS(pos);
    (pos fi_+ 1) fi_mod BUF_SIZE
enddefine;

define :inline lconstant BUF_ENTRY(pos);
    subscrv(pos fi_+ 1, input_buffer)
enddefine;

;;; INTERNAL PROCEDURES

;;; Flush_in - ensure that input device events are up to date.
define lconstant Flush_in();
    if xvedappcontext and num_inputs /== 0 then
        fast_XptAppTryEvents(xvedappcontext);
    endif
enddefine;


/* ==== Registering Event Input ===================================== */
;;; Splices an event name and data into the device input stream

define xved_raise_event(window, name, data);
    lvars next_pos;

    if name == "userInterrupt" and vedinvedprocess then
        ;;; user interrupt bypass the event dispatching process
#_IF DEF sys_send_signal
        ;;; In Unix, send to process group (i.e. simulate Ctrl-C etc being
        ;;; typed in base window)
        sys_send_signal(0, SIG_INT) -> ;
#_ELSE
        sys_raise_ast(SIG_INT);
#_ENDIF
        return
    endif;

    unless window.wved_is_live_window then
        vederror('xved_raise_event: not a valid window');
    endunless;

    if window /== xvedcurrinputwin and window then
        ;;; change of window
        returnif((ADVANCE_POS(write_pos) ->> next_pos) == read_pos);
        window ->> xvedcurrinputwin -> BUF_ENTRY(write_pos);
        next_pos -> write_pos
    endif;

    returnif((ADVANCE_POS(write_pos) ->> next_pos) == read_pos);
    ;;; set input at write position
    conspair(name, data) -> BUF_ENTRY(write_pos);
    ;;; advance write position
    next_pos -> write_pos;
    ;;; increate char count - an event will also generate a single
    ;;; character output
    num_inputs fi_+ 1 -> num_inputs;

    unless in_read then
        if xvedisasynchronous then
            external_defer_apply(vedprocess_try_input)
        else
            XptSetXtWakeup()
        endif
    endunless;
enddefine;



/* ==== Registering String Input ===================================== */

;;; Public interface for adding new events and strings to input queue

define xved_raise_ascii(window, string);
    lvars window, string, n, m, c1, next_pos;

    unless window.wved_is_live_window then
        vederror('xved_raise_ascii: not a valid window');
    endunless;

    if isinteger(string) then
        ;;; single char
        string, 1;
        false -> string
    endif;

    if string then
        returnif((datalength(string) ->> n) == 0);
        subscrs(1, string)
    else
        ;;; n chars and n on the stack
        returnif((() ->> n) == 0);
        dup()
    endif -> c1;

    ;;; NB no good testing for veddointerrupt in vednormaltable here
    ;;; -- the procedure in vednormaltable isn't veddointerrupt
    if n == 1 and c1 == xvedinterruptchar then
        ;;; maps onto a interrupt char -- perform an interrupt
        unless string then -> endunless;
        chain(window, "userInterrupt", false, xved_raise_event)
    endif;

    if window /== xvedcurrinputwin then
        ;;; change of window
        returnif((ADVANCE_POS(write_pos) ->> next_pos) == read_pos);
        window ->> xvedcurrinputwin -> BUF_ENTRY(write_pos);
        next_pos -> write_pos
    endif;

    unless string or n fi_<= 16 then consstring(n) -> string endunless;

    if string and n fi_> 16 then
        ;;; make a character 'repeater'
        returnif((ADVANCE_POS(write_pos) ->> next_pos) == read_pos);
        consvector(1, string, 2) -> BUF_ENTRY(write_pos);
        next_pos -> write_pos;
        num_inputs fi_+ 1 -> num_inputs
    else
        if string then deststring(string) -> n endif;
        ;;; n chars now on stack
        n -> m;
        until n == 0 do
            ;;; set character at write position and advance write position.
            quitif((ADVANCE_POS(write_pos) ->> next_pos) == read_pos);
            subscr_stack(n) -> BUF_ENTRY(write_pos);
            next_pos -> write_pos;
            num_inputs fi_+ 1 -> num_inputs;
            n fi_- 1 -> n
        enduntil;
        erasenum(m)
    endif;

    unless in_read then
        if xvedisasynchronous then
            external_defer_apply(vedprocess_try_input)
        else
            XptSetXtWakeup()
        endif
    endunless
enddefine;


/* ======== Ved Screen Input Hooks ============================ */

lconstant macro (
    SELECT_WINDOW   = 2:1e0,
    ALLOW_WARP      = 2:1e1,
    SELECT_WINDOW_ALLOW_WARP = SELECT_WINDOW || ALLOW_WARP,
);

define lconstant get_next_event() -> event;
    BUF_ENTRY(read_pos) -> event;
    if isxvedwin(event) then
        event -> last_event_window;
        ADVANCE_POS(read_pos) -> read_pos;
        BUF_ENTRY(read_pos) -> event
    endif
enddefine;

;;; returns true if the next input is an event for window of specified type
define xved_is_next_event(window, type);
    lvars input;
    num_inputs /== 0
    and ved_char_in_stream == []
    and ispair(get_next_event() ->> input)
    and type == fast_front(input) and window == last_event_window
enddefine;

;;; returns true if input waiting

define :XVED_FOR vedscr_input_waiting() /* -> bool */;
    returnif(in_read)(false);
    Flush_in();
    num_inputs /== 0 and num_inputs
enddefine;

;;; clears any stuff on input buffer

define :XVED_FOR vedscr_clear_input();
    Flush_in();
    0 ->> write_pos ->> read_pos -> num_inputs;
    false -> xvedcurrinputwin
enddefine;


;;; read char/proc input, blocking until characters are available

define lconstant Read_input(allow_events) -> input;
    lvars allow_events, input, event, n, string, save_busy, window;

    define raise_timeout =
        xved_raise_event(%false, "readTimeout", false%)
    enddefine;

    define block_input();
        wvedwindow -> xvedblockinput;
        XptBusyCursorOn -> save_busy;
        false -> XptBusyCursorFeedback(wvedwindow.xvedwin_shell);
        true -> XptBusyCursorOn;
    enddefine;

    define exit_action();
        if xvedblockinput then
            false -> xvedblockinput;
            true -> XptBusyCursorFeedback(wvedwindow.xvedwin_shell);
            save_busy -> XptBusyCursorOn
        endif;
        false -> sys_timer(raise_timeout)
    enddefine;

    define event_flags(event);
        if event == "rowDragScroll" or event == "colDragScroll"
        or event == "dragLoad" or event == "clearSelection" then
            0
        elseif isstartstring('button', event) then
            SELECT_WINDOW
        else
            SELECT_WINDOW_ALLOW_WARP
        endif
    enddefine;


    dlocal 0 % , if dlocal_context fi_<= 2 then     ;;; not suspend/resume
                    exit_action()
                 endif%;

    ;;; flush output first - then flush input buffer
    vedscr_flush_output();

    Flush_in();
    dlocal in_read = true;

    if isinteger(pop_timeout_secs) then
        pop_timeout_secs * 1e6 -> sys_timer(raise_timeout)
    endif;

    repeat
        ;;; until we've got something to return
        while num_inputs == 0 do
            if ved_char_in_stream /== []
            and (allow_events or isinteger(hd(ved_char_in_stream)->>input)
                    or isstring(input))
            then
                return(vedgetinput() -> input)
            endif;
            ;;; wait for inputs
            unless allow_events or xvedblockinput then block_input() endunless;
            xved_rawin_read_trap()
        endwhile;

        ;;; get next event and window
        get_next_event() -> input;
        last_event_window -> window;

        ;;; check window set correctly
        if window /== wvedwindow then
            ;;; check if event requires window to be set
            if ispair(input) then
                event_flags(fast_front(input))
            else
                SELECT_WINDOW_ALLOW_WARP
            endif -> n;
            if n &&/=_0 SELECT_WINDOW then
                ;;; window must be set
                if xvedblockinput and xvedblockinput /== window then
                    ;;; don't allow window change when blocking input
                    vedscreenbell();
                    ADVANCE_POS(read_pos) -> read_pos;
                    num_inputs fi_- 1 -> num_inputs;
                    nextloop
                else
                    procedure(window, flags);
                        dlocal vedwarpcontext;
                        unless flags &&/=_0 ALLOW_WARP or wvedalwaysraise then
                            false -> vedwarpcontext
                        endunless;
                        xved_select_window(window)
                    endprocedure(window, n)
                endif
            endif
        endif;

        if isvector(input) then
            ;;; character repeater
            fast_subscrv(1,input) -> n;
            fast_subscrv(2,input) -> string;
            n fi_+ 1 -> fast_subscrv(1,input);
            fast_subscrs(n,string) -> input;
            returnunless(n == datalength(string))
        endif;

        ;;; advance read pointer
        ADVANCE_POS(read_pos) -> read_pos;
        num_inputs fi_- 1 -> num_inputs;

        returnif(isinteger(input));

        ;;; else it's an event pair
        sys_grbg_destpair(input) -> (event, input);
        event -> fast_subscrv(1, xvedrawdevindata);
        window -> fast_subscrv(2, xvedrawdevindata);
        input -> fast_subscrv(3, xvedrawdevindata);
        if event == "readTimeout" then
            ;;; test still an integer, just in case
            if isinteger(pop_timeout_secs) then pop_timeout() endif
        elseif event == "userInterrupt" then
            sys_raise_ast(SIG_INT)
        elseif allow_events then
            return(xved_process_event -> input)
        else
            unless xvedblockinput then block_input() endunless;
            ;;; when xvedblockinput is set, only events whose handlers
            ;;; have a pair in the pdprops are actioned
            xved_process_event()
        endif
    endrepeat
enddefine;      /* Read_input */

define :XVED_FOR vedscr_read_input();
    Read_input(true)
enddefine;

define :XVED_FOR vedscr_read_ascii();
    Read_input(false)
enddefine;


endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  6 1998
        Improved Read_input so that for most events, xved_select_window is
        called to set the file before the event is returned (so that the
        appropriate file is already set when ved_apply_action is called).
--- John Gibson, Oct 23 1998
        Move code in from xved*rawin.p and got rid of latter.
--- John Gibson, Jan 15 1994
        Allowed xved_raise_ascii to take false for string arg meaning
        n chars ... n on the stack
--- John Gibson, Jun 17 1992
        Changed xved_raise_event to call sys_send_signal in Unix for an
        interrupt
--- John Gibson, Mar 28 1992
        Moved 'event' action to xvedwm.p
--- John Gibson, Aug  3 1991
        Got rid of selectWindow event generation (now done in xvedrawin.p)
--- Jonathan Meyer, Jul 27 1991
        Added xved_before_editor_exit action
--- John Gibson, Jul 12 1991
        Added test for xvedinterruptchar in xved_raise_ascii
--- Jonathan Meyer, Jun 22 1991
        Added the 'event' action type. Removed xved*after_editor_exit action.
--- Jonathan Meyer, Jun 17 1991
        Renamed xved_raise_input xved_raise_ascii
--- John Gibson, Jun 15 1991
        Removed wvedbase/textwindow
--- Jonathan Meyer, Jun  5 1991
        Allowed xvedblockinput to be a window
--- John Gibson, Jun  4 1991
        Changed -xved_raise_event- to pass \^C for interrupt when
        not inside ved.
--- John Gibson, Jun  3 1991
        Changed chain(interrupt) to sys_raise_s*ignal and added test
        for vedinvedprocess
--- Jonathan Meyer, May 31 1991
        Changed response to "userInterrupt" events.
 */
