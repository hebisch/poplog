/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.unix/lib/ved/vedpipein.p
 > Purpose:         Run a unix command and put its output into VED buffer
 > Author:          Aaron Sloman, Apr 19 1988 (see revisions)
 > Documentation:   HELP * PIPEUTILS, * PIPEIN
 > Related Files: LIB * PIPEIN, *VEDGENSHELL, *VED_SH, *VED_CSH, *VED_RSH
 */
compile_mode :pop11 +strict;

/*
VEDPIPEIN runs a unix command and stores the output in a specified
VED file, which can then be displayed if required. If only one line
of output is produced, and it will fit on the status line, then it
is shown on the status line, if the global variable show_output_on_status
is true (the default).


vedpipein(<string:C>,<list:A>,<string|vector|false:F>,<proc:P>,<boole:B>,
            <boole|string:H>);

    C is a command path name and A a list of argument strings, as
    required for *SYSEXECUTE and *PIPEIN. The command is run and any
    output received is put into a VED file defined by F (which can be
    either a file name or a VED file structure). P is a procedure to be
    executed in the environment of the VED file as soon as it is opened,
    e.g. to set defaults. If B is true the output from the command is
    displayed as a VED window on the screen, or, if there is enough
    space and show_output_on_status is non-false, on the status line. If
    H is a string then if an output file is created, the string is
    inserted as a header.

    If F is false, read the output into the current VED buffer, after
    the cursor line.
*/

section;

vars show_output_on_status;

unless isboolean(show_output_on_status) then
    true -> show_output_on_status
endunless;


define vedpipein(command, arglist, file, defaults, display, header);
    lvars   device, command, arglist, file, defaults, display, header, empty,
            stacklen, size;
    lconstant
            mess = '\{b}please wait ...';
    dlocal  show_output_on_status, vedediting, ved_current_file;

    define lconstant do_interrupt();
        dlocal vedediting = true;
        clearstack();
        if vedmessage = mess then vedputmessage('\{b}interrupted') endif;
        chain(interrupt)
    enddefine;

    define dlocal interrupt = chainfrom(%vedpipein, do_interrupt%) enddefine;

    if isstring(file) then
        vedopen(file) -> file;
    elseif file == false then
        ;;; don't start new file
    elseunless isvector(file) then
        mishap(file,1,'vedpipein: STRING OR FILE STRUCTURE NEEDED')
    endif;

    vedputmessage(mess);
    false -> vedediting;
    if file then file -> ved_current_file endif;

    if display == 1 then
        true -> display;
        false -> show_output_on_status
    endif;

    procedure;
        dlocal ved_on_status = false;
        lconstant no_mess = '\{b}no output';
        if file then defaults() endif;

        ;;; run the command, insert output in ved buffer
        pipein(command, arglist, false) -> device;
        stacklength() -> stacklen;
        if header then header endif;    ;;; header line

        ;;; read in all the output from the sub-process
        unless vedreadin(device) then
            ;;; failed for some reason, e.g. interrupted
            interrupt();
        endunless;

        if file then
            ;;; create new file or display on command line
            repeat vedwindowlength times nullstring endrepeat;
            stacklength() - stacklen -> vvedbuffersize;
            consvector(vvedbuffersize) -> vedbuffer;
            vedusedsize(vedbuffer) -> vvedbuffersize;
            vedjumpto(if header then 2 else 1 endif, 1);
            if vvedbuffersize < vedline then
                no_mess
            elseif vvedbuffersize == vedline
            and show_output_on_status and vvedlinesize + 15 < vedscreenwidth
            and not(wved_window_of_file(file))
            then
                vedthisline()
            else
                false
            endif -> empty;
        else
            ;;; insert in the file
            stacklength() - stacklen -> size;
            if size /== 0 then
                vednextline();
                fast_repeat size times
                    vedlineabove();
                    -> vedthisline();
                endrepeat;
                false;
            else
                no_mess
            endif -> empty;
            display -> vedediting;
            vedrefreshrange(vedline, vvedbuffersize, undef);
        endif
    endprocedure();

    define lconstant do_display(file, empty);
        lvars file, empty;
        dlocal vedediting = true;
        if empty then
            ;;; no output from command, or one liner
            vedputmessage(empty);
            vedrefreshstatus();
        elseif file then
            ;;; ensure screen is refreshed
            if mess = vedmessage then vedputmessage('\{b}done') endif;
            vedsetonscreen(file, procedure(); ->; true, false endprocedure);
        endif
    enddefine;

    if display then chain(file, empty, do_display) endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May  8 1996
        Fixed BR davidy.89 by changing 2nd arg to vedsetonscreen
        (in do_display),
--- John Gibson, Apr 21 1994
        Removed sys*wait for p*ipein_child_pid -- no longer necesssary.
--- John Williams, Nov 17 1992
        Argument display can now be 1, meaning display output, but never
        on status line (i.e. assigns false to show_output_on_status).
--- James Goodlet, Jan 15 1992
        Now uses -p*ipein_child_pid- in -sys*wait-
        loop to ensure that it waits for the correct child.  Otherwise,
        severe synchronisation problems could occur.
--- John Gibson, Jun 22 1991
        Rewritten.
--- Aaron Sloman, Jun  9 1991
    Made vedediting dlocal, and updated it properly
--- Rob Duncan, Jun  6 1990
    Fixed to display single-line output even when -header- is false
--- Aaron Sloman, Aug 11 1989
    Changed to avoid spurious blank line if -file- is false
--- Aaron Sloman, Aug  1 1989
    Changed so that if -file- is false, then the output is inserted
    into the current file.
--- Aaron Sloman, Jan 14 1989 removed vednullstring
--- Aaron Sloman, Dec 12 1988 Put in call of sys*wait to avoid zombies

 */
