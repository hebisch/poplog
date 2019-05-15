/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/commands.p
 > Purpose:         Prolog: top-level commands
 > Author:          Rob Duncan & Simon Nichols, Jul  5 1990 (see revisions)
 */

section prolog;

constant
    procedure ( Read_cmdname, Read_cmdarg, prolog_handlepopescape, );

;;; ========================================================================

vars procedure (
    prolog_read_cmdname = Read_cmdname,
    prolog_read_cmdarg = Read_cmdarg,
);


;;; do_vedcmd:
;;;     execute a Prolog VED command.

define lconstant do_vedcmd(arg, ved_cmd) with_props false;
    lvars   arg, ved_cmd;
    dlocal  proglist_state = proglist_new_state([^arg]);
    popvedcommand(ved_cmd);
enddefine;


;;; cd_cmd:
;;;     change directory.

define lconstant cd_cmd(arg);
    lvars arg;
    arg -> current_directory;
enddefine;


;;; pwd_cmd:
;;;     print current directory.

define lconstant pwd_cmd();
    dlocal pop_pr_quotes = false;
    printf(current_directory, '%P\n');
enddefine;


;;; pop11_cmd:
;;;     switch subsystem to POP-11.

define lconstant pop11_cmd();
    "pop11" -> sys_compiler_subsystem(`c`);
enddefine;


;;; prolog_command:
;;;     maps command names to command procedures.

define prolog_command =
    newproperty([
        [% "ved",       do_vedcmd(% "ved_ved" %)    %]
        [% "help",      do_vedcmd(% "ved_help" %)   %]
        [% "teach",     do_vedcmd(% "ved_teach" %)  %]
        [% "ref",       do_vedcmd(% "ved_ref" %)    %]
        [% "showlib",   do_vedcmd(% "ved_showlib" %)%]
        [% "im",        do_vedcmd(% "ved_im" %)     %]
        [% "source",    do_vedcmd(% "ved_source" %) %]
        [% "cd",        cd_cmd                      %]
        [% "pwd",       pwd_cmd                     %]
        [% "pop11",     pop11_cmd                   %]
        [% "'\^['",     prolog_handlepopescape      %]
        [% "bye",       sysexit                     %]
    ], 16, false, true);
enddefine;


;;; prolog_read_command:
;;;     reads a command, returning <false> if unsuccessful.

define vars prolog_read_command();
    lvars  item, cmd;
    returnunless(prolog_read_cmdname() ->> item)(false);
    if prolog_command(item) ->> cmd then
        if pdnargs(cmd) == 1 then
            cmd(% prolog_read_cmdarg() %);
        else
            cmd;
        endif;
    else
        item -> prolog_read_cmdname();
        false;
    endif;
enddefine;


;;; prolog_abolish_command:
;;;     deletes a command from the command table.

define prolog_abolish_command(cmd);
    lvars cmd;
    false -> prolog_command(cmd);
enddefine;

endsection;

PROLOG

:- module prolog.

prolog_abolish_command(Cmd) :-
    prolog_eval(prolog_abolish_command(quote(Cmd))).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  4 1993
        Added source command
--- Robert John Duncan, Jul 15 1993
        Moved in definition of prolog_abolish_command/1
--- John Gibson, Jan 18 1993
        Replaced use of s*witch_subsystem_to with sys_compiler_subsystem(`c`)
--- Robert John Duncan, May 18 1992
        Changed to use -popvedcommand-
--- Simon Nichols, May 28 1991
        Added bye command for quitting Prolog.
--- Robert John Duncan, Jan 24 1991
        Added ESC command for the PWM.
--- John Gibson, Oct 31 1990
        Changed -do_vedcmd- to take only name of pop command, since things
        like -help- now work like -ved-, i.e. assign to -vedargument directly;
        also, test for vedediting no longer necessary.
        changed -prolog_command- accordingly.
--- Simon Nichols, Oct 26 1990
        Stopped -pwd_cmd- from printing spaces before the path.
--- Simon Nichols, Sep 12 1990
        Added -prolog_abolish_command-.
 */
