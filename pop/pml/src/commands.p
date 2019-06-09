/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/commands.p
 > Purpose:         PML: Command table
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

vars
    ml_allow_commands = true,
        ;;; <false> disables all commands
;

;;; Basic commands:

define lconstant bye_cmd(name, args);
    lvars   name, args;
    dlocal  pop_pr_exception = ml_pr_exception;
    sysexit();
enddefine;

define lconstant pwd_cmd(name, args);
    lvars   name, args;
    dlocal  pop_pr_exception = ml_pr_exception;
    printf(current_directory, '\s\s%S\n');
enddefine;

define lconstant cd_cmd(name, args);
    lvars   name, args;
    dlocal  pop_pr_exception = ml_pr_exception;
    if args == [] then nullstring else hd(args) endif -> current_directory;
enddefine;

define lconstant pop11_cmd(name, args);
    lvars   name, args;
    dlocal  pop_pr_exception = ml_pr_exception;
    "pop11" -> sys_compiler_subsystem(`c`);
enddefine;

define lconstant ved_cmd(name, args);
    lvars   name, args;
    dlocal  pop_pr_exception = ml_pr_exception, proglist = args,
            subsystem = "ml";
    valof(consword(name))();
enddefine;

;;; Command table:

define command_table =
    newproperty([
        [bye        ^bye_cmd]
        [pwd        ^pwd_cmd]
        [cd         ^cd_cmd]
        [pop11      ^pop11_cmd]
        [ved        ^ved_cmd]
        [help       ^ved_cmd]
        [teach      ^ved_cmd]
        [im         ^ved_cmd]
        [showlib    ^ved_cmd]
        [trace      ^trace_command]
        [untrace    ^trace_command]
    ], 16, false, "perm");
enddefine;


;;; Read and execute top-level commands:

define do_commands(itemrep) with_props false;
    lvars itemrep, item, cmd;
    repeat
        while (itemrep() ->> item) == RESERVED ';' do endwhile;
        if item == termin then
            return(termin);
        elseif ml_allow_commands
        and isshortid(item)
        and (command_table(item) ->> cmd)
        then
            cmd(word_string(item), lex_command_args(itemrep));
        else
            item -> itemrep();
            return(true);
        endif;
    endrepeat;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new exception printing
--- Robert John Duncan, Dec 20 1994
        Added trace and untrace
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 24 1994
        Static error procedure renamed to ml_prm*ishap
--- John Gibson, Jan 18 1993
        Replaced use of s*witch_subsystem_to with sys_compiler_subsystem(`c`)
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem in ved_cmd
--- Robert John Duncan, Nov  1 1991
        Renamed warning and error procedures.
--- Robert John Duncan, Mar 18 1991
        Added "bye" command in response to ISL FR 4333
--- Robert John Duncan, Mar  1 1991
        Uniform interface for all command procedures:
        each takes the command name and a list of argument strings
        read by -lex_command_args-. New variable -ml_allow_commands-
        means that command recognition can be disabled all together.
        The "load" command is now defined in "compile.p".
--- Robert John Duncan, Oct 31 1990
        Simplified -do_vedcmd- now that the POP-11 "help", "im" etc. are
        better behaved
--- Rob Duncan, Feb 23 1990
        Added dlocal of -prm*ishap- in -do_vedcmd- (for unreadable files
        etc.)
--- Rob Duncan, Jan 31 1990
        Added "showlib" command: needed a slight change to the -do_vedcmd-
        procedure
--- Rob Duncan, Oct 26 1989
        Changed load command to use -sourcefile-
 */
