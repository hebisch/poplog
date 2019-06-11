/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/ved/veddo.p
 >  Purpose:        acts as if string had been typed in on command line
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * VEDDO
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define lconstant Last_command();
    dlocal ved_on_status = true;
    vedtrimline();
    vedthisline();
enddefine;

define veddo(command);
    lvars command, put_on_status = false;
    dlocal vedcommand, vedlastcommand;
    lconstant comm_ref = consref(0);

    if command.isboolean then (), command -> (command, put_on_status) endif;

    nullstring sys_>< command -> command;

    if put_on_status then
        if command /= Last_command() then vedputcommand(command) endif;
        command
    else
        ;;; Putting the command in a ref tells veddocommand not to muck around
        ;;; with the status line etc
        command -> fast_cont(comm_ref);
        comm_ref
    endif -> vedcommand;

    veddocommand()
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jan 28 1995
        Changed to put command in a ref when put_on_status is false
--- Jonathan Meyer, Sep 12 1991
        Stopped it from putting the same command on the command line twice
--- Jonathan Meyer, Jul  3 1991
        Converted to use compile mode strict (changed vars to dlocal).
        Allowed optional extra argument stating whether the new command
        should be inserted on the status.
        Changed use of -space- to -nullstring-.
        Made it be a vars procedure by default.
--- Aaron Sloman, Nov 11 1990 replaced >< with sys_><, and declared vedcommand

--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
